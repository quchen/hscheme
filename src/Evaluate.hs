{-# LANGUAGE TupleSections #-}

-- | Evaluates a Lisp tree. This module contains the actual 'evaluate' function,
--   and the primitive expressions of chapter 4 of the R5RS report.
module Evaluate (
      evaluate,
      Mutable.newEnv,
) where

import LispLanguage
import LispError

import qualified Evaluate.Mutable as Mutable

import Control.Monad.Error
import Data.Map (singleton, fromList)
import Data.List
import Data.Maybe


-- TODO: evaluate equal?
-- TODO: evaluate cond, case -> http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.1


-- | Evaluates a Lisp tree.
evaluate :: EnvR -> LispValue -> ThrowsErrorIO LispValue
evaluate _ x@(Bool _)                   = return x
evaluate _ x@(Number _)                 = return x
evaluate _ x@(String _)                 = return x
evaluate e (Atom x)                     = Mutable.readVar e x
evaluate _ (List (Atom "quote"  : xs )) = quote     xs
evaluate e (List (Atom "if"     : xs )) = ifLisp  e xs
evaluate e (List (Atom "set!"   : xs )) = set     e xs
evaluate e (List (Atom "define" : xs )) = define  e xs
evaluate e (List (Atom "begin"  : xs )) = begin   e xs
evaluate e (List (Atom "lambda" : xs )) = lambda  e xs
evaluate e (List (Atom "let"    : xs )) = letLisp e xs True
evaluate e (List (Atom "let*"   : xs )) = letLisp e xs False
evaluate e (List (Atom "cond"   : xs )) = cond    e xs
evaluate e (List (f             : xs )) = do evalF <- evaluate e f
                                             args <- mapM (evaluate e) xs
                                             apply evalF args
evaluate _ unknown                      = throwError . BadExpr $ show unknown


-- | Updates an already existing variable
set :: EnvR -> [LispValue] -> ThrowsErrorIO LispValue
set    env [Atom var, value] = evaluate env value >>= Mutable.setVar env var
set    _   [_       , _    ] = throwError $ BadArg "Expected atom"
set    _   args              = throwError $ NumArgs EQ 2 (length args) "set!"

-- | Defines or updates a variable. If a list starting with an atom is given,
--   defines a Lambda.
--   (define x 2) ==> defines x = 2
--   (define (f x) (* 2 x)) ==> defines f(x) = 2*x
define :: EnvR -> [LispValue] -> ThrowsErrorIO LispValue
define envR [Atom var, value] = evaluate envR value >>= Mutable.defineVar envR var
define envR [List  (Atom f:params)    , body] = lambda envR [List  params    , body] >>= Mutable.defineVar envR f
define envR [List' (Atom f:params) dot, body] = lambda envR [List' params dot, body] >>= Mutable.defineVar envR f
define _   [_       , _    ] = throwError $ BadArg "Expected atom"
define _   args              = throwError $ NumArgs EQ 2 (length args) "define"

-- | If statement. Only evaluates the branch it needs to.
ifLisp :: EnvR -> [LispValue] -> ThrowsErrorIO LispValue
ifLisp envR [ p, trueBranch, falseBranch ] = do
      p' <- evaluate envR p
      evaluate envR $ if isTrue p' then trueBranch
                                   else falseBranch
ifLisp _ args = throwError $ NumArgs EQ 3 (length args) "if"

-- | Checks whether an expression is to be considered True or False.
isTrue :: LispValue -> Bool
isTrue (Bool False) = False
isTrue _            = True

-- | Quoted statement are returned unevaluated to the parse tree.
quote :: [LispValue] -> ThrowsErrorIO LispValue
quote [expr] = return expr
quote args   = throwError $ NumArgs EQ 1 (length args) "quote"

-- | Sequencing. Evaluates the arguments in order, and returns the last result.
begin :: EnvR -> [LispValue] -> ThrowsErrorIO LispValue
begin _    []     = throwError $ NumArgs GT 0 0 "begin"
begin envR (x:xs) = evaluate envR x >>= if null xs then return
                                                   else const (begin envR xs)

-- | Lambda handling
lambda :: EnvR -> [LispValue] -> ThrowsErrorIO LispValue
-- (lambda (x y) body)
lambda envR [List params, body] = do
      paramNames <- mapM (liftThrows . unAtom) params
      lambdaCheckMultiBindings paramNames
      return $ Lambda paramNames Nothing body envR
-- (lambda (x y . dot) body)
lambda envR [List' params vararg, body] = do
      paramNames <- mapM (liftThrows . unAtom) params
      lambdaCheckMultiBindings paramNames
      varargName <- (liftThrows . unAtom) vararg
      return $ Lambda paramNames (Just varargName) body envR
-- (lambda x body)
lambda envR [Atom varargName, body] =
      return $ Lambda [] (Just varargName) body envR
lambda _ xs = throwError $ NumArgs EQ 2 (length xs) "lambda"

-- | Helper function for Lambdas. Throws an error if an argument is bound more
--   than once.
lambdaCheckMultiBindings :: [String] -> ThrowsErrorIO ()
lambdaCheckMultiBindings bindings = do
      let maxCount = maximum . map length . group . sort $ bindings
      when (maxCount > 1) $
            throwError $ BadArg "Every variable can occur at most once in a lambda binding"

unAtom :: LispValue -> ThrowsError String
unAtom (Atom a) = return a
unAtom _        = throwError $ BadArg "Expected atom"

-- | Let binding. Depending on the boolean argument, duplicate bindings produce
--   an error (let) or not (let*).
letLisp :: EnvR -> [LispValue] -> Bool -> ThrowsErrorIO LispValue
letLisp envR [List bindings, body] errorOnDuplicate = do
      let -- Converts Lisp's (x val) to Haskell's (x, val)
          toTuple (List [Atom var, value]) = fmap (var,) $ evaluate envR value
          toTuple (List (Atom var : xs  )) = throwError $ NumArgs EQ 1 (length xs) ("let/" ++ var)
          toTuple (List (_notAtom : _   )) = throwError $ BadArg "Expected atom"
          toTuple _                        = throwError $ BadArg "Expected list of let bindings"
      bindingTuples <- mapM toTuple bindings
      let maxCount = maximum (map length . group . sort . map fst $ bindingTuples)
      when (errorOnDuplicate && maxCount > 1) $
            throwError $ BadArg "Every variable can occur at most once in a let binding"
      scopedEnvR <- liftIO $ Mutable.setScopeVars (fromList bindingTuples) envR
      evaluate scopedEnvR body
letLisp _ _ _ = throwError $ BadArg "Expected let binding list"





apply :: LispValue -> [LispValue] -> ThrowsErrorIO LispValue
apply (PrimitiveF f) args = liftThrows $ f args
apply (Lambda params vararg body envR) args
      -- | TODO error handling
      | False = undefined -- To mute HLint
      | otherwise = do
            checkNumArgs
            lambdaEnv >>= evalBody

      where checkNumArgs = do
                  let lowerBound = length params + maybe 0 length vararg
                      upperBound = maybe (Just lowerBound) (const Nothing) vararg
                      numArgs = length args

                      throwNeedMore = throwError $
                            NumArgs GT (lowerBound - 1) numArgs "lambda"
                      throwNeedLess = throwError $
                            NumArgs LT (fromJust upperBound + 1) numArgs "lambda"
                  when (numArgs < lowerBound) throwNeedMore
                  when (maybe False (numArgs >) upperBound) throwNeedLess


            -- Environment made out of the scoped environment plus the variables
            --  bound by the lambda.
            lambdaEnv :: ThrowsErrorIO EnvR
            lambdaEnv =
                  -- Set the lambda's arguments in the stored environment
                  liftIO (Mutable.setScopeVars (fromList $ zip params args) envR)
                  >>=
                  -- Same thing for the vararg
                  setScopeVararg

            -- If vararg is present, set them in the environment env
            setScopeVararg env = maybe
                  (return env)
                  (\argName -> liftIO $ Mutable.setScopeVars
                                              (singleton argName $ List remainingArgs)
                                              env)
                  vararg

            -- Evaluates the Lambda's body using a specified environment
            evalBody :: EnvR -> ThrowsErrorIO LispValue
            evalBody env = evaluate env body

            -- All the arguments not covered by the parameters
            remainingArgs = drop (length params) args
apply x xs = throwError . BadExpr . show . List $ (x:xs)
-- TODO: ^ gives a bad error message sometimes. For example, in
--       ((lambda (x) (x)) 3)
--       the 3 is inserted first, and then (3) triggers the above error.
--       This behavior isn't wrong, but the error message could be better.


-- | Conditional.
cond :: EnvR -> [LispValue] -> ThrowsErrorIO LispValue
cond _    (List [] :_) = throwError $ BadArg "Expected predicate"
cond envR (List [p] : rest) = do pEval <- evaluate envR p
                                 if isTrue pEval then return p
                                                 else cond envR rest
cond envR (List [p, Atom "=>", expr] : rest) = do
      pEval <- evaluate envR p
      if isTrue pEval then do exprEval <- evaluate envR expr
                              apply exprEval [pEval]
                      else cond envR rest
cond _ (List (_ : Atom "=>" : _ : xs) : _) =
      throwError $ NumArgs EQ 1 (length xs) "=> X"
cond envR (List (p:exprs):rest) = do
      pEval <- case p of Atom "else" -> return $ Bool True
                         _otherwise  -> evaluate envR p
      if isTrue pEval then begin envR exprs
                      else cond envR rest
cond _ (_:_) = throwError $ BadArg "Expecting list as cond arguments"
cond _ _     = throwError $ BadArg "Unsatisfied cond"

-- TODO: case