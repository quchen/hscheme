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
import Data.Foldable (foldrM)


-- TODO: evaluate equal?
-- TODO: evaluate cond, case -> http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.1


-- | Evaluates a Lisp tree.
evaluate :: EnvR -> LispValue -> ThrowsErrorIO LispValue
evaluate _ x@(Bool _)                   = return x
evaluate _ x@(Number _)                 = return x
evaluate _ x@(String _)                 = return x
evaluate e (Atom x)                     = Mutable.readVar e x
evaluate _ (List (Atom "quote"  : xs )) = quote xs
evaluate e (List (Atom "if"     : xs )) = ifLisp e xs
evaluate e (List (Atom "set!"   : xs )) = set    e xs
evaluate e (List (Atom "define" : xs )) = define e xs
evaluate e (List (Atom "begin"  : xs )) = begin  e xs
evaluate e (List (Atom "lambda" : xs )) = lambda e xs
evaluate e (List (f             : xs )) = do evalF <- evaluate e f
                                             args <- mapM (evaluate e) xs
                                             apply evalF args
evaluate _ unknown                      = throwError . BadExpr $ show unknown


-- | Updates an already existing variable
set :: EnvR -> [LispValue] -> ThrowsErrorIO LispValue
set    env [Atom var, value] = evaluate env value >>= Mutable.setVar env var
set    _   [_       , _    ] = throwError $ BadArg "Expected atom"
set    _   args              = throwError $ NumArgs 2 (length args) "set!"

-- | Defines or updates a variable. If a list starting with an atom is given,
--   defines a Lambda.
--   (define x 2) ==> defines x = 2
--   (define (f x) (* 2 x)) ==> defines f(x) = 2*x
define :: EnvR -> [LispValue] -> ThrowsErrorIO LispValue
define envR [Atom var, value] = evaluate envR value >>= Mutable.defineVar envR var
define envR [List  (Atom f:params)    , body] = lambda envR [List  params    , body] >>= Mutable.defineVar envR f
define envR [List' (Atom f:params) dot, body] = lambda envR [List' params dot, body] >>= Mutable.defineVar envR f
define _   [_       , _    ] = throwError $ BadArg "Expected atom"
define _   args              = throwError $ NumArgs 2 (length args) "define"

-- | If statement. Only evaluates the branch it needs to.
ifLisp :: EnvR -> [LispValue] -> ThrowsErrorIO LispValue
ifLisp envR [ Bool False, _     , ifFalse ] = evaluate envR ifFalse
ifLisp envR [ _         , ifTrue, _       ] = evaluate envR ifTrue
ifLisp _ args = throwError $ NumArgs 3 (length args) "if"

-- | Quoted statement are returned unevaluated to the parse tree.
quote :: [LispValue] -> ThrowsErrorIO LispValue
quote [expr] = return expr
quote args   = throwError $ NumArgs 1 (length args) "quote"

-- | Sequencing. Evaluates the arguments in order, and returns the last result.
begin :: EnvR -> [LispValue] -> ThrowsErrorIO LispValue
begin _    []     = throwError $ NumArgs 1 0 "begin"
begin envR (x:xs) = fmap last . mapM (evaluate envR) $ (x:xs)
-- TODO: Error message type for "expected: >= n args"

-- | Lambda handling
lambda :: EnvR -> [LispValue] -> ThrowsErrorIO LispValue
-- (lambda (x y) body)
lambda envR [List params, body] = do
      paramNames <- mapM (liftThrows . unAtom) params
      return $ Lambda paramNames Nothing body envR
-- (lambda (x y . dot) body)
lambda envR [List' params vararg, body] = do
      paramNames <- mapM (liftThrows . unAtom) params
      varargName <- (liftThrows . unAtom) vararg
      return $ Lambda paramNames (Just varargName) body envR
-- (lambda x body)
lambda envR [Atom varargName, body] = do
      return $ Lambda [] (Just varargName) body envR
lambda envR lambdaArgs@(_:_) = throwError $ BadArg "Lambdy body must be list"

lambda _ xs = throwError $ NumArgs 2 (length xs) "lambda"
-- TODO: Error when a variable is used multiple times as Lambda parameter

unAtom :: LispValue -> ThrowsError String
unAtom (Atom a) = return a
unAtom _        = throwError $ BadArg "Expected atom"


-- | Lambda [String] (Maybe String) [LispValue] EnvR

-- TODO: Error message type for "expected: >= n args"




apply :: LispValue -> [LispValue] -> ThrowsErrorIO LispValue
apply (PrimitiveF f) args = liftThrows $ f args
apply (Lambda params vararg body closure) args
      -- | TODO error handling
      | otherwise = lambdaEnv >>= evalBody

      where

            -- Environment made out of the closure plus the variables bound by
            -- the lambda.
            lambdaEnv :: ThrowsErrorIO EnvR
            lambdaEnv = do
                  -- Set the lambda's arguments in the stored environment
                  (liftIO $ Mutable.setScopeVars (fromList $ zip params args) closure)
                  >>=
                  -- Same thing for the vararg
                  setScopeVararg vararg

            -- If vararg is present, set them in the environment env
            setScopeVararg args env = maybe
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
