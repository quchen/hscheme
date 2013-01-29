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
import Debug.Trace


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
evaluate e (List (f             : xs )) = do f' <- evaluate e f
                                             args <- mapM (evaluate e) xs
                                             apply f' args
evaluate _ unknown                      = throwError . BadExpr $ show unknown


-- | Updates an already existing variable
set :: EnvR -> [LispValue] -> ThrowsErrorIO LispValue
set    env [Atom var, value] = evaluate env value >>= Mutable.setVar env var
set    _   [_       , _    ] = throwError $ BadArg "Expected atom"
set    _   args              = throwError $ NumArgs 2 (length args) "set!"

-- | Defines or updates a variable
define :: EnvR -> [LispValue] -> ThrowsErrorIO LispValue
define env [Atom var, value] = evaluate env value >>= Mutable.defineVar env var
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
begin envR (x:xs) = foldM eval2 x xs
      where eval2 _ = evaluate envR
-- TODO: Error message type for "expected: >= n args"

lambda :: EnvR -> [LispValue] -> ThrowsErrorIO LispValue
-- Evaluate body, replacing ocurrences of x with a value.
lambda envR [params, body] = return $ Lambda params Nothing body env
-- ### TODO ### CONTINUE
lambda _ xs = throwError $ NumArgs 2 (length xs) "lambda"
               | Lambda [String] (Maybe String) [LispValue] EnvR

-- TODO: Error message type for "expected: >= n args"




apply :: LispValue -> [LispValue] -> ThrowsErrorIO LispValue
apply (PrimitiveF f) args = liftThrows $ f args
apply (Lambda params varargs body closure) args
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
                  -- Same thing for the varargs
                  setScopeVarargs varargs

            -- If varargs are present, set them in the environment env
            setScopeVarargs args env = maybe
                  (return env)
                  (\argName -> liftIO $ Mutable.setScopeVars
                                              (singleton argName $ List remainingArgs)
                                              env)
                  varargs

            -- Evaluates the Lambda's body using a specified environment
            evalBody :: EnvR -> ThrowsErrorIO LispValue
            evalBody env = fmap last . mapM (evaluate env) $ body

            -- All the arguments not covered by the parameters
            remainingArgs = drop (length params) args
apply _ _ = throwError . Generic $ "This is a bug and should not happen. Please email me about it."