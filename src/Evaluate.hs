-- | Evaluates a Lisp tree. This module contains the actual 'evaluate' function,
--   and the primitive expressions of chapter 4 of the R5RS report.
module Evaluate (
      evaluate,
      Mutable.newEnv,
) where

import LispLanguage
import LispError

import qualified Evaluate.Mutable as Mutable
import qualified Evaluate.Standard as Standard

import Control.Monad.Error


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
evaluate e (List (Atom f        : xs )) = mapM (evaluate e) xs >>= liftThrows . Standard.apply f
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
lambda envR [args, body] = undefined
lambda _ xs = throwError $ NumArgs 2 (length xs) "lambda"
-- Lambda [LispValue] (Maybe [LispValue]) LispValue Env
-- TODO: Error message type for "expected: >= n args"


##############
## CONTINUE ##
##############
apply' :: LispValue -> [LispValue] -> ThrowsErrorIO LispValue
apply' (PrimitiveF f) args = f args
apply' (Lambda params varargs body closure) args
      -- TODO: Error handling
      -- | length params /= length args = throwError $ General "Wrong number of args for Lambda"
      | otherwise = do
            (liftIO $ Mutable.setScopeVars closure (zip params args))
            >>=
            setScopeVarargs varargs
            >>=
            evalBody body

      where evalBody (b:bs) env = foldM f b bs
                  where f _acc expr = evaluate env expr
            evalBody [] _ = undefined-- throwError "Shouldn't happen! This is a bug and I'd like to hear about it."
            setScopeVarargs args env = maybe
                  (return env)
                  (\argName -> liftIO $ Mutable.setScopeVars env [(argName, List remainingArgs)])
                  varargs
            remainingArgs = drop (length params) args