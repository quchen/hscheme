-- | Evaluates a Lisp tree. This module contains the actual 'evaluate' function,
--   and all the primitives that could not
module Evaluate (
      evaluate,
      Mutable.newEnv,
      Mutable.EnvR
) where

import LispLanguage
import LispError

import           Evaluate.Mutable (EnvR)
import qualified Evaluate.Mutable as Mutable
import qualified Evaluate.Primitive as Primitive

import Control.Monad.Error


-- TODO: evaluate equal?
-- TODO: evaluate cond, case -> http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.1


-- | Evaluates a Lisp tree.
evaluate :: EnvR -> LispValue -> ThrowsErrorIO LispValue
evaluate _ x@(Bool _)                  = return x
evaluate _ x@(Number _)                = return x
evaluate _ x@(String _)                = return x
evaluate e (Atom x)                    = Mutable.readVar e x
evaluate _ (List (Atom "quote" : xs )) = quote xs
evaluate e (List (Atom "if"    : xs )) = ifLisp e xs
evaluate e (List (Atom "set!"  : xs )) = set e xs
evaluate e (List (Atom "define": xs )) = define e xs
evaluate e (List (Atom "begin" : xs )) = begin e xs
evaluate e (List (Atom f       :args)) = mapM (evaluate e) args >>= liftThrows . Primitive.apply f
evaluate _ unknown                     = throwError . BadExpr $ show unknown


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
-- TODO: Error message type for "expected: >= 2 args"