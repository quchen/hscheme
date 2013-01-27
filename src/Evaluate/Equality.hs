-- | *Generic* functions for equality checks.
--   (More specific functions, such as 'string=?', are in the respective
--   modules)
module Evaluate.Equality (
      eq,
      eqv
) where

import LispLanguage
import LispError

import Control.Monad.Error

-- | Equality check
eq :: [LispValue] -> ThrowsError LispValue
eq [x, y] = return . Bool $ x == y
eq args   = throwError $ NumArgs 2 (length args) "eq"

-- | Equality check again. Identical to 'eq'.
-- TODO: Implement some differences? Behavior is correct but redundant right now
eqv :: [LispValue] -> ThrowsError LispValue
eqv [x,y] = eq [x,y]
eqv args  = throwError $ NumArgs 2 (length args) "eqv"