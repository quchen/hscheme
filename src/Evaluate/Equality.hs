-- | *Generic* functions for equality checks.
--   (More specific functions, such as 'string=?', are in the respective
--   modules)
module Evaluate.Equality (
      eqQ,
      eqvQ
) where

import LispLanguage
import LispError

import Control.Monad.Error

-- | Equality check
eqQ :: [LispValue] -> ThrowsError LispValue
eqQ [x, y] = return . Bool $ x == y
eqQ args   = throwError $ NumArgs EQ 2 (length args) "eq"

-- | Equality check again. Identical to 'eq'.
-- TODO: Implement some differences? Behavior is correct but redundant right now
eqvQ :: [LispValue] -> ThrowsError LispValue
eqvQ [x,y] = eqQ [x,y]
eqvQ args  = throwError $ NumArgs EQ 2 (length args) "eqv"

-- TODO: evaluate equal?