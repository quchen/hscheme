module LispError where -- TODO: exports

import LispLanguage
import Control.Monad.Error

-- TODO: Custom Show instance
data LispError = Generic String            -- ^ If you don't know better
               | BadExpr String      -- ^ Evaluating stuff 'evaluate' doesn't support
               | UnknownFunc String    -- ^ Function unknown
               | NumArgs Integer           -- ^ Wrong number of arguments
               | BadArg String             -- ^ Bad argument, e.g. (- "1")
      deriving (Show)

instance Error LispError where
      noMsg  = Generic "An  occurred"
      strMsg = Generic