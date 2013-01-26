module LispError (
      LispError(..)
) where

import LispLanguage
import Control.Monad.Error
import Text.Printf

data LispError = Generic String            -- ^ If you don't know better
               | BadExpr String      -- ^ Evaluating stuff 'evaluate' doesn't support
               | UnknownFunc String    -- ^ Function unknown
               | NumArgs Integer Integer String -- ^ Wrong number of arguments. Format: expected, given, name
               | BadArg String             -- ^ Bad argument, e.g. (- "1")

instance Show LispError where
      show x = printf "### Error: %s" (show' x)

-- | Prettyprints a single error. Wrapped by the Show instance to prepend
--   stuff.
show' :: LispError -> String
show' (Generic s)     = printf "An error occurred: '%s'" s
show' (BadExpr e)     = printf "Unrecognized (unimplemented?) expression: '%s'" e
show' (UnknownFunc f) = printf "Unknown function: '%s'" f
show' (NumArgs e g n) = printf "'%s' expects %d argument%s, %d given" n e (if e > 1 then "s" else "") g
show' (BadArg b)      = printf "Bad argument: %s" b

instance Error LispError where
      noMsg  = Generic "An error occurred"
      strMsg = Generic