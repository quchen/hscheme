module LispError (
      LispError(..)
) where

import LispLanguage
import Control.Monad.Error
import Text.Printf

data LispError =
        Generic String -- ^ If you don't know better
      | BadExpr String -- ^ Evaluating stuff 'evaluate' doesn't support
      | UnknownFunc String -- ^ Function unknown
      | NumArgs Integer Integer String -- ^ Wrong number of arguments.
                                       --   Format: expected, given, name
      | BadArg String -- ^ Bad argument, e.g. (- "1")

instance Show LispError where
      show x = printf "### Error: %s" (show' x)

-- | Prettyprints a single error. Wrapped by the Show instance to prepend
--   stuff.
show' :: LispError -> String
show' (Generic s) = printf pattern s
      where pattern = "An error occurred: '%s'"
show' (BadExpr e) = printf pattern e
      where pattern = "Unrecognized expression: '%s'"
show' (UnknownFunc f) = printf pattern f
      where pattern = "Unknown function: '%s'"
show' (NumArgs e g n) = printf pattern n e (pluralS e) g
      where pattern = "'%s' expects %d argument%s, %d given"
show' (BadArg b) = printf pattern b
      where pattern = "Bad argument: %s"

pluralS x | x > 1     = "s"
          | otherwise = ""

instance Error LispError where
      noMsg  = Generic "An error occurred"
      strMsg = Generic