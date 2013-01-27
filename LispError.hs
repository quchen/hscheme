module LispError (
      LispError(..)
) where

import LispLanguage () -- Import only instances
import Control.Monad.Error
import Text.Printf
import Text.Parsec (ParseError)

data LispError =
        Generic String -- ^ If you don't know better
      | BadExpr String -- ^ Evaluating stuff 'evaluate' doesn't support
      | UnknownFunc String -- ^ Function unknown
      | NumArgs Int Int String -- ^ Wrong number of arguments.
                               --   Format: expected, given, name
      | BadArg String -- ^ Bad argument, e.g. (- "1")
      | BadParse ParseError -- ^ Parsec complained
      | UnknownVar String -- ^ Variable not set

instance Show LispError where
      show x = printf "### Error: %s" (show' x)

-- | Prettyprints a single error. Wrapped by the Show instance to prepend
--   stuff.
show' :: LispError -> String
show' (Generic s) = printf pattern s
      where pattern = "An error occurred: '%s'"
show' (BadExpr e) = printf pattern e
      where pattern = "Unrecognized expression: '%s' (Did you forget a quote?)"
show' (UnknownFunc f) = printf pattern f
      where pattern = "Unknown function: '%s'"
show' (NumArgs e g n) = printf pattern n e (pluralS e) g
      where pattern = "'%s' expects %d argument%s, %d given"
show' (BadArg b) = printf pattern b
      where pattern = "Bad argument: %s"
show' (BadParse b) = printf pattern (show b)
      where pattern = "Parse error: %s"

-- | Generates a plural "s" for numbers greater than 1.
pluralS :: (Num a, Ord a) => a -> String
pluralS x | x > 1     = "s"
          | otherwise = ""

instance Error LispError where
      noMsg  = Generic "An error occurred"
      strMsg = Generic