module LispError (
        LispError(..)
      , ThrowsError
      , ThrowsErrorIO
      , liftThrows
) where

-- TODO is this necessary? import LispLanguage () -- Import only instances
import Control.Monad.Error
import Text.Printf
import Text.Parsec (ParseError)

data LispError =
        Generic String                  -- ^ If you don't know better
      | BadExpr String                  -- ^ Evaluating stuff 'evaluate' doesn't support
      | UnknownFunc String              -- ^ Function unknown
      | NumArgs Ordering Int Int String -- ^ Wrong number of arguments.
                                        --   Format: expected, given, name.
                                        --   GT: "More than X expected"
                                        --   EQ: "Exactly X expected"
                                        --   LT: "Less than X expected"
      | BadArg String                   -- ^ Bad argument, e.g. (- "1")
      | BadParse ParseError             -- ^ Parsec complained
      | UnknownVar String               -- ^ Variable not set

instance Show LispError where
      show x = printf "### Error: %s" (show' x)

-- | Prettyprints a single error. Wrapped by the Show instance to prepend stuff.
show' :: LispError -> String
show' (Generic s) = printf pattern s
      where pattern = "%s"
show' (BadExpr e) = printf pattern e
      where pattern = "Unrecognized expression: '%s' (Did you forget a quote?)"
show' (UnknownFunc f) = printf pattern f
      where pattern = "Unknown function: '%s'"
show' (NumArgs ord e g n) = printf pattern n ord' e (pluralS e) g
      where pattern = "'%s' expects %s%d argument%s, %d given"
            ord' = case ord of LT -> "less than "
                               EQ -> ""
                               GT -> "more than "
show' (BadArg b) = printf pattern b
      where pattern = "Bad argument: %s"
show' (BadParse b) = printf pattern (show b)
      where pattern = "Parse error: %s"
show' (UnknownVar v) = printf pattern v
      where pattern = "Unknown variable '%s'"

-- | Generates a plural "s" for numbers other than 1.
pluralS :: (Num a, Ord a) => a -> String
pluralS 1 = ""
pluralS _ = "s"

instance Error LispError where
      noMsg  = Generic "An error occurred"
      strMsg = Generic

type ThrowsError = Either LispError

type ThrowsErrorIO = ErrorT LispError IO

-- | Lifts a non-IO error into the error transformer
liftThrows :: ThrowsError a -> ThrowsErrorIO a
liftThrows (Right r) = return r
liftThrows (Left  l) = throwError l