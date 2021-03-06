module Parser (
      parseLisp
) where

import LispLanguage

import Control.Applicative
import Control.Monad
import LispError
import Text.Parsec as P hiding ((<|>), many)
import Text.Parsec.String

-- TODO: Add quasiquotation support -> http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.6

-- TODO write Read instance for LispVal
-- see http://stackoverflow.com/q/14523728/1106679



-- | Parses (and discards) consecutive whitespaces.
whitespaceP :: Parser ()
whitespaceP = void $ many space

-- | Parses a Lisp comment, i.e. an expression ranging from ';' to newline or
--   end of input.
commentP :: Parser ()
commentP = void $ char ';' >> manyTill anyChar (eol <|> eof)

-- | Parses (and discards) one Unix/Windows/OSX line end.
eol :: Parser ()
eol = void (char '\n')
      <|>
      char '\r' *> P.optional (char '\n')
      <?> "\"\\n\", \"\\r\" or \"\\r\\n\""

-- | Discards any combination of comments and whitespaces.
ignoreP :: Parser ()
ignoreP = whitespaceP <* sepEndBy commentP whitespaceP

-- | Allowed Lisp symbols.
symbolP :: Parser Char
symbolP = oneOf "!$%&|*+-/:<=>?@^_~"


-- | Parses an Atom, unless it's prefixed with "#" (for Bool) or "-" (for
--   negative numbers)
miscP :: Parser LispValue
miscP = do
      x <- letter <|> symbolP
      case x of '-' -> numberP True <|> return (Atom [x])
                            -- True -> add "-" to the parsed number
                _   -> Atom . (x:) <$> many (letter <|> symbolP <|> digit)

-- | Parses "#f", "#t" to Booleans.
boolP :: Parser LispValue
boolP = Bool . toBool <$> (char '#' *> (oneOf "tf" <?> hint))
      where toBool b = b == 't'
            hint = "t/f after #"

-- | Parses lists and dotted lists.
listP :: Parser LispValue
listP = do
      void $ char '('
      xs <- many expressionP
      dot <- optionMaybe $ char '.' *> expressionP
      void $ char ')'
      return $ maybe (List xs) (List' xs) dot


-- | Parses a nonnegative Integer.
--   If the parameter is set to True by an external call because a "-" was
--   called in advance, negate the number.
numberP :: Bool -> Parser LispValue
numberP neg = Number . negateIf . read <$> many1 digit
      where negateIf | neg       = (0 -)
                     | otherwise = id

-- TODO: Make parser fail if number's bad (instead of letting read ramble). Presumably, "fail" of Parsec is just right for this.
-- TODO: More flexible number parsing
--       http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4

-- | Parses a string, i.e. an arbitrary character sequence delimited by '"'.
--   "\" will escape the next character, giving it special meaning if it has
--   one. Current special chars are \" \n \r \t.
stringP :: Parser LispValue
stringP = String <$> (quote *> many nonEscQuotes <* quote)
      where quote = char '\"'
            bslash = char '\\'
            nonEscQuotes = escaped <|> noneOf "\""
            escaped = toSpecial <$> (bslash *> anyChar)

            -- Used to convert characters after backslash to
            -- special characters if applicable.
            toSpecial 'n'  = '\n'
            toSpecial 'r'  = '\r'
            toSpecial 't'  = '\t'
            toSpecial x    = x

-- | Parses a quoted datum, e.g. '(+ 1 2) evaluates to (+ 1 2)
quotedP :: Parser LispValue
quotedP = do
      void $ char '\''
      expr <- expressionP
      return $ List [Atom "quote", expr]

-- | Parses a whole expression.
expressionP :: Parser LispValue
expressionP = between ignoreP ignoreP $
                  boolP
              <|> miscP
              <|> numberP False
              <|> stringP
              <|> listP
              <|> quotedP


-- | Parses a whole Lisp expression
lispP :: Parser LispValue
lispP = ignoreP *> expressionP <* ignoreP <* eof

-- | Takes Lisp code as a String, and returns the parse tree.
--   This is the API function of this module.
parseLisp :: String -> Either LispError LispValue
parseLisp = toLispError . parse lispP "Lisp code parser"
      where -- Convert Parsec error to Lisp error
            toLispError = mapLeft BadParse
            mapLeft f (Left  l) = Left (f l)
            mapLeft _ (Right r) = Right r

-- NOTE: Current position in Parsec can be obtained using sourceLine <$> getPosition