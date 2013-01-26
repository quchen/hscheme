module Parser (
      parseLisp
) where

import LispLanguage

import Control.Applicative
import Data.Either
import Data.Monoid
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String

-- TODO: Add quasiquotation support -> http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.6


-- TODO write Read instance for LispVal
-- see http://stackoverflow.com/q/14523728/1106679
-- instance Read LispValue where
--       readsPrec = either undefined undefined parseResult
--             where parseResult = parse undefined



spacesP :: Parser String
spacesP = many space

symbolP :: Parser Char
symbolP = oneOf "!$%&|*+-/:<=>?@^_~"


-- Lisp value parsers

-- Atom: (letter|symbol)(letter|symbol|digit)*
-- Parses an Atom, unless it's prefixed with "#" (for Bool) or "-" (for negative
-- numbers)
miscP :: Parser LispValue
miscP = do
      x <- letter <|> symbolP
      case x of '-' -> numberP negate <|> return (Atom [x])
                _   -> Atom . (x:) <$> (many $ letter <|> symbolP <|> digit)

boolP :: Parser LispValue
boolP = Bool . toBool <$> (char '#' *> (oneOf "tf" <?> hint))
      where toBool b = b == 't'
            hint = "t/f after #"

listP = do
      char '('
      xs <- expressionP `endBy` spacesP
      dot <- optionMaybe $ char '.' *> spacesP *> expressionP <* spacesP
      char ')'
      return $ maybe (List xs) (List' xs) dot


-- | Parses a nonnegative Integer.
--   If there's a "-" sign it'll be parsed by atomP first, and passed as
--   'negate' parameter to this function.
numberP :: (Integer -> Integer) -> Parser LispValue
numberP modifier = Number . modifier . read <$> many1 digit
-- TODO: Make parser fail if number's bad (instead of letting read ramble). Presumably, "fail" of Parsec is just right for this.
-- TODO: More flexible number parsing
--       http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4

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

expressionP :: Parser LispValue
expressionP =     boolP
              <|> miscP
              <|> numberP id
              <|> stringP
              <|> listP

parseLisp :: String -> Either ParseError LispValue
parseLisp = parse expressionP "Lisp code parser"