module LispLanguage (
      LispValue(..),
      prettyShow,
      debugShow
) where

import Data.List
import Text.Printf

data LispValue = Atom String
               | Bool Bool
               | List [LispValue]
               | List' [LispValue] LispValue
               | Number Integer
               | String String
               deriving (Eq)
-- TODO: Add Char -> http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.4
-- TODO: Add other numbers -> http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.1
-- TODO: Add vectors -> http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.6


instance Show LispValue where
      show = prettyShow

-- | Prettyprints Lisp code for production
prettyShow (Atom s)    = s
prettyShow (Bool b)    = show b
prettyShow (Number i)  = show i
prettyShow (String s)  = printf "\"%s\"" s
prettyShow (List l)    = encloseIn "(" ")" $ spacedShow prettyShow l
prettyShow (List' l d) = encloseIn "(" ")" $
                         spacedShow prettyShow l ++ " . "++ prettyShow d

-- | Adds types to printouts for debugging.
--   This is similar to the auto-derived instance.
debugShow (Atom s)    = printf "Atom:%s" s
debugShow (Bool b)    = printf "Bool:%s" (show b)
debugShow (Number i)  = printf "Number:%s" (show i)
debugShow (String s)  = printf "String:\"%s\"" s
debugShow (List l)    = encloseIn "(" ")" $ spacedShow debugShow l
debugShow (List' l d) = encloseIn "(" ")" $
                        spacedShow debugShow l ++ " . "++ debugShow d


-- | Shows the list's elements separated by spaces.
spacedShow :: (Show a) => (a -> String) -> [a] -> String
spacedShow showF = unwords . map showF

-- | Encloses a list in two other lists.
--   Example: encloseIn "(" ")" "hello" ==> "(hello)"
encloseIn :: [a] -> [a] -> [a] -> [a]
encloseIn open close = (open ++) . (++ close)