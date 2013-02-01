module LispLanguage (
      LispValue(..),
      prettyShow,
      debugShow,
      Env,
      EnvR
) where

import LispError (ThrowsError)

import Text.Printf
import Data.Map (Map)
import Data.IORef


-- | Variable database. Variable values are mutable, but variable definitions
--   are not. Use 'EnvR' for that.
type Env = Map String (IORef LispValue)

-- | Pointer to the variable database (R = Reference). Same as 'Env', but in an
--   'EnvR' variables can be created. (Note that there's no Scheme function for
--   deleting variables, although this interface would permit making one.)
type EnvR = IORef Env


data LispValue = Atom String
               | Bool Bool
               | List [LispValue]
               | List' [LispValue] LispValue
               | Number Integer
               | String String
               | PrimitiveF ([LispValue] -> ThrowsError LispValue)
               | Lambda [String] (Maybe String) LispValue EnvR
-- TODO: Add Char -> http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.4
-- TODO: Add other numbers -> http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.1
-- TODO: Add vectors -> http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.6

instance Eq LispValue where
      (Atom a) == (Atom b) = a == b
      (Bool a) == (Bool b) = a == b
      (List a) == (List b) = a == b
      (List' a aDot) == (List' b bDot) = (a, aDot) == (b, bDot)
      (Number a) == (Number b) = a == b
      (String a) == (String b) = a == b
      (PrimitiveF _) == (PrimitiveF _) = False
      (Lambda a1 a2 a3 a4) == (Lambda b1 b2 b3 b4) = (a1, a2, a3, a4) == (b1, b2, b3, b4)
      _ == _ = False


instance Show LispValue where
      show = prettyShow

-- | Prettyprints Lisp code for production
prettyShow :: LispValue -> String
prettyShow (Atom s)    = s
prettyShow (Bool b)    = if b then "#t" else "#f"
prettyShow (Number i)  = show i
prettyShow (String s)  = printf "\"%s\"" s
prettyShow (List [Atom "quote", x]) = '\'' : show x
prettyShow (List l)    = printf "(%s)" $ spacedShow prettyShow l
prettyShow (List' l d) = printf "(%s)" $
                         spacedShow prettyShow l ++ " . "++ prettyShow d
prettyShow (PrimitiveF _) = "<primitive>"
prettyShow (Lambda args vararg _body _env)
      = printf "(lambda (%s%s) ...)" args' vararg'
      where args' = unwords args
            vararg' = maybe "" (" . " ++) vararg

-- | Adds types to printouts for debugging.
--   This is similar to the auto-derived instance.
debugShow :: LispValue -> String
debugShow (Atom s)           = printf "Atom:%s" s
debugShow (Bool b)           = printf "Bool:%s" (show b)
debugShow (Number i)         = printf "Number:%s" (show i)
debugShow (String s)         = printf "String:\"%s\"" s
debugShow (List l)           = printf "(%s)" $ spacedShow debugShow l
debugShow (List' l d)        = printf "(%s . %s)" (spacedShow debugShow l)
                                                  (debugShow d)
debugShow f@(PrimitiveF _)   = prettyShow f
debugShow l@(Lambda {})      = prettyShow l



-- | Shows the list's elements separated by spaces.
--   spacedShow show [1,2,3] = "1 2 3"
spacedShow :: (Show a)
           => (a -> String) -- ^ Showing function
           -> [a]           -- ^ List to show
           -> String
spacedShow showF = unwords . map showF