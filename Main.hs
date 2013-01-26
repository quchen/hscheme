module Main where

import LispLanguage
import LispError
import Parser
import Evaluate
import Data.Either

main = do
      putStrLn "Basic sanity checks"
      putStrLn "==================="
      let lisps = [
                    "#t" -- Bool
                  , "\"he\\tl\\\\lo\"" -- Complicated string
                  , "(- 1 2 3 4)" -- Primitive
                  , "(+ (- 4 2 1) 3 5 6)" -- Nested
                  , "(- 2 (* -4 -2))" -- Negative number
                  , "(1 2 3 . 4)" -- Dotted list
                  ]
      mapM_ (putStrLn . prettyEval . getLisp) lisps


-- DIRTY SECTION

unEither :: Either LispError LispValue -> LispValue
unEither = either (error . show) id

-- | Parses and un-eithers lisp code
getLisp :: String -> LispValue
getLisp = either (error . show) id . parseLisp

-- | Evaluates a lisp value and formats it to a pretty string
prettyEval :: LispValue -> String
prettyEval lisp =    prettyShow lisp ++ "\n= "
                  ++ prettyShow (either (error . show) id $ evaluate lisp)