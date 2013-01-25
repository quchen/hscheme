module Main where

import LispLanguage
import Parser
import Evaluate
import Data.Either

main = do
      let lisps = [
                    "#t" -- Bool
                  , "\"he\\tl\\\\lo\"" -- Complicated string
                  , "(- 1 2 3 4)" -- Primitive
                  , "(+ (- 4 2 1) 3 5 6)" -- Nested
                  , "(- 2 (* -4 -2))" -- Negative number
                  ]
      mapM_ (putStrLn . prettyEval . getLisp) lisps

-- | Parses and un-eithers lisp code
getLisp = either (error . show) id . parseLisp

-- | Evaluates a lisp value and formats it to a pretty string
prettyEval :: LispValue -> String
prettyEval lisp = prettyShow lisp ++ "\n= " ++ prettyShow (evaluate lisp)