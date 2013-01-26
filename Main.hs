module Main where

import LispLanguage
import LispError
import Parser
import Evaluate
import Text.Printf
import Control.Monad
import System.Environment

main :: IO ()
main = runWithArgs

-- | Start a REPL (Read-Evaluate-Print Loop)
repl :: IO ()
repl = do
      expr <- putStr "> " >> getLine
      putStr ">>> "
      either print print $ parseLisp expr >>= evaluate
      repl

-- | Runs command line argument as Lisp if given, otherwise starts the REPL.
runWithArgs :: IO ()
runWithArgs = do
      args <- getArgs
      case args of (lisp:_) -> either print print $ parseLisp lisp >>= evaluate
                   _        -> repl

-- | Prints a couple of Lisp expressions and what they evaluate to
testExpressions :: IO ()
testExpressions = do
      putStrLn "Basic sanity checks"
      putStrLn "==================="

      let expressions = [ "#t" -- Bool
                        , "\"he\\\"l\\\\lo\"" -- Complicated string
                        , "(1 2 3 4)" -- List
                        , "(1 2 3 . 4)" -- Dotted list
                        , "(- 1 2 3 4)" -- Primitive
                        , "(+ (- 4 2 1) 3 5 6)" -- Nested
                        , "(- 2 (* -4 -2))" -- Negative number
                        , "'(+ 1 2 3 4)" -- Quoted expression
                        , "(quote 'a 'b)" -- Quoted expression
                        , "(|| #t #f)" -- Boolean binary operator
                        , "(= 1 2)" -- Boolean numeric operator
                        , "(if #t \"a\" \"b\")" -- If statement
                        , "(if #t \"a\" \"b\" \"x\")" -- If statement with wrong parameter count
                        , "(+ #t 2)" -- If statement with wrong parameter type
                        , "(car '(1 \"hello\" #t))" -- car
                        , "(car '(1))" -- car
                        , "(car 1)" -- car
                        , "(car 1 2)" -- car
                        , "(cdr '(1 \"hello\" #t))" -- cdr
                        , "(cdr '(1))" -- cdr
                        , "(cdr 1)" -- cdr
                        , "(cdr 1 2)" -- cdr
                        , "(cons 1 2)" -- cons: two to dotted
                        , "(cons 1 (2 3))" -- cons: one to list
                        , "(cons 1 (2 3 . 4))" -- cons: one to dotted list
                        , "(car (cdr (cons 2 (3 4))))" -- car+cdr+cons
                        , "(eq? 1 2)" -- eq? for two numbers
                        , "(eq? (1 2 3) (1 2 3))" -- eq? for two lists
                        , "(eq? 1 (1 2 3))" -- eq? for number/list
                        , "(eq? 1 2 3)" -- eq? with too many args
                        ]


      let maxLength = maximum . map (length . prettyShow . getLisp)
            -- The above parses the stuff before evaluation, but this is just
            -- a toy expression anyway
      forM_ expressions $ doEverything (maxLength expressions)


-- DIRTY SECTION

unEither :: Either LispError LispValue -> LispValue
unEither = either (error . show) id

-- | Parses and un-eithers lisp code
getLisp :: String -> LispValue
getLisp = either (error . show) id . parseLisp

-- | Evaluates a lisp value and formats it to a pretty string
prettyEval :: Int -> LispValue -> String
prettyEval pad lisp = printf "%*s   ==>   %s" pad
                                              (prettyShow lisp)
                                              (either show prettyShow $ evaluate lisp)

-- | Read, parse, evaluate, prettyprint Lisp. First argument is padding so that
--   multiple expressions can be printed the same way.
doEverything :: Int -> String -> IO ()
doEverything pad = putStrLn . prettyEval pad . getLisp