module Main where

import LispLanguage
import LispError
import Parser
import Evaluate
import Text.Printf
import Control.Monad
import System.Environment
import Control.Monad.Error
import System.IO

main :: IO ()
main = do
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering
      runWithArgs

-- | Start a REPL (Read-Evaluate-Print Loop)
repl :: IO ()
repl = newEnv >>= repl'
      where repl' env = do
                  putStr "> "
                  expr <- getLine
                  putStr ">>> "
                  result <- fullEvaluate env expr
                  either print print result
                  repl' env

-- | Runs command line argument as Lisp if given, otherwise starts the REPL.
runWithArgs :: IO ()
runWithArgs = do
      args <- getArgs
      env <- newEnv
      case args of (lisp:_) -> fullEvaluate env lisp >>= either print print
                   _        -> repl

-- | Parses and evaluates a Lisp expression,.
fullEvaluate :: EnvR -- ^ Pointer to the environment to run in
             -> String -- ^ Lisp code
             -> IO (Either LispError LispValue)
fullEvaluate env expr = runErrorT $ liftThrows (parseLisp expr) >>= evaluate env

-- | Prints a couple of Lisp expressions and what they evaluate to
testExpressions :: IO ()
testExpressions = do
      putStrLn "Basic sanity checks"
      putStrLn "==================="

      let expressions = [ "#t" -- Bool
                        , "\"he\\\"l\\lo\"" -- Complicated string. Note that Haskell parses every second backslash away, then Parsec will do the same. Therefore, only 1/4 of the backslashes actually make it to Lisp :-)
                        , "(1 2 3 4)" -- List
                        , "'(1 2 3 4)" -- List
                        , "(1 2 3 . 4)" -- Dotted list
                        , "'(1 2 3 . 4)" -- Dotted list
                        , "(- 1 2 3 4)" -- Primitive
                        , "(+ (- 4 2 1) 3 5 6)" -- Nested
                        , "(- 2 (* -4 -2))" -- Negative numbers
                        , "'(+ 1 2 3 4)" -- Quoted expression
                        , "(quote 'a 'b)" -- Quoted expression
                        , "(|| #t #f)" -- Boolean binary operator
                        , "(= 1 2)" -- Boolean numeric operator
                        , "(if #t \"a\" \"b\")" -- If statement
                        , "(if #t \"a\" \"b\" \"x\")" -- If statement with wrong parameter count
                        , "(+ #t 2)" -- If statement with wrong parameter type
                        , "(car '(1 \"hello\" #t))" -- car with heterogeneous list
                        , "(car '(1))" -- car with singleton list
                        , "(car 1)" -- car with wrong argument
                        , "(car 1 2)" -- car with too many arguments
                        , "(cdr '(1 \"hello\" #t))" -- cdr with heterogeneous list
                        , "(cdr '(1))" -- cdr with singleton argument
                        , "(cdr 1)" -- cdr with wrong argument
                        , "(cdr 1 2)" -- cdr with too many arguments
                        , "(cons 1 2)" -- cons: two to dotted
                        , "(cons 1 '(2 3))" -- cons: one to list
                        , "(cons 1 '(2 3 . 4))" -- cons: one to dotted list
                        , "(car (cdr (cons 2 '(3 4))))" -- car+cdr+cons
                        , "(eq? 1 2)" -- eq? for two numbers
                        , "(eq? '(1 2 3) '(1 2 3))" -- eq? for two lists
                        , "(eq? 1 '(1 2 3))" -- eq? for number/list
                        , "(eq? 1 2 3)" -- eq? with too many args
                        , "(set! hworld \"Hello World!\")" -- Seting unknown variable
                        , "(define hworld \"Hello World!\")" -- Defining unknown variable
                        , "hworld" -- Reading variable
                        , "(set! hworld \"Jelly World!\")" -- Setting known variable
                        , "hworld" -- Reading altered variable
                        , "(cdr (if (< 1 2) (define x '(1 . 2)) (define x '(2 3))))"
                        ]

      let maxLength = maximum . map length $ expressions
      env <- newEnv
      results <- forM expressions $ fullEvaluate env

      let codeResultPairs = zip expressions results
          showUnEither (Left l) = show l
          showUnEither (Right r) = show r
          prettyPrintf (lisp, result) = printf "%*s   ==>   %s\n"
                                               maxLength
                                               lisp
                                               (showUnEither result)

      mapM_ prettyPrintf codeResultPairs