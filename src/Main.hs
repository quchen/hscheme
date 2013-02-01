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
                        , "(1 2 \"Quote missing\" 4)" -- List
                        , "'(1 2 3 4)" -- List
                        , "(1 \"Quote missing\" 3 . 4)" -- Dotted list
                        , "'(1 2 3 . 4)" -- Dotted list
                        , "(- 1 2 3 4)" -- Primitive
                        , "(+ (- 4 2 1) 3 5 6)" -- Nested
                        , "(- 2 (* -4 -2))" -- Negative numbers
                        , "'(+ 1 2 3 4)" -- Quoted expression
                        , "(quote 'Too 'many)" -- Quoted expression
                        , "(|| #t #f)" -- Boolean binary operator
                        , "(= 1 2)" -- Boolean numeric operator
                        , "(if #t \"a\" \"b\")" -- If statement
                        , "(if #t \"Too\" \"many\" \"args\")" -- If statement with wrong parameter count
                        , "(+ 1 \"Not a number\")" -- If statement with wrong parameter type
                        , "(car '(1 \"hello\" #t))" -- car with heterogeneous list
                        , "(car '(1))" -- car with singleton list
                        , "(car \"Not a list\")" -- car with wrong argument
                        , "(car '(too) 'many)" -- car with too many arguments
                        , "(cdr '(1 \"hello\" #t))" -- cdr with heterogeneous list
                        , "(cdr '(1))" -- cdr with singleton argument
                        , "(cdr \"Not a list\")" -- cdr with wrong argument
                        , "(cdr 1 2)" -- cdr with too many arguments
                        , "(cons 1 2)" -- cons: two to dotted
                        , "(cons 1 '(2 3))" -- cons: one to list
                        , "(cons 1 '(2 3 . 4))" -- cons: one to dotted list
                        , "(car (cdr (cons \"-->\" '(3 \"<--\"))))" -- car+cdr+cons
                        , "(eq? 1 2)" -- eq? for two numbers
                        , "(eq? '(1 2 3) '(1 2 3))" -- eq? for two lists
                        , "(eq? 1 '(1 2 3))" -- eq? for number/list
                        , "(eq? \"Too\" \"many\" \"args\")" -- eq? with too many args
                        , "(set! hworld \"Unknown variable\")" -- Seting unknown variable
                        , "(define hworld \"Hello World!\")" -- Defining unknown variable
                        , "hworld" -- Reading variable
                        , "(set! hworld \"Jelly World!\")" -- Setting known variable
                        , "hworld" -- Reading altered variable
                        , "(cdr (if (< 1 2) (define x '(1 . 2)) (define x '(2 3))))"
                        , " ; XXX\\n\n (+ (cdr x) ; #This#\\n\n ; #should#\\n\n 3 ;;; #be 5#\\n\n ) ; XXX " -- Comments everywhere
                        , " ( + 1 2 ) " -- Silly spaces
                        , "((lambda (x) x) 3)" -- Lambda
                        , "((lambda (x) (+ x 1)) 3)" -- Lambda
                        , "((lambda (x y) (+ x y)) 1 2)" -- Lambda with multiple args
                        , "((lambda x x) 3 4 5 6)" -- Vararg lambda
                        , "((lambda (x y . z) z) 3 4 5 6)" -- Vararg lambda
                        , "((lambda (x y . z) ((+ x y) z)) 1 2 4 5 6)" -- Vararg lambda. Fails, because evaluates (3 (4 5 6)).
                        , "((lambda (x y x) x) 'multi 'bind)" -- Lambda with multiple binds to the same variable
                        , "((lambda (x) 'Too) 'many 'args)" -- Lambda with multiple binds to the same variable
                        , "((lambda (x) \"Not enough args\"))" -- Lambda with multiple binds to the same variable
                        , "(begin)" -- Error: sequence of nothing.
                        , "(begin '1 '2 '3)" -- Sequence.
                        , "(begin (define (f x y) (+ x y)) (f 3 5))" -- Sequence with Lambda
                        , "(begin (define x 0) (define (f y) (set! x (+ x x y))) (f 1) (f 10) x)" -- Sequence with side-effecting Lambda.
                        , "(let ((plus +) (a 1) (b 2)) (plus a b))" -- Let bindings
                        , "(let ((too 'many 'args)) 'body)" -- Let bindings
                        , "(let (wrong 'format) 'body)" -- Let bindings
                        ]

      let maxLength = maximum . map length $ expressions
      env <- newEnv
      results <- forM expressions $ fullEvaluate env

      let codeResultPairs = zip (map (unwords . lines) expressions) results
          showUnEither (Left l) = show l
          showUnEither (Right r) = show r
          prettyPrintf (lisp, result) = printf "%*s   ==>   %s\n"
                                               maxLength
                                               lisp
                                               (showUnEither result)

      mapM_ prettyPrintf codeResultPairs