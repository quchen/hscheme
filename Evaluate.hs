module Evaluate (
      evaluate
) where

import LispLanguage
import LispError

import Data.Functor
import Control.Monad
import Control.Monad.Error
import Data.Map hiding (map)
import Prelude hiding (lookup) -- FU prelude

-- | Evaluates a Lisp tree.
evaluate :: LispValue -> Either LispError LispValue

-- Primitive evaluators
evaluate x@(Atom _)   = return x
evaluate x@(Bool _)   = return x
evaluate x@(Number _) = return x
evaluate x@(String _) = return x

-- If statement
evaluate (List [Atom "if", p, ifTrue, ifFalse]) = evaluate $
      case p of (Bool False) -> ifFalse
                _            -> ifTrue -- Everything but #f is true
evaluate (List (Atom "if" : xs)) = throwError $ NumArgs 3 (lengthI xs) "if"

-- Car
evaluate (List (Atom "car" : (List  (x:_)  ) : [] )) = evaluate x -- Replace evaluate by return and you get some form of lazy evaluation :-)
evaluate (List (Atom "car" : (List' (x:_) _) : [] )) = evaluate x
evaluate (List (Atom "car" : _               : [] )) = throwError $ BadArg "Expecting list"
evaluate (List (Atom "car"                   : xs )) = throwError $ NumArgs 1 (lengthI xs) "car"

-- Cdr CONTINUE
-- RULES:
-- (cdr '(a b c)) = (b c)
-- (cdr '(a b)) = (b)
-- (cdr '(a)) = NIL
-- (cdr '(a . b)) = b
-- (cdr '(a b . c)) = (b . c)
-- (cdr 'a) = error (not list)
-- (cdr 'a 'b) = error (too many args)



-- Other application
evaluate (List (Atom f : args)) = mapM evaluate args >>= apply f

evaluate unknown = throwError . BadExpr $ show unknown

-- | Applies f to args
apply :: String -- ^ Name of the function
      -> [LispValue] -- ^ Argument list
      -> Either LispError LispValue
apply fName args = maybe (throwError . UnknownFunc $ "Function \"" ++ fName
                                                     ++ "\" not recognized")
                         ($ args)
                         (lookup fName functions)

-- | Collection of allowed functions.
functions :: Map String ([LispValue] -> Either LispError LispValue)
functions = fromList [
                       -- Numerical binary operators
                       (        "+", numBinOp (+) )
                     , (        "-", numBinOp (-) )
                     , (        "*", numBinOp (*) )
                     , (        "/", numBinOp div )
                     , (      "mod", numBinOp mod )
                     , ( "quotient", numBinOp quot)
                     , ("remainder", numBinOp rem )

                       -- Numerical boolean binary operators
                     , (  "=", numBoolBinOp (==) )
                     , (  "<", numBoolBinOp (<)  )
                     , (  ">", numBoolBinOp (>)  )
                     , ( "<=", numBoolBinOp (<=) )
                     , ( ">=", numBoolBinOp (>=) )
                     , ( "/=", numBoolBinOp (/=) )

                       -- Boolean binary operators
                     , ( "&&", boolBoolBinOp (&&) )
                     , ( "||", boolBoolBinOp (||) )

                       -- String boolean operators
                     , ( "string=?", strBoolBinOp (==) )
                     , ( "string<?", strBoolBinOp (<) )
                     , ( "string>?", strBoolBinOp (>) )
                     , ( "string<=?", strBoolBinOp (<=) )
                     , ( "string>=?", strBoolBinOp (>=) )
                     ]

-- | Applies numerical binary operators.
numBinOp :: (Integer -> Integer -> Integer) -- ^ Binary function
         -> [LispValue]                     -- Argument list to fold over
         -> Either LispError LispValue
numBinOp f (x:xs) = foldM f' x xs
      where f' (Number a) (Number b) = return . Number $ a `f` b
            f' _          (Number _) = throwError $ BadArg "Not a number"
            f' _          _          = throwError $ BadArg "Not a number"
numBinOp f xs = throwError $ NumArgs 2 (lengthI xs) "Numerical binary function"

-- | Applies binary operators that map to Bool.
boolBinOp :: (LispValue -> Either LispError a) -- ^ Unpacking function
          -> (a -> a -> Bool)                  -- ^ Binary operator
          -> [LispValue]                       -- ^ Arguments
          -> Either LispError LispValue
boolBinOp unpack f [x,y] = Bool <$> liftM2 f (unpack x) (unpack y)
boolBinOp _      _ xs     = throwError $ NumArgs 2 (lengthI xs) "Boolean binary function"

-- | Boolean-valued binary integer operator application
numBoolBinOp :: (Integer -> Integer -> Bool)
             -> [LispValue]
             -> Either LispError LispValue
numBoolBinOp  = boolBinOp unpackNum

-- | Boolean-valued binary integer operator application
boolBoolBinOp :: (Bool -> Bool -> Bool)
              -> [LispValue]
              -> Either LispError LispValue
boolBoolBinOp = boolBinOp unpackBool

-- | Boolean-valued binary integer operator application
strBoolBinOp :: (String -> String -> Bool)
             -> [LispValue]
             -> Either LispError LispValue
strBoolBinOp  = boolBinOp unpackString


-- | Returns the contained number of an error.
unpackNum :: LispValue -> Either LispError Integer
unpackNum (Number n) = return n
unpackNum _          = throwError $ BadArg "Expecting boolean"

-- | Returns the contained bool of an error.
unpackBool :: LispValue -> Either LispError Bool
unpackBool (Bool b) = return b
unpackBool _        = throwError $ BadArg "Expecting boolean"

-- | Returns the contained string of an error.
unpackString :: LispValue -> Either LispError String
unpackString (String s) = return s
unpackString _          = throwError $ BadArg "Expecting string"

-- | Prelude.length, but with a Num return value
lengthI :: Num b => [a] -> b
lengthI = fromIntegral . length