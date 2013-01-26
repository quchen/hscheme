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
-- NOTE: if is lazy in Scheme!
--       http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.5
evaluate (List [Atom "if", p, ifTrue, ifFalse]) = evaluate $
      case p of (Bool False) -> ifFalse
                _            -> ifTrue -- Everything but #f is true
evaluate (List (Atom "if" : xs)) = throwError $ NumArgs 3 (lengthI xs) "if"

-- List functions: car, cons, cdr
evaluate (List (Atom "car"  : xs)) = car  xs
evaluate (List (Atom "cdr"  : xs)) = cdr  xs
evaluate (List (Atom "cons" : xs)) = cons xs

-- Eqv? (== eq?)
evaluate (List [Atom f, a, b])
      | f == "eqv?" || f == "eq?" = return . Bool $ a == b
evaluate (List (Atom f : xs))
      | f == "eqv?" || f == "eq?" = throwError $ NumArgs 2 (lengthI xs) f

-- TODO: evaluate equal?
-- TODO: evaluate cond, case -> http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.1

-- Quoted datums. Leaves its argument unevaluated.
evaluate (List [Atom "quote", expr]) = return expr
evaluate (List (Atom "quote" : xs )) = throwError $ NumArgs 1 (lengthI xs) "quote"

-- Other function application
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
numBinOp _ xs = throwError $ NumArgs 2 (lengthI xs) "Numerical binary function"

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


-- | car returns the first element of a list.
car :: [LispValue] -> Either LispError LispValue
car [xs@(List _)    ] = case evaluate xs of
                              Right (List (x:_)) -> return x
                              Right (List []   ) -> throwError $ BadArg "Expected non-empty list"
                              Right _            -> throwError $ BadArg "Expected (dotted?) list"
                              left               -> left
car [xs@(List' _ _) ] =  case evaluate xs of
                               Right (List' (x:_) _) -> return x
                               Right (List' []    _) -> throwError $ BadArg "Expected non-empty list"
                               Right _               -> throwError $ BadArg "Expected (dotted?) list"
                               left                  -> left
car [_]               = throwError $ BadArg "Expected (dotted?) list"
car xs                = throwError $ NumArgs 1 (lengthI xs) "car"

-- | cdr returns all but the first element of a list.
cdr :: [LispValue] -> Either LispError LispValue
cdr [xs@(List _)    ] = case evaluate xs of
                              Right (List (_:ys)) -> return $ List ys
                              Right (List []   )  -> throwError $ BadArg "Expected non-empty list"
                              Right _             -> throwError $ BadArg "Expected (dotted?) list"
                              left                -> left
cdr [xs@(List' _ _) ] =  case evaluate xs of
                               Right (List' (_:ys) d) -> return $ List' ys d
                               Right (List' []     d) -> throwError $ BadArg "Expected non-empty list"
                               Right _                -> throwError $ BadArg "Expected (dotted?) list"
                               left                   -> left
cdr [_]               = throwError $ BadArg "Expected (dotted?) list"
cdr xs                = throwError $ NumArgs 1 (lengthI xs) "cdr"

-- | cons prepends its first argument to the list applied to the second.
--   As a special case, if the second argument is not a list, it creates a
--   dotted list.
cons :: [LispValue] -> Either LispError LispValue
cons [x, xs] = do
      x' <- evaluate x
      case evaluate xs of
            Right (List xs')      -> return $ List (x':xs')
            Right (List' xs' dot) -> do dot' <- evaluate dot
                                        return $ List' (x':xs') dot'
            Right y               -> do y' <- evaluate y
                                        return $ List' [x'] y'
            left                  -> left
cons xs = throwError $ NumArgs 2 (lengthI xs) "cons"