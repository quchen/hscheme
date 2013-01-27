-- | Primitive built-in functions, in the sense that they're hardcoded in the
--   interpreter. Most (if not all) of these functions could have been
--   implemented as a Lisp library as well.
module Evaluate.Primitive (
      apply
) where

import LispLanguage
import LispError

import qualified Evaluate.List as List
import qualified Evaluate.Equality as Equality

import Data.Map
import Prelude hiding (lookup)
import Data.Functor
import Control.Monad
import Control.Monad.Error




-- | Applies a function to an argument list
apply :: String -- ^ Name of the function
      -> [LispValue] -- ^ Argument list
      -> ThrowsError LispValue
apply fName args = maybe (throwError $ UnknownFunc fName)
                         ($ args)
                         (lookup fName primitiveFunctions)

-- | Collection of primitive functions.
primitiveFunctions :: Map String ([LispValue] -> ThrowsError LispValue)
primitiveFunctions = fromList [
        -- Numerical binary operators
        (        "+", numFoldOp (+) )
      , (        "-", numFoldOp (-) )
      , (        "*", numFoldOp (*) )
      , (        "/", numFoldOp div )
      , (      "mod", numFoldOp mod )
      , ( "quotient", numFoldOp quot)
      , ("remainder", numFoldOp rem )

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

         -- General boolean operators
      , (  "eq?", Equality.eq  )
      , ( "eqv?", Equality.eqv )

        -- String boolean operators
      , (  "string=?", strBoolBinOp (==) )
      , (  "string<?", strBoolBinOp (<)  )
      , (  "string>?", strBoolBinOp (>)  )
      , ( "string<=?", strBoolBinOp (<=) )
      , ( "string>=?", strBoolBinOp (>=) )

        -- List functions
      , ( "cons", List.cons )
      , (  "car", List.car  )
      , (  "cdr", List.cdr  )
      ]

-- | Applies numerical binary operators.
numFoldOp :: (Integer -> Integer -> Integer) -- ^ Binary function
         -> [LispValue]                      -- Argument list to fold over
         -> ThrowsError LispValue
numFoldOp f (x:xs) = foldM f' x xs
      where f' (Number a) (Number b) = return . Number $ a `f` b
            f' _          (Number _) = throwError $ BadArg "Expected number"
            f' _          _          = throwError $ BadArg "Expected number"
numFoldOp _ args = throwError $ NumArgs 2 (length args) "Numerical binary function"

-- | Applies binary operators that map to Bool.
boolBinOp :: (LispValue -> ThrowsError a) -- ^ Unpacking function
          -> (a -> a -> Bool)             -- ^ Binary operator
          -> [LispValue]                  -- ^ Arguments
          -> ThrowsError LispValue
boolBinOp unpack f [x,y] = Bool <$> liftM2 f (unpack x) (unpack y)
boolBinOp _      _ args  = throwError $ NumArgs 2 (length args) "Boolean binary function"

-- | Boolean-valued binary integer operator application
numBoolBinOp :: (Integer -> Integer -> Bool)
             -> [LispValue]
             -> ThrowsError LispValue
numBoolBinOp  = boolBinOp assertNum

-- | Boolean-valued binary integer operator application
boolBoolBinOp :: (Bool -> Bool -> Bool)
              -> [LispValue]
              -> ThrowsError LispValue
boolBoolBinOp = boolBinOp assertBool

-- | Boolean-valued binary integer operator application
strBoolBinOp :: (String -> String -> Bool)
             -> [LispValue]
             -> ThrowsError LispValue
strBoolBinOp  = boolBinOp assertString






-- | Returns the contained number or throws an error.
assertNum :: LispValue -> ThrowsError Integer
assertNum (Number n) = return n
assertNum _          = throwError $ BadArg "Expected number"

-- | Returns the contained bool or throws an error.
assertBool :: LispValue -> ThrowsError Bool
assertBool (Bool b) = return b
assertBool _        = throwError $ BadArg "Expected boolean"

-- | Returns the contained string or throws an error.
assertString :: LispValue -> ThrowsError String
assertString (String s) = return s
assertString _          = throwError $ BadArg "Expected string"