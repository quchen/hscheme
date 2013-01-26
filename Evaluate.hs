module Evaluate (
      evaluate
) where

import LispLanguage
import LispError

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

-- Function application
evaluate (List (Atom f : args)) = mapM evaluate args >>= apply f

evaluate unknown = throwError . BadExpr $ "Bad expression: " ++ show unknown

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
functions = fromList [ (        "+", numericBinOp (+) )
                     , (        "-", numericBinOp (-) )
                     , (        "*", numericBinOp (*) )
                     , (        "/", numericBinOp div )
                     , (      "mod", numericBinOp mod )
                     , ( "quotient", numericBinOp quot)
                     , ("remainder", numericBinOp rem )
                     ]

-- | Stores numerical binary operators.
numericBinOp :: (Integer -> Integer -> Integer) -- ^ Binary function
             -> [LispValue]                     -- Argument list to fold over
             -> Either LispError LispValue
numericBinOp f (x:xs) = foldM f' x xs
      where f' (Number a) (Number b) = return . Number $ a `f` b
            f' _          (Number _) = throwError . BadArg $ "First argument must be a number"
            f' _          _          = throwError . BadArg $ "Second argument must be a number"
numericBinOp f _ = throwError $ NumArgs 2