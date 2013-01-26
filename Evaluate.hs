module Evaluate (
      evaluate
) where

import Prelude hiding (lookup) -- FU prelude
import LispLanguage
import Data.List (foldl1')
import Data.Map hiding (map)

-- | Evaluates a Lisp tree.
evaluate :: LispValue -> LispValue

-- Primitive evaluators
evaluate x@(Atom _) = x
evaluate x@(Bool _) = x
evaluate x@(Number _) = x
evaluate x@(String _) = x

-- Function application
evaluate (List (Atom f : args)) = apply f $ map evaluate args

evaluate x = error $ "\"" ++ show x ++ "\" is not implemented yet"

-- | Applies f to args
apply f args = maybe (Bool False) ($ args) $ lookup f functions

-- | Collection of allowed functions.
functions :: Map String ([LispValue] -> LispValue)
functions = fromList [ (        "+", numericBinOp (+) )
                     , (        "-", numericBinOp (-) )
                     , (        "*", numericBinOp (*) )
                     , (        "/", numericBinOp div )
                     , (      "mod", numericBinOp mod )
                     , ( "quotient", numericBinOp quot)
                     , ("remainder", numericBinOp rem )
                     ]

-- | Stores numerical binary operators.
numericBinOp :: (Integer -> Integer -> Integer) -> [LispValue] -> LispValue
numericBinOp f = foldl1' f'
      where getNumber (Number n) = n
            getNumber _          = 0 -- TODO: make this an error
            f' a b = Number $ getNumber a `f` getNumber b