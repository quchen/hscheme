-- | Functions for working with lists.
module Evaluate.List (
      car,
      cdr,
      cons,
      lispNull
) where

import LispLanguage
import LispError

import Control.Monad.Error

-- | 'car' returns the first element of a list.
car :: [LispValue] -> ThrowsError LispValue
car [List  (x:_)  ] = return x
car [List' (x:_) _] = return x
car [List  []     ] = throwError $ BadArg "Expected non-empty list"
car [_            ] = throwError $ BadArg "Expected (dotted?) list"
car args            = throwError $ NumArgs EQ 1 (length args) "car"

-- | 'cdr' returns all but the first element of a list.
cdr :: [LispValue] -> ThrowsError LispValue
cdr [List  (_:xs)    ] = return $ List xs
cdr [List' [_]    dot] = return dot
cdr [List' (_:xs) dot] = return $ List' xs dot
cdr [List  []        ] = throwError $ BadArg "Expected non-empty list"
cdr [_               ] = throwError $ BadArg "Expected (dotted?) list"
cdr args               = throwError $ NumArgs EQ 1 (length args) "cdr"

-- | 'cons' prepends its first argument to the list applied to the second.
--   As a special case, if the second argument is not a list, it creates a
--   dotted list.
cons :: [LispValue] -> ThrowsError LispValue
cons [x, List xs     ] = return $ List (x:xs)
cons [x, List' xs dot] = return $ List' (x:xs) dot
cons [x, y           ] = return $ List' [x] y
cons args              = throwError $ NumArgs EQ 2 (length args) "cons"

lispNull :: [LispValue] -> ThrowsError LispValue
lispNull [List []] = return $ Bool True
lispNull [_]       = return $ Bool False
lispNull args      = throwError $ NumArgs EQ 1 (length args) "null?"
