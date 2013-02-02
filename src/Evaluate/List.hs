-- | Functions for working with lists.
module Evaluate.List (
      car,
      cdr,
      cons,
      nullQ,
      pairQ,
      lispLength,
      lispReverse,
      listTail,
      list
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

-- | Checks whether the argument is the empty list.
-- TODO: What about dotted lists?
nullQ :: [LispValue] -> ThrowsError LispValue
nullQ [List []] = return $ Bool True
nullQ [_]       = return $ Bool False
nullQ args      = throwError $ NumArgs EQ 1 (length args) "null?"

-- | Checks whether the argument is a pair.
--   Note that a pair is anything on which car and cons work, i.e. non-empty
--   lists.
pairQ :: [LispValue] -> ThrowsError LispValue
pairQ [List  _  ] = return $ Bool True
pairQ [List' _ _] = return $ Bool True
pairQ [_        ] = return $ Bool False
pairQ args      = throwError $ NumArgs EQ 1 (length args) "pair?"

-- | Creates a list out of all its arguments.
list :: [LispValue] -> ThrowsError LispValue
list xs = return $ List xs

-- | Calculates the length of a list.
lispLength :: [LispValue] -> ThrowsError LispValue
lispLength [List xs] = return . Number . toInteger $ length xs
lispLength (_:_:_)   = throwError $ BadArg "'length' expected single argument"
lispLength _         = throwError $ BadArg "'length' expected list"

-- TODO append
-- TODO The specification is shitty on this one, check what
-- it does using some other interpreter

lispReverse :: [LispValue] -> ThrowsError LispValue
lispReverse [List xs] = return . List . reverse $ xs
lispReverse [_]       = throwError $ BadArg "'reverse' expected list"
lispReverse args      = throwError $ NumArgs EQ 1 (length args) "reverse"

listTail :: [LispValue] -> ThrowsError LispValue
listTail [List xs, Number k] = go xs k
      where go :: [LispValue] -> Integer -> ThrowsError LispValue
            go _  r| r < 0 = throwError $ BadArg "'listTail' expected nonnegative argument"
            go ys 0 = return $ List ys
            go [] _ = throwError . BadArg $ "'listTail' argument longer than requested "
                                                                ++ show k ++ " elements"
            go (_:ys) r = go ys (r-1)
listTail [_, Number _] = throwError . BadArg $ "'listTail' expected list"
listTail [_, _] = throwError . BadArg $ "'listTail' expected number"
listTail args      = throwError $ NumArgs EQ 2 (length args) "listTail"

-- TODO list-ref, assoc