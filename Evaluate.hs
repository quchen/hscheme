module Evaluate (
      evaluate
) where

import LispLanguage
import LispError

import Data.Maybe
import Data.Monoid
import Data.Functor
import Control.Monad
import Control.Monad.Error
import Data.Map hiding (map)
import Data.IORef
import Prelude hiding (lookup) -- FU prelude

type ThrowsError = Either LispError

type ThrowsErrorIO = ErrorT LispError IO

-- | Lifts a non-IO error into the error transformer
liftThrows :: ThrowsError a -> ThrowsErrorIO a
liftThrows (Right r) = return r
liftThrows (Left  l) = throwError l




-- #############################################################################
-- ## Variable handling ########################################################
-- #############################################################################

type Env = Map String (IORef LispValue)

-- | Pointer to the variable database (R = Reference)
type EnvR = IORef Env

emptyEnv :: IO EnvR
emptyEnv = newIORef empty

-- | Checks whether a variable is set in the current environment.
isSet :: EnvR
      -> String -- ^ Variable name
      -> IO Bool
isSet envR var = readIORef envR >>= return . isJust . lookup var

-- | Reads the value of a variable
readVar :: EnvR
        -> String -- ^ Variable name
        -> ThrowsErrorIO LispValue
readVar envR var = do
      env <- liftIO $ readIORef envR
      maybe (throwError $ UnknownVar var)
            (liftIO . readIORef)
            (lookup var env)

-- | Sets the value of an existing variable.
setVar :: EnvR
       -> String -- ^ Variable name
       -> LispValue -- ^ New value
       -> ThrowsErrorIO LispValue
setVar envR var value = do
      env <- liftIO $ readIORef envR
      maybe (throwError $ UnknownVar var)
            (liftIO . flip writeIORef value)
            (lookup var env)
      return value

-- | Creates or overwrites a variable.
-- TODO: Probably doesn't matter, but maybe checking whether the variable is
--       already defined instead of just overwriting without looking may be
--       more efficient.
defineVar :: EnvR
          -> String
          -> LispValue
          -> ThrowsErrorIO LispValue
defineVar envR var value = do
      env <- liftIO $ readIORef envR
      valueR <- liftIO $ newIORef value
      liftIO $ writeIORef envR $ insert var valueR env
      return value

      -- bindVars :: Env -> [(String, LispVal)] -> IO Env

-- | Creates an environment with certain new variables
setScopeVars :: EnvR -> [(String, LispValue)] -> IO EnvR
setScopeVars envR assocs = readIORef envR >>= addVars >>= newIORef
      where
            -- | Adds the assocs provided by the parent scope to the
            --   environment.
            addVars :: Env -> IO Env
            addVars env = do
                  newEnv <- fmap fromList $ mapM makeRef assocs
                  return $ newEnv <> env -- Note that (<>) is left-biased for
                                         -- Data.Map, therefore newEnv variables
                                         -- are inserted with higher priority.

            -- | Takes (var, value) and packs the value into an IORef, resulting
            --   in (var, IORef value).
            makeRef :: (a, b) -> IO (a, IORef b)
            makeRef (var, value) = do valueR <- newIORef value
                                      return (var, valueR)


-- #############################################################################
-- ## Evaluation function ######################################################
-- #############################################################################

-- | Evaluates a Lisp tree.
evaluate :: LispValue -> ThrowsError LispValue

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
evaluate (List (Atom "if" : xs)) = throwError $ NumArgs 3 (length xs) "if"

-- TODO: evaluate equal?
-- TODO: evaluate cond, case -> http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.1

-- Quoted datums. Leaves its argument unevaluated.
evaluate (List [Atom "quote", expr]) = return expr
evaluate (List (Atom "quote" : xs )) = throwError $ NumArgs 1 (length xs) "quote"

-- Other function application
evaluate (List (Atom f : args)) = mapM evaluate args >>= apply f

evaluate unknown = throwError . BadExpr $ show unknown





-- #############################################################################
-- ## General function evaluation interface ####################################
-- #############################################################################

-- | Applies f to args
apply :: String -- ^ Name of the function
      -> [LispValue] -- ^ Argument list
      -> ThrowsError LispValue
apply fName args = maybe (throwError . UnknownFunc $ "Function \"" ++ fName
                                                     ++ "\" not recognized")
                         ($ args)
                         (lookup fName functions)

-- | Collection of allowed functions.
functions :: Map String ([LispValue] -> ThrowsError LispValue)
functions = fromList [
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
                     , ( "eq?", eq )
                     , ( "eqv?", eq )

                       -- String boolean operators
                     , ( "string=?", strBoolBinOp (==) )
                     , ( "string<?", strBoolBinOp (<) )
                     , ( "string>?", strBoolBinOp (>) )
                     , ( "string<=?", strBoolBinOp (<=) )
                     , ( "string>=?", strBoolBinOp (>=) )

                       -- List functions
                     , ( "cons", cons )
                     , ( "car", car )
                     , ( "cdr", cdr )
                     ]

-- | Applies numerical binary operators.
numFoldOp :: (Integer -> Integer -> Integer) -- ^ Binary function
         -> [LispValue]                     -- Argument list to fold over
         -> ThrowsError LispValue
numFoldOp f (x:xs) = foldM f' x xs
      where f' (Number a) (Number b) = return . Number $ a `f` b
            f' _          (Number _) = throwError $ BadArg "Expected number"
            f' _          _          = throwError $ BadArg "Expected number"
numFoldOp _ xs = throwError $ NumArgs 2 (length xs) "Numerical binary function"

-- | Applies binary operators that map to Bool.
boolBinOp :: (LispValue -> ThrowsError a) -- ^ Unpacking function
          -> (a -> a -> Bool)                  -- ^ Binary operator
          -> [LispValue]                       -- ^ Arguments
          -> ThrowsError LispValue
boolBinOp unpack f [x,y] = Bool <$> liftM2 f (unpack x) (unpack y)
boolBinOp _      _ xs     = throwError $ NumArgs 2 (length xs) "Boolean binary function"

-- | Boolean-valued binary integer operator application
numBoolBinOp :: (Integer -> Integer -> Bool)
             -> [LispValue]
             -> ThrowsError LispValue
numBoolBinOp  = boolBinOp unpackNum

-- | Boolean-valued binary integer operator application
boolBoolBinOp :: (Bool -> Bool -> Bool)
              -> [LispValue]
              -> ThrowsError LispValue
boolBoolBinOp = boolBinOp unpackBool

-- | Boolean-valued binary integer operator application
strBoolBinOp :: (String -> String -> Bool)
             -> [LispValue]
             -> ThrowsError LispValue
strBoolBinOp  = boolBinOp unpackString





-- #############################################################################
-- ## Unpackers ################################################################
-- #############################################################################

-- | Returns the contained number of an error.
unpackNum :: LispValue -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum _          = throwError $ BadArg "Expecting boolean"

-- | Returns the contained bool of an error.
unpackBool :: LispValue -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool _        = throwError $ BadArg "Expecting boolean"

-- | Returns the contained string of an error.
unpackString :: LispValue -> ThrowsError String
unpackString (String s) = return s
unpackString _          = throwError $ BadArg "Expecting string"





-- #############################################################################
-- ## List functions ###########################################################
-- #############################################################################

-- | car returns the first element of a list.
car :: [LispValue] -> ThrowsError LispValue
car [List  (x:xs)    ] = return x
car [List' (x:xs) dot] = return x
car [_               ] = throwError $ BadArg "Expected (dotted?) list"
car xs                 = throwError $ NumArgs 1 (length xs) "car"

-- | cdr returns all but the first element of a list.
cdr :: [LispValue] -> ThrowsError LispValue
cdr [List  (x:xs)    ] = return $ List xs
cdr [List' (x:xs) dot] = return $ List' xs dot
cdr [_               ] = throwError $ BadArg "Expected (dotted?) list"
cdr xs                 = throwError $ NumArgs 1 (length xs) "cdr"

-- | cons prepends its first argument to the list applied to the second.
--   As a special case, if the second argument is not a list, it creates a
--   dotted list.
cons :: [LispValue] -> ThrowsError LispValue
cons [x, List xs     ] = return $ List (x:xs)
cons [x, List' xs dot] = return $ List' (x:xs) dot
cons [x, y           ] = return $ List' [x] y
cons xs                = throwError $ NumArgs 2 (length xs) "cons"





-- #############################################################################
-- ## Equality functions #######################################################
-- #############################################################################
-- | Equality check
eq :: [LispValue] -> ThrowsError LispValue
eq [x, y] = return . Bool $ x == y
eq xs     = throwError $ NumArgs 2 (length xs) "eq"

-- | Equality check again. Identical to 'eq'.
-- TODO: Implement some differences? Behavior is correct but redundant right now
eqv :: [LispValue] -> ThrowsError LispValue
eqv [x,y] = eq [x,y]
eqv xs    = throwError $ NumArgs 2 (length xs) "eqv"