-- | Defines an environment and functions to work with mutable variables.
module Evaluate.Mutable (
      EnvR,
      newEnv,
      isSet,
      readVar,
      setVar,
      defineVar,
      setScopeVars
) where


import LispLanguage
import LispError

import Control.Monad.Error
import Data.IORef
import Data.Map
import Data.Maybe
import Data.Monoid
import Prelude hiding (lookup)



-- | Variable database. Variable values are mutable, but variable definitions
--   are not. Use 'EnvR' for that.
type Env = Map String (IORef LispValue)

-- | Pointer to the variable database (R = Reference). Same as 'Env', but in an
--   'EnvR' variables can be created. (Note that there's no Scheme function for
--   deleting variables, although this interface would permit making one.)
type EnvR = IORef Env

-- | New empty environment
newEnv :: IO EnvR
newEnv = newIORef empty

-- | Checks whether a variable is set in the current environment.
isSet :: EnvR
      -> String -- ^ Variable name
      -> IO Bool
isSet envR var = fmap (isJust . lookup var) $ readIORef envR

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

-- | Creates an environment with certain new variables
setScopeVars :: EnvR -> [(String, LispValue)] -> IO EnvR
setScopeVars envR newVars = readIORef envR >>= addVars >>= newIORef
      where
            -- | Adds the new variables provided by the parent scope to the
            --   environment.
            addVars :: Env -> IO Env
            addVars env = do
                  env' <- fmap fromList $ mapM makeRef newVars
                  return $ env' <> env -- Note that (<>) is left-biased for
                                       -- Data.Map, therefore env' variables
                                       -- are inserted with higher priority.

            -- | Takes (var, value) and packs the value into an IORef, resulting
            --   in (var, IORef value).
            makeRef :: (a, b) -> IO (a, IORef b)
            makeRef (var, value) = do valueR <- newIORef value
                                      return (var, valueR)