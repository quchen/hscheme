-- | Defines an environment and functions to work with mutable variables.
module Evaluate.Mutable (
      newEnv,
      isSet,
      readVar,
      setVar,
      defineVar,
      setScopeVars
) where


import LispLanguage
import LispError
import Evaluate.Standard as Standard

import Control.Monad.Error
import Data.IORef
import Data.Map
import Data.Traversable (traverse)
import Data.Maybe
import Data.Monoid
import Prelude hiding (lookup)





-- | New empty environment. Only the primitive functions are set.
newEnv :: IO EnvR
newEnv = newIORef empty >>= setScopeVars Standard.functions

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
setScopeVars :: Map String LispValue -> EnvR -> IO EnvR
setScopeVars newVars envR = readIORef envR >>= addVars >>= newIORef
      where
            -- | Adds the new variables provided by the parent scope to the
            --   environment.
            addVars :: Env -> IO Env
            addVars env = do
                  env' <- traverse newIORef newVars
                  return $ env' <> env -- Note that (<>) is left-biased for
                                       -- Data.Map, therefore env' variables
                                       -- are inserted with higher priority.