module Data.Env (nullEnv, getVar, setVar, defineVar, bindVars, defineMacro, getMacro, isBound, envMacros, envFunctions) where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import Types

nullEnv :: IO EnvRef
nullEnv = newIORef (Env {functions = [], macros = []})

envFunctions :: EnvRef -> IO [(String, IORef LispVal)]
envFunctions = fmap getFunctions . readIORef
  where
    getFunctions (Env functions macros) = functions

envMacros :: EnvRef -> IO [(String, IORef LispVal)]
envMacros = fmap getMacros . readIORef
  where
    getMacros (Env functions macros) = macros

isBound :: (EnvRef -> IO [(String, IORef LispVal)]) -> EnvRef -> String -> IO Bool
isBound getEnv envRef var = isJust . lookup var <$> getEnv envRef

getVar :: EnvRef -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ envFunctions envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

getMacro :: EnvRef -> String -> IOThrowsError LispVal
getMacro envRef var = do
  env <- liftIO $ envMacros envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound macro" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: EnvRef -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ envFunctions envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value

setMacro :: EnvRef -> String -> LispVal -> IOThrowsError LispVal
setMacro envRef var value = do
  env <- liftIO $ envMacros envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound macro" var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value

defineVar :: EnvRef -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envFunctions envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef env {functions = (var, valueRef) : functions env}
      return value

defineMacro :: EnvRef -> String -> LispVal -> IOThrowsError LispVal
defineMacro envRef var value = do
  alreadyDefined <- liftIO $ isBound envMacros envRef var
  if alreadyDefined
    then setMacro envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef env {macros = (var, valueRef) : macros env}
      return value

bindVars :: EnvRef -> [(String, LispVal)] -> IO EnvRef
bindVars envRef bindings = do
  extendedFunctions <- envFunctions envRef >>= extendEnv bindings
  env <- readIORef envRef
  newIORef (env {functions = extendedFunctions})
  where
    extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)
