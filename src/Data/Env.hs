module Data.Env (getVar, bindVars) where

import Data.Error
import Types

getVar :: Env -> String -> ThrowsError LispVal
getVar env var =
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    return
    (lookup var env)

bindVars :: Env -> Env -> Env
bindVars newEnv oldEnv = newEnv ++ filter (\(name, _) -> name `notElem` (fst <$> newEnv)) oldEnv
