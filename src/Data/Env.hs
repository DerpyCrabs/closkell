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
bindVars env1 env2 = env1 ++ env2
