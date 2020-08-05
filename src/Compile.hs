module Compile (compile) where

import Types
import Eval.Primitive (primitiveBindings)
import Control.Monad.Except
import Data.State
import Data.List (isPrefixOf)
import Eval
import Data.Either (isRight)


compile :: [LispVal] -> IOThrowsError [LispVal]
compile vals = do
  env <- liftIO primitiveBindings
  state <- liftIO nullState
  compile' env state vals 
  
compile' :: EnvRef -> StateRef -> [LispVal] -> IOThrowsError [LispVal]
compile' env state (val:vals) = do
  newVal <- evalPure env state val
  compiled <- compile' env state vals
  return (extractVal newVal:compiled)
compile' env state [] = return []
  
evalPure :: EnvRef -> StateRef -> LispVal -> IOThrowsError (Either LispVal LispVal)
evalPure env state val@(Atom _ atom) = if "io." `isPrefixOf` atom
    then return (Left val)
    else return (Right val)
evalPure env state val@(List _ (func:args)) = do
  func <- evalPure env state func
  args <- mapM (evalPure env state) args
  if all isRight (func:args)
    then Right <$> eval state env val
    else return $ Left $ List Nothing $ extractVal <$> (func:args)
  
evalPure env state val = return $ Right val
  
extractVal (Right val) = val
extractVal (Left val) = val