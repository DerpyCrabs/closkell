module Compile (compile) where

import Types
import Eval.Primitive (primitiveBindings)
import Control.Monad.Except
import Data.State
import Data.List (isPrefixOf)
import Eval
import Data.Value
import Data.Either (isRight)


compile :: [LispVal] -> IOThrowsError [LispVal]
compile vals = do
  env <- liftIO primitiveBindings
  state <- liftIO nullState
  compile' state env vals 
  
compile' :: StateRef -> EnvRef -> [LispVal] -> IOThrowsError [LispVal]
compile' state env (val:vals) = do
  evaledVal <- evalPure state env val
  compiledVals <- compile' state env vals
  return (extractVal evaledVal:compiledVals)
compile' state env [] = return []
  
evalPure :: StateRef -> EnvRef -> LispVal -> IOThrowsError (Either LispVal LispVal)
evalPure state env val@(Atom _ atom) = if "io." `isPrefixOf` atom
    then return (Left val)
    else return (Right val)
evalPure state env val@(List _ (function:args)) = do
  evaledFunc <- evalPure state env function
  case evaledFunc of
    Right (Atom _ "quote") -> do
      quotedArg <- evalPureUnquote state env (head args)
      case quotedArg of
        Right arg -> return $ Right arg
        Left arg -> return $ Left (func "quote" [arg])
    _ -> do
      evaledArgs <- mapM (evalPure state env) args
      if all isRight (evaledFunc:evaledArgs)
        then Right <$> eval state env val
        else return $ Left $ list $ extractVal <$> (evaledFunc:evaledArgs)
  
evalPure state env val = return $ Right val

evalPureUnquote :: StateRef -> EnvRef -> LispVal -> IOThrowsError (Either LispVal LispVal)
evalPureUnquote state env (List pos args) = do
  evaledArgs <- sequenceVals <$> mapM evalUnquoteSplicing args
  case evaledArgs of
    Right args -> return $ Right $ List pos $ concat args
    Left args -> return $ Left $ List pos $ concat args
  where
    evalUnquoteSplicing :: LispVal -> IOThrowsError (Either [LispVal] [LispVal])
    evalUnquoteSplicing (List _ [Atom _ "unquote-splicing", arg]) = do
      evaledArg <- evalPure state env arg
      case evaledArg of
        (Right (List _ vals)) -> return $ Right vals
        (Left arg@(List _ _)) -> return $ Left [func "unquote-splicing" [arg]]
        _ -> throwError $ Default "failed unquote-splicing"
    evalUnquoteSplicing (List _ [Atom _ "unquote", arg]) = do
      evaledArg <- evalPure state env arg
      return $ case evaledArg of
        Right arg -> Right [arg]
        Left arg -> Left [func "unquote" [arg]]
    evalUnquoteSplicing other = do
      evaledOther <- evalPureUnquote state env other
      return $ case evaledOther of
        Right val -> Right [val]
        Left val -> Left [val]
evalPureUnquote state env val = return $ Right val
  
extractVal (Right val) = val
extractVal (Left val) = val

sequenceVals vals | all isRight vals = Right $ extractVal <$> vals
                  | otherwise = Left $ extractVal <$> vals
