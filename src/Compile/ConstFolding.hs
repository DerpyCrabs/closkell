module Compile.ConstFolding (constFolding) where
  
import Types
import Data.List (isPrefixOf)
import Eval
import Data.Value
import Data.Either (isRight)
import Eval.Primitive
import Control.Monad.Except
import Data.State

constFolding val = do
  env <- liftIO primitiveBindings
  state <- liftIO nullState
  constFolding' state env val

constFolding' state env (val:vals) = do
  evaledVal <- evalPure state env val
  compiledVals <- constFolding' state env vals
  return (extractVal evaledVal:compiledVals)
constFolding' state env [] = return []
  
evalPure :: StateRef -> EnvRef -> LispVal -> IOThrowsError (Either LispVal LispVal)
evalPure state env (List _ [Atom _ "unquote", val]) = evalPure state env val
evalPure state env (List _ [Atom _ "gensym"]) = do
  counter <- liftIO $ nextGensymCounter state
  return $ Right $ Atom Nothing (show counter)
evalPure state env (List _ [Atom _ "gensym", (String prefix)]) = do
  counter <- liftIO $ nextGensymCounter state
  return $ Right $ Atom Nothing ((prefix ++) $ show counter)
evalPure state env (List _ [Atom _ "quote", val]) = do
  quotedArg <- evalPureUnquote state env val
  case quotedArg of
    Right arg -> return $ Right arg
    Left arg -> return $ Left (func "quote" [arg])
evalPure state env (List _ [Atom _ "if", pred, conseq, alt]) = do
  evaledPred <- evalPure state env pred
  case evaledPred of
    Right (Bool True) -> evalPure state env conseq
    Right (Bool False) -> evalPure state env alt
    Left pred -> return $ Left $ list [atom "if", pred, conseq, alt]
evalPure state env val@(Atom _ atom) = if "io." `isPrefixOf` atom
  then return (Left val)
  else return (Right val)
evalPure state env val@(List _ (function:args)) = do
  evaledFunc <- evalPure state env function
  case evaledFunc of
    Right (Atom _ name) | name `elem` ["quote", "unquote", "apply", "io.throw!", "load", "if", "gensym"] -> 
      evalPure state env (List Nothing (extractVal evaledFunc : args))
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