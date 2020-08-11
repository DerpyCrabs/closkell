module Compile.ConstFolding (constFolding) where

import Control.Monad.Except
import Data.Either (isRight)
import Data.Env
import Data.Error
import Data.List (isPrefixOf)
import Data.State
import Data.Value
import Eval
import Eval.Primitive
import Types

constFolding val = do
  env <- liftIO primitiveBindings
  state <- liftIO nullState
  constFolding' state env val

constFolding' state env (val : vals) = do
  evaledVal <- evalPure state env val
  compiledVals <- constFolding' state env vals
  return (extractVal evaledVal : compiledVals)
constFolding' state env [] = return []

evalPure :: StateRef -> EnvRef -> LispVal -> IOThrowsError (Either LispVal LispVal)
evalPure state env val@(List _ [Atom _ "forbid-folding", arg]) = return $ Left arg
evalPure state env val@(List _ (Atom _ "defmacro" : Atom _ name : body)) = return (Macro body env) >>= defineVar env name >> (returnVar val)
evalPure state env (List _ [Atom _ "define", Atom _ var, form]) = do
  evaledForm <- evalPure state env form
  case evaledForm of
    Right form -> do
      definedVar <- defineVar env var form
      return $ Right $ (List Nothing [Atom Nothing "define", Atom Nothing var, form])
    Left form -> return $ Left $ (List Nothing [Atom Nothing "define", Atom Nothing var, form])
evalPure state env val@(List _ (Atom _ "define" : List _ (Atom _ var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var >> (returnVar val)
evalPure state env val@(List _ (Atom _ "define" : DottedList _ (Atom _ var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var >> (returnVar val)
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
evalPure state env (List _ (Atom _ "do" : body)) = do
  evaledBody <- mapM (evalPure state env) body
  if all isRight evaledBody
    then Right <$> eval state env (extractVal $ last $ evaledBody)
    else return $ Left $ func "do" $ extractVal <$> evaledBody
evalPure state env (List _ ((Atom _ "let"):bindsAndExpr)) = do
  let binds = init bindsAndExpr
  let expr = last bindsAndExpr
  let vars = matchVar <$> binds
  newEnv <- liftIO $ bindVars env vars
  evaledVars <- evalVars newEnv vars
  case evaledVars of
    Right (vars) -> do
      mapM_ (\(name, var) -> setVar newEnv name var) vars
      evaledExpr <- evalPure state newEnv expr
      case evaledExpr of
        Right res -> return $ Right res
        Left res -> do
          return $ Left (list (concat [[atom "let"], binds, [res]]))
    Left (vars) -> do
      return $ Left (list (concat [[atom "let"], binds, [expr]]))
  where
    matchVar (List _ [Atom _ name, var]) = (name, var)
    vars binds = matchVar <$> binds
    evalVar env (name, var) = do
      evaledVar <- evalPure state env var
      return (name, evaledVar)
    evalVars :: EnvRef -> [(String, LispVal)] -> IOThrowsError (Either [(String, LispVal)] [(String, LispVal)])
    evalVars env vars = let
      sequenceVars vars
        | all isRight (snd <$> vars) = Right $ (\(n, v) -> (n, extractVal v)) <$> vars
        | otherwise = Left $ (\(n, v) -> (n, extractVal v)) <$> vars
      in do 
        evaledVars <- mapM (evalVar env) vars
        return $ sequenceVars evaledVars
  
evalPure state env (List _ (Atom _ "lambda" : (List _ [Atom _ "quote", List _ []]) : body)) =
  makeNormalFunc env [] body >>= returnVar
evalPure state env (List _ (Atom _ "lambda" : List _ params : body)) =
  makeNormalFunc env params body >>= returnVar
evalPure state env (List _ (Atom _ "lambda" : DottedList _ params varargs : body)) =
  makeVarArgs varargs env params body >>= returnVar
evalPure state env (List _ (Atom _ "lambda" : varargs@(Atom _ _) : body)) =
  makeVarArgs varargs env [] body >>= returnVar
evalPure state env val@(Atom _ id) =
  if "io." `isPrefixOf` id
    then return $ Left val
    else do
      var <- getVar env id
      case var of
        (Macro _ _) -> return (Right (atom id))
        _ -> return (Right var)
evalPure state env (List _ [Atom _ "apply", func, args]) = do
  evaledArgs <- evalPure state env args
  case evaledArgs of
    (Right (List _ evaledArgs)) -> evalPure state env (list (func : evaledArgs))
    (Left (List _ evaledArgs)) -> return $ Left $ list [atom "apply", func, list evaledArgs]
evalPure state env val@(List _ (function : args)) = do
  evaledFunc <- evalPure state env function
  case evaledFunc of
    Right (Atom _ name)
      | name `elem` ["forbid-folding", "quote", "unquote", "apply", "io.throw!", "if", "gensym", "do"] ->
        evalPure state env (List Nothing (extractVal evaledFunc : args))
    Right (Atom _ name) -> do
      var <- getVar env name
      case var of
        (Macro _ _) -> do
          result <- applyPure state var args
          case result of
            (Right result) -> evalPure state env result
            (Left _) -> return $ Left $ list (atom name : args)
        _ -> do
          evaledArgs <- mapM (evalPure state env) args
          if all isRight (evaledFunc : evaledArgs)
            then Right <$> eval state env val
            else return $ Left $ list $ extractVal <$> (evaledFunc : evaledArgs)
    _ -> do
      evaledArgs <- mapM (evalPure state env) args
      if all isRight (evaledFunc : evaledArgs)
        then do
          result <- applyPure state (extractVal evaledFunc) (extractVal <$> evaledArgs)
          case result of
            Right (res) -> return $ Right res
            Left _ -> return $ Left $ list $ (function : (extractVal <$> evaledArgs))
        else return $ Left $ list (function : (extractVal <$> evaledArgs))
evalPure state env val = return $ Right val

applyPure :: StateRef -> LispVal -> [LispVal] -> IOThrowsError (Either LispVal LispVal)
applyPure state (PrimitiveFunc func) args = do
  result <- liftThrows $ func args
  return $ Right result
applyPure state (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else do
      env <- (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs
      evaledBody <- evalBody env
      if all isRight evaledBody
        then return $ last evaledBody
        else return $ Left (atom "nil")
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = mapM (evalPure state env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, list $ remainingArgs)]
      Nothing -> return env
applyPure state (IOFunc _) args = throwError $ Default $ "Tried to evaluate IO function at compile time"
applyPure state (Macro body closure) args = applyPure state (Func [] (Just "body") body closure) args
applyPure state k _ = throwError $ Default $ "Invalid apply pure: " ++ show k

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

sequenceVals vals
  | all isRight vals = Right $ extractVal <$> vals
  | otherwise = Left $ extractVal <$> vals

returnVar = return . Right
