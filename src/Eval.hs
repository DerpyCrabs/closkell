{-# LANGUAGE ExistentialQuantification #-}

module Eval
  ( eval,
  )
where

import Control.Monad.Except
import Data.Env
import Data.Error
import Data.State
import Data.Value
import Types
import Eval.Primitive
import Parse (readExprList, load)

eval :: StateRef -> EnvRef -> LispVal -> IOThrowsError LispVal
eval state env val@(String _) = return val
eval state env val@(Character _) = return val
eval state env val@(Integer _) = return val
eval state env val@(Float _) = return val
eval state env val@(Bool _) = return val
eval state env (Atom _ id) = do
  isMacro <- liftIO $ isBound envMacros env id
  if isMacro
    then do
      return (Atom Nothing id)
    else do
      getVar env id
eval state env (List _ [Atom _ "quote", val]) = evalUnquote state env val
eval state env (List _ [Atom _ "apply", func, args@(List _ _)]) = do
  func <- eval state env func
  (List _ args) <- eval state env args
  apply state func args
eval state env (List _ [Atom _ "unquote", val]) = eval state env val
eval state env (List _ [Atom _ "gensym"]) = do
  counter <- liftIO $ nextGensymCounter state
  return $ Atom Nothing (show counter)
eval state env (List _ [Atom _ "gensym", (String prefix)]) = do
  counter <- liftIO $ nextGensymCounter state
  return $ Atom Nothing ((prefix ++) $ show counter)
eval state env (List _ [Atom _ "if", pred, conseq, alt]) =
  do
    result <- eval state env pred
    case result of
      Bool False -> eval state env alt
      Bool True -> eval state env conseq
      _ -> throwError $ TypeMismatch "boolean" result
eval state env (List _ [Atom _ "io.throw!", obj]) = eval state env obj >>= throwError . FromCode
eval state env (List _ (Atom _ "defmacro" : Atom _ name : body)) = return (Func [] (Just "body") body env) >>= defineMacro env name
eval state env (List _ [Atom _ "define", Atom _ var, form]) = eval state env form >>= defineVar env var
eval state env (List _ (Atom _ "define" : List _ (Atom _ var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval state env (List _ (Atom _ "define" : DottedList _ (Atom _ var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval state env (List _ (Atom _ "lambda" : (List _ [Atom _ "quote", List _ []]) : body)) =
  makeNormalFunc env [] body
eval state env (List _ (Atom _ "lambda" : List _ params : body)) =
  makeNormalFunc env params body
eval state env (List _ (Atom _ "lambda" : DottedList _ params varargs : body)) =
  makeVarArgs varargs env params body
eval state env (List _ (Atom _ "lambda" : varargs@(Atom _ _) : body)) =
  makeVarArgs varargs env [] body
eval state env (List _ [Atom _ "load", String filename]) =
  load filename >>= fmap last . mapM (eval state env)
eval state env (List pos (function : args)) = do
  evaledFunc <- eval state env function
  case evaledFunc of
    (Atom _ name) | name `elem` ["quote", "unquote", "apply", "io.throw!", "load", "if", "gensym"] -> do
      eval state env (List pos (evaledFunc : args))
    (Atom _ name) -> do
      isMacro <- liftIO $ isBound envMacros env name
      if isMacro
        then do
          macro <- getMacro env name
          result <- apply state macro args
          eval state env result
        else do
          argVals <- mapM (eval state env) args
          apply state evaledFunc argVals
    _ -> do
      argVals <- mapM (eval state env) args
      apply state evaledFunc argVals
eval state env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalUnquote :: StateRef -> EnvRef -> LispVal -> IOThrowsError LispVal
evalUnquote state env (List pos exprs) = List pos . concat <$> mapM evalUnquoteSplicing exprs
  where
    evalUnquoteSplicing (List _ [Atom _ "unquote-splicing", vals]) = do
      vals <- eval state env vals
      case vals of
        (List _ vals) -> return vals
        _ -> throwError $ Default "failed unquote-splicing"
    evalUnquoteSplicing (List _ [Atom _ "unquote", val]) = (: []) <$> eval state env val
    evalUnquoteSplicing other = (: []) <$> evalUnquote state env other
evalUnquote state env other = return other

apply :: StateRef -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply state (PrimitiveFunc func) args = liftThrows $ func args
apply state (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval state env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, list $ remainingArgs)]
      Nothing -> return env
apply state (IOFunc func) args = func args
apply state k _ = throwError $ Default $ "Invalid apply " ++ show k