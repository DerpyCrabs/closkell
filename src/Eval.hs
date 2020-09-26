module Eval
  ( stepEval,
    eval,
  )
where

import Control.Monad.Except
import Data.Env
import Data.Error
import Data.Maybe (isNothing)
import Data.State
import Data.Value
import Types

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val = eval' z
  where
    z = lvSetEnv env . lvFromAST $ val
    eval' z = do
      nextStep <- stepEval z
      case nextStep of
        (_, Nothing, _) -> return $ lvToAST z
        (_, Just _, _) -> eval' nextStep

stepEval :: LispValZipper -> IOThrowsError LispValZipper
stepEval z@(_, Just (String _), _) = return $ lvNext z
stepEval z@(_, Just (Character _), _) = return $ lvNext z
stepEval z@(_, Just (Integer _), _) = return $ lvNext z
stepEval z@(_, Just (Float _), _) = return $ lvNext z
stepEval z@(_, Just (Bool _), _) = return $ lvNext z
stepEval z@(env, Just (Atom _ id), _) = do
  var <- liftThrows $ getVar env id
  return . lvSet var $ z
stepEval z@(env, Just (List _ [Atom _ "lambda", (List _ [Atom _ "quote", List _ []]), body]), _) =
  return . lvNext . lvSet (makeNormalFunc env [] body) $ z
stepEval z@(env, Just (List _ [Atom _ "lambda", List _ params, body]), _) =
  return . lvNext . lvSet (makeNormalFunc env params body) $ z
stepEval z@(env, Just (List _ [Atom _ "lambda", DottedList _ params varargs, body]), _) =
  return . lvNext . lvSet (makeVarArgs varargs env params body) $ z
stepEval z@(env, Just (List pos (function : args)), _) =
  case function of
    PrimitiveFunc f -> do
      res <- liftThrows $ f args
      return . lvSet res $ z
    IOFunc f -> do
      res <- f args
      return . lvSet res $ z
    f@(Func params varargs body closure) -> liftThrows $ applyFunc f z args
    _ -> stepEval . lvDown $ z
stepEval z@(_, Just (PrimitiveFunc _), _) = return . lvNext $ z
stepEval z@(_, Just (IOFunc _), _) = return . lvNext $ z
stepEval z@(_, Just (Func {}), _) = return . lvNext $ z
stepEval (_, Just badForm, _) = throwError $ BadSpecialForm "Unrecognized special form" badForm

applyFunc :: LispVal -> LispValZipper -> [LispVal] -> ThrowsError LispValZipper
applyFunc (Func params varargs body closure) z args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else do
      let env = bindVarArgs varargs . bindVars closure $ zip params args
      return . lvSet body . lvSetEnv env $ z
  where
    num = toInteger . length
    remainingArgs = drop (length params) args
    bindVarArgs arg env = case arg of
      Just argName -> bindVars env [(argName, list remainingArgs)]
      Nothing -> env
