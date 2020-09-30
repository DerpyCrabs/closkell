module Eval
  ( stepEval,
    evalSteps,
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
eval env val = do
  steps <- lift $ evalSteps env val
  case last $ steps of
    Left err -> throwError err
    Right zipper -> return $ lvToAST zipper

evalSteps :: Env -> LispVal -> IO [ThrowsError LispValZipper]
evalSteps env val = evalSteps' [Right zipper] zipper
  where
    zipper = lvSetEnv env . lvFromAST $ val
    evalSteps' :: [ThrowsError LispValZipper] -> LispValZipper -> IO [ThrowsError LispValZipper]
    evalSteps' acc z = do
      nextStep <- runExceptT $ stepEval z
      case nextStep of
        Right z@(_, Just _, _) -> evalSteps' (acc ++ [nextStep]) z
        Right (_, Nothing, _) -> return acc
        Left err -> return (acc ++ [nextStep])

stepEval :: LispValZipper -> IOThrowsError LispValZipper
stepEval z@(_, Just (String _), _) = return $ lvNext z
stepEval z@(_, Just (Character _), _) = return $ lvNext z
stepEval z@(_, Just (Integer _), _) = return $ lvNext z
stepEval z@(_, Just (Float _), _) = return $ lvNext z
stepEval z@(_, Just (Bool _), _) = return $ lvNext z
stepEval z@(_, Just (Atom _ id), _)
  | id `elem` ["quote"] =
    return . lvUp $ z
stepEval z@(env, Just (Atom _ id), _) = do
  var <- liftThrows $ getVar env id
  return . lvSet var $ z
stepEval z@(env, Just (List _ (Atom _ "let" : bindsAndExpr)), _) = do
  let binds = init bindsAndExpr
  let expr = last bindsAndExpr
  let newEnv = bindVars env (vars binds)
  return . lvSetEnv newEnv . lvSet expr $ z
  where
    matchVars (List _ [Atom _ name, var]) = (name, var)
    vars binds = matchVars <$> binds
stepEval z@(_, Just (List _ [Atom _ "if", conseq, alt, Bool True]), _) =
  return . lvSet conseq $ z
stepEval z@(_, Just (List _ [Atom _ "if", conseq, alt, Bool False]), _) =
  return . lvSet alt $ z
stepEval z@(_, Just (List _ [Atom _ "if", pred, conseq, alt]), _) =
  return . lvRight . lvRight . lvDown . lvSet (list [atom "if", conseq, alt, pred]) $ z
stepEval z@(_, Just (List _ [Atom _ "apply", func, List _ args]), _) = do
  return . lvSet (list ([func] ++ args)) $ z
stepEval z@(_, Just (List _ [Atom _ "quote", val]), _) =
  return . lvNext . lvSet val $ z
stepEval z@(env, Just (List _ [Atom _ "lambda", (List _ [Atom _ "quote", List _ []]), body]), _) =
  return . lvNext . lvSet (makeNormalFunc env [] body) $ z
stepEval z@(env, Just (List _ [Atom _ "lambda", List _ params, body]), _) =
  return . lvNext . lvSet (makeNormalFunc env params body) $ z
stepEval z@(env, Just (List _ [Atom _ "lambda", DottedList _ params varargs, body]), _) =
  return . lvNext . lvSet (makeVarArgs varargs env params body) $ z
stepEval z@(_, Just (List pos (function : args)), _) =
  case function of
    PrimitiveFunc _ f -> do
      res <- liftThrows $ f args
      return . lvSet res $ z
    IOFunc _ f -> do
      res <- f args
      return . lvSet res $ z
    f@(Func params varargs body closure) -> liftThrows $ applyFunc f z args
    _ -> stepEval . lvDown $ z
stepEval z@(_, Just (PrimitiveFunc _ _), _) = return . lvNext $ z
stepEval z@(_, Just (IOFunc _ _), _) = return . lvNext $ z
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
