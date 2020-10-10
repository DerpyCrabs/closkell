module Eval
  ( stepEval,
    evalSteps,
    eval,
  )
where

import Control.Monad.Except
import Data.Env
import Data.Error
import Data.Maybe (isJust, isNothing)
import Data.Value
import Types

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val = do
  steps <- lift $ evalSteps env val
  case last steps of
    Left err -> throwError err
    Right zipper -> return $ lvToAST zipper

evalSteps :: Env -> LispVal -> IO [ThrowsError LVZipper]
evalSteps env val = evalSteps' [id] [Right zipper] zipper
  where
    zipper = lvSetEnv env . lvFromAST $ val
    evalSteps' :: [LVZipperTurn] -> [ThrowsError LVZipper] -> LVZipper -> IO [ThrowsError LVZipper]
    evalSteps' (step : steps) acc z = do
      res <- runExceptT $ stepEval (step z)
      case res of
        Right (z, newSteps) ->
          if length (newSteps ++ steps) /= 0
            then evalSteps' (newSteps ++ steps) (acc ++ [Right ((head (newSteps ++ steps)) z)]) z
            else evalSteps' (newSteps ++ steps) (acc ++ [Right z]) z
        Left err -> return (acc ++ [Left err])
    evalSteps' [] acc _ = return acc

stepEval :: LVZipper -> IOThrowsError (LVZipper, [LVZipperTurn])
stepEval z@(_, String _, _) = return (z, [])
stepEval z@(_, Character _, _) = return (z, [])
stepEval z@(_, Integer _, _) = return (z, [])
stepEval z@(_, Float _, _) = return (z, [])
stepEval z@(_, Bool _, _) = return (z, [])
stepEval z@(_, PrimitiveFunc _ _, _) = return (z, [])
stepEval z@(_, IOFunc _ _, _) = return (z, [])
stepEval z@(_, Func {}, _) = return (z, [])
stepEval z@(env, List _ [Atom _ "lambda", (List _ [Atom _ "quote", List _ []]), body], _) =
  return (lvSet (makeNormalFunc env [] body) z, [])
stepEval z@(env, List _ [Atom _ "lambda", List _ params, body], _) =
  return (lvSet (makeNormalFunc env params body) z, [])
stepEval z@(env, List _ [Atom _ "lambda", DottedList _ params varargs, body], _) =
  return (lvSet (makeVarArgs varargs env params body) z, [])
stepEval z@(env, Atom _ name, _) = do
  var <- liftThrows $ getVar env name
  return (lvSet var $ z, [id])
stepEval z@(env, List _ (Atom _ "let" : bindsAndExpr), _) = do
  let binds = init bindsAndExpr
  let expr = last bindsAndExpr
  let newEnv = bindVars env (vars binds)
  return (lvSetEnv newEnv . lvSet expr $ z, [id])
  where
    matchVars (List _ [Atom _ name, var]) = (name, var)
    vars binds = matchVars <$> binds
stepEval z@(_, List _ [Atom _ "if", Bool True, conseq, _], _) =
  return (lvSet conseq z, [id])
stepEval z@(_, List _ [Atom _ "if", Bool False, _, alt], _) =
  return (lvSet alt z, [id])
stepEval z@(_, List _ [Atom _ "if", _, _, _], _) =
  return (z, [lvRight . lvDown, lvUp])
stepEval z@(_, List _ [Atom _ "apply", func, List _ args], _) =
  return (lvSet (list ([func] ++ args)) z, [id])
stepEval z@(_, List _ [Atom _ "quote", val], _) =
  let path = quoteEvalPath val
      correctPath path@(_ : _ : _) =
        let first = lvRight . lvDown . (head path)
            lst = lvUp . (last path)
         in [first] ++ (init $ tail path) ++ [lst]
      correctPath p = p
      correctedPath = correctPath path
   in case length correctedPath of
        0 -> return (lvSet val z, [])
        _ -> return (lvSet (func "evaluating-unquote" [val]) z, correctPath path)
stepEval z@(_, List _ [Atom _ "evaluating-unquote", val], _) =
  return (lvSet (performUnquoteSplicing val) z, [])
stepEval z@(_, List _ [Atom _ "unquote", val], _) =
  return (lvSet val z, [id])
stepEval z@(_, List _ [Atom _ "unquote-splicing", val], _) =
  return (lvSet (func "evaluating-unquote-splicing" [val]) z, [lvRight . lvDown, lvUp])
stepEval z@(_, List _ [Atom _ "evaluating-unquote-splicing", (List _ _)], _) =
  return (z, [])
stepEval z@(_, List _ (function : args), _) =
  case function of
    PrimitiveFunc _ f -> do
      res <- liftThrows $ f args
      return (lvSet res z, [])
    IOFunc _ f -> do
      res <- f args
      return (lvSet res z, [])
    f@(Func {}) -> do
      res <- liftThrows $ applyFunc f z args
      return (res, [id])
    _ ->
      return (z, [lvDown] ++ (replicate (length args) lvRight) ++ [lvUp])
stepEval (_, badForm, _) = throwError $ BadSpecialForm "Unrecognized special form" badForm

applyFunc :: LispVal -> LVZipper -> [LispVal] -> ThrowsError LVZipper
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
      Just argName -> bindVars env [(argName, func "quote" [list remainingArgs])]
      Nothing -> env

quoteEvalPath :: LispVal -> [LVZipperTurn]
quoteEvalPath val = case quoteEvalPath' val of
  Just path -> (head path : composeUpDown (tail path))
  Nothing -> []
  where
    quoteEvalPath' :: LispVal -> Maybe [LVZipperTurn]
    quoteEvalPath' (List _ [Atom _ "unquote", _]) = Just [id, id]
    quoteEvalPath' (List _ [Atom _ "unquote-splicing", _]) = Just [id, id]
    quoteEvalPath' (List _ args) =
      let maybeUnquotePaths = quoteEvalPath' <$> args
          argPaths = scanl (\path _ -> lvRight . path) lvDown $ tail args
          unquotePaths =
            (\(Just [d, u], t) -> [d . t, lvUp . u])
              <$> (filter (\(p, _) -> isJust p) $ zip maybeUnquotePaths argPaths)
       in case length unquotePaths of
            0 -> Nothing
            _ -> Just $ concat unquotePaths
    quoteEvalPath' _ = Nothing
    composeUpDown (x : y : xs) = [y . x] ++ composeUpDown xs
    composeUpDown [x] = [x]

performUnquoteSplicing :: LispVal -> LispVal
performUnquoteSplicing (List _ vals) = list (concat $ performUnquoteSplicing' <$> vals)
  where
    performUnquoteSplicing' :: LispVal -> [LispVal]
    performUnquoteSplicing' (List _ [Atom _ "evaluating-unquote-splicing", List _ vals]) = vals
    performUnquoteSplicing' v@(List _ _) = [performUnquoteSplicing v]
    performUnquoteSplicing' other = [other]
