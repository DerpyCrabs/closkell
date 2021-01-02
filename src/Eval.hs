module Eval
  ( stepEval,
    evalSteps,
    eval,
    evalUsingNode,
  )
where

import Compile.ClosureCompilerPass (closureCompilerPass)
import Compile.EmitJS (emitJS)
import Control.Monad.Except
import Data.Env
import Data.Error
import Data.Maybe (isJust, isNothing)
import Data.Value
import System.Command
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
    evalSteps' (step : steps) acc z@(_, val, _) = do
      res <- runExceptT $ stepEval (step z)
      case res of
        Right (z, newSteps) ->
          let nextSteps = newSteps ++ steps
           in if not $ null nextSteps
                then evalSteps' nextSteps (acc ++ [Right (head (newSteps ++ steps) z)]) z
                else evalSteps' [] (acc ++ [Right z]) z
        Left err -> return (acc ++ [Left err])
    evalSteps' [] acc _ = return acc

stepEval :: LVZipper -> IOThrowsError (LVZipper, [LVZipperTurn])
stepEval z@(_, val, _) | isNormalForm val = return (z, [])
stepEval z@(_, val@(List _ args), _) =
  let path = quoteEvalPath (Call args)
      correctPath path@(_ : _ : _) =
        let first = lvRight . lvDown . head path
            lst = lvUp . last path
         in [first] ++ init (tail path) ++ [lst]
      correctPath p = p
      correctedPath = correctPath path
   in case length correctedPath of
        0 -> return (lvSet val z, [])
        _ -> return (lvSet (func "evaluating-unquote-list" [Call args]) z, correctPath path)
stepEval z@(env, fn@(Call (Atom _ "fn" : _)), _) =
  return (lvSet (createFn env fn) z, [])
stepEval z@(env, Atom _ name, _) = do
  var <- liftThrows $ getVar env name
  return $ case var of
    Call _ -> (lvSet var z, [id])
    Atom _ _ -> (lvSet var z, [id])
    _ -> (lvSet var z, [])
stepEval z@(env, Call (Atom _ "let" : bindsAndExpr), _) = do
  let binds = init bindsAndExpr
  let expr = last bindsAndExpr
  if null binds
    then return (lvSet expr z, [id])
    else do
      let bindsEvalPath = replicate (length binds) lvRight
      let newEnv = bindVars (vars binds) env
      let unquotedBinds = unquoteBind newEnv <$> binds
      let filteredEvalPath = fst <$> mergePath (zip bindsEvalPath unquotedBinds)
      if null filteredEvalPath
        then do
          let unquotedEnv = bindVars (vars unquotedBinds) env
          return (lvSetEnv unquotedEnv . lvSet expr $ z, [id])
        else return (lvSetEnv newEnv . lvSet (func "evaluating-let" (unquotedBinds ++ [expr])) $ z, head filteredEvalPath . lvDown : tail filteredEvalPath ++ [lvUp])
  where
    matchVars (List _ [Atom _ name, var]) = (name, var)
    vars binds = matchVars <$> binds
    unquoteBind _ bind@(List _ [name, var]) | isNormalForm var = bind
    unquoteBind env (List _ [name, fn@(Call (Atom _ "fn" : _))]) = list [name, createFn env fn]
    unquoteBind _ (List _ [name, var]) = list [name, func "unquote" [var]]
    mergePath ((p1, List _ [name, var]) : (p2, bind2) : next) | isNormalForm var = mergePath ((p2 . p1, bind2) : next)
    mergePath [(p1, List _ [name, var])] | isNormalForm var = []
    mergePath ((p1, bind1@(List _ [name, var])) : (p2, bind2) : next) = (p1, bind1) : mergePath ((p2, bind2) : next)
    mergePath rest = rest
stepEval z@(env, Call (Atom _ "evaluating-let" : bindsAndExpr), _) = do
  let binds = init bindsAndExpr
  let expr = last bindsAndExpr
  let newBinds = bindVars (vars binds) env
  let newEnv = bindVars newBinds env
  return (lvSetEnv newEnv . lvSet expr $ z, [id])
  where
    matchVars (List _ [Atom _ name, var]) = (name, var)
    vars binds = matchVars <$> binds
stepEval z@(_, Call [Atom _ "if", Bool True, conseq, _], _) =
  return (lvSet conseq z, [id])
stepEval z@(_, Call [Atom _ "if", Bool False, _, alt], _) =
  return (lvSet alt z, [id])
stepEval z@(_, Call [Atom _ "if", _, _, _], _) =
  return (z, [lvRight . lvDown, lvUp])
stepEval z@(_, Call [Atom _ "apply", f, args], _)
  | isNormalForm f =
    return (lvSet (func "evaluating-apply" [f, args]) z, [lvRight . lvRight . lvDown, lvUp])
stepEval z@(_, Call [Atom _ "apply", f, args], _) =
  return (lvSet (func "evaluating-apply" [f, args]) z, [lvRight . lvDown, lvRight, lvUp])
stepEval z@(_, Call [Atom _ "evaluating-apply", f, List _ args], _) =
  return (lvSet (Call (f : args)) z, [id])
stepEval z@(_, Call [Atom _ "quote", val], _) =
  let path = quoteEvalPath val
      correctPath path@(_ : _ : _) =
        let first = lvRight . lvDown . head path
            lst = lvUp . last path
         in [first] ++ init (tail path) ++ [lst]
      correctPath p = p
      correctedPath = correctPath path
   in case length correctedPath of
        0 -> return (lvSet val z, [])
        _ -> return (lvSet (func "evaluating-unquote" [val]) z, correctPath path)
stepEval z@(_, Call [Atom _ "evaluating-unquote", val], _) =
  return (lvSet (performUnquoteSplicing val) z, [])
stepEval z@(_, Call [Atom _ "evaluating-unquote-list", val], _) =
  return (lvSet ((\(Call args) -> List Nothing args) $ performUnquoteSplicing val) z, [])
stepEval z@(_, Call [Atom _ "unquote", val], _)
  | isNormalForm val =
    return (lvSet val z, [])
stepEval z@(_, Call [Atom _ "unquote", val], _) =
  return (lvSet val z, [id])
stepEval z@(_, Call [Atom _ "unquote-splicing", val], _) =
  return (lvSet (func "evaluating-unquote-splicing" [val]) z, [lvRight . lvDown, lvUp])
stepEval z@(_, Call [Atom _ "evaluating-unquote-splicing", List _ _], _) =
  return (z, [])
stepEval z@(_, Call (function : args), _) =
  case function of
    PrimitiveFunc _ f -> do
      res <- liftThrows $ f $ unsplice args
      return (lvSet res z, [])
    IOFunc _ f -> do
      res <- f $ unsplice args
      return (lvSet res z, [])
    f@Func {} -> do
      res <- liftThrows $ applyFunc f z $ unsplice args
      return (res, [id])
    _ ->
      return (z, evalArgsPath)
  where
    unsplice args = (\(Call args) -> args) $ performUnquoteSplicing $ Call args
    evalArgsPath = reverse $ foldl foldFunc [lvDown] (zip [1 ..] (function : args))
      where
        foldFunc (t : ts) (i, arg) | i /= length args + 1 = if isNormalForm arg then lvRight . t : ts else lvRight : t : ts
        foldFunc (t : ts) (_, arg) = if isNormalForm arg then lvUp . t : ts else lvUp : t : ts
stepEval (_, badForm, _) = throwError $ BadSpecialForm "Unrecognized special form" badForm

isNormalForm :: LispVal -> Bool
isNormalForm Unit = True
isNormalForm (String _) = True
isNormalForm (Character _) = True
isNormalForm (Integer _) = True
isNormalForm (Float _) = True
isNormalForm (Bool _) = True
isNormalForm (PrimitiveFunc _ _) = True
isNormalForm (IOFunc _ _) = True
isNormalForm Func {} = True
isNormalForm (Type _) = True
isNormalForm (List _ xs) | all isNormalForm xs = True
isNormalForm (Map xs) | all (\(key, value) -> isNormalForm key && isNormalForm value) xs = True
isNormalForm _ = False

applyFunc :: LispVal -> LVZipper -> [LispVal] -> ThrowsError LVZipper
applyFunc (Func params varargs body closure) z args =
  do
    let env = bindVarArgs varargs . bindVars (zip params args) $ closure
    return . lvSet body . lvSetEnv env $ z
  where
    remainingArgs = drop (length params) args
    bindVarArgs arg env = case arg of
      Just argName -> bindVars [(argName, list remainingArgs)] env
      Nothing -> env

quoteEvalPath :: LispVal -> [LVZipperTurn]
quoteEvalPath val = case quoteEvalPath' val of
  Just path -> head path : composeUpDown (tail path)
  Nothing -> []
  where
    callPath args =
      let maybeUnquotePaths = quoteEvalPath' <$> args
          argPaths = scanl (\path _ -> lvRight . path) lvDown $ tail args
          unquotePaths =
            (\(Just [d, u], t) -> [d . t, lvUp . u])
              <$> filter (\(p, _) -> isJust p) (zip maybeUnquotePaths argPaths)
       in case length unquotePaths of
            0 -> Nothing
            _ -> Just $ concat unquotePaths
    quoteEvalPath' :: LispVal -> Maybe [LVZipperTurn]
    quoteEvalPath' (Call [Atom _ "unquote", val]) = Just [id, id]
    quoteEvalPath' (Call [Atom _ "unquote-splicing", val]) = Just [id, id]
    quoteEvalPath' (Call args) = callPath args
    quoteEvalPath' _ = Nothing
    composeUpDown (x : y : xs) = (y . x) : composeUpDown xs
    composeUpDown [x] = [x]

performUnquoteSplicing :: LispVal -> LispVal
performUnquoteSplicing = performUnquoteSplicing'
  where
    performUnquoteSplicing' (Call vals) = Call (concat $ performUnquoteSplicing'' <$> vals)
    performUnquoteSplicing'' :: LispVal -> [LispVal]
    performUnquoteSplicing'' (Call [Atom _ "evaluating-unquote-splicing", List _ vals]) = vals
    performUnquoteSplicing'' v@(Call _) = [performUnquoteSplicing' v]
    performUnquoteSplicing'' other = [other]

evalUsingNode :: LispVal -> IOThrowsError String
evalUsingNode val = do
  let jsSource = emitJS val
  jsOptimizedSource <- liftIO $ closureCompilerPass jsSource
  Stdout out <- liftIO $ command [Stdin jsOptimizedSource] "node" []
  return out

createFn :: Env -> LispVal -> LispVal
createFn env (Call [Atom _ "fn", List _ params, body]) =
  makeNormalFunc env params body
createFn env (Call [Atom _ "fn", DottedList _ params varargs, body]) =
  makeVarArgs varargs env params body
