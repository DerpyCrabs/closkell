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
import Data.AST (fromValue)
import Data.Env
import Data.Error
import Data.Maybe (isJust, isNothing)
import Data.Value
import System.Command
import Types

eval :: Env -> Value -> IOThrowsError Value
eval env val = do
  steps <- lift $ evalSteps env val
  case last steps of
    Left err -> throwError err
    Right zipper -> return $ vzToAST zipper

evalSteps :: Env -> Value -> IO [ThrowsError ValueZipper]
evalSteps env val = evalSteps' [id] [Right zipper] zipper
  where
    zipper = vzSetEnv env . vzFromAST $ val
    evalSteps' :: [ValueZipperTurn] -> [ThrowsError ValueZipper] -> ValueZipper -> IO [ThrowsError ValueZipper]
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

stepEval :: ValueZipper -> IOThrowsError (ValueZipper, [ValueZipperTurn])
stepEval z@(_, val, _) | isNormalForm val = return (z, [])
stepEval z@(_, val@(List _ args), _) =
  let correctedPath = correctQuoteEvalPath (Call args)
   in case length correctedPath of
        0 -> return (vzSet val z, [])
        _ -> return (vzSet (func "evaluating-unquote-list" [Call args]) z, correctedPath)
stepEval z@(_, val@(Map args), _) =
  let correctedPath = correctQuoteEvalPath (Call args)
   in case length correctedPath of
        0 -> return (vzSet val z, [])
        _ -> return (vzSet (func "evaluating-unquote-map" [Call args]) z, correctedPath)
stepEval z@(env, fn@(Call (Atom _ "fn" : _)), _) =
  return (vzSet (createFn env fn) z, [])
stepEval z@(env, Atom _ name, _) = do
  var <- liftThrows $ getVar env name
  return $ case var of
    Call _ -> (vzSet var z, [id])
    Atom _ _ -> (vzSet var z, [id])
    _ -> (vzSet var z, [])
stepEval z@(env, Call (Atom _ "let" : bindsAndExpr), _) = do
  let binds = init bindsAndExpr
  let expr = last bindsAndExpr
  if null binds
    then return (vzSet expr z, [id])
    else do
      let bindsEvalPath = replicate (length binds) vzRight
      let newEnv = bindVars (vars binds) env
      let unquotedBinds = unquoteBind newEnv <$> binds
      let filteredEvalPath = fst <$> mergePath (zip bindsEvalPath unquotedBinds)
      if null filteredEvalPath
        then do
          let unquotedEnv = bindVars (vars unquotedBinds) env
          return (vzSetEnv unquotedEnv . vzSet expr $ z, [id])
        else return (vzSetEnv newEnv . vzSet (func "evaluating-let" (unquotedBinds ++ [expr])) $ z, head filteredEvalPath . vzDown : tail filteredEvalPath ++ [vzUp])
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
  return (vzSetEnv newEnv . vzSet expr $ z, [id])
  where
    matchVars (List _ [Atom _ name, var]) = (name, var)
    vars binds = matchVars <$> binds
stepEval z@(_, Call [Atom _ "if", Bool True, conseq, _], _) =
  return (vzSet conseq z, [id])
stepEval z@(_, Call [Atom _ "if", Bool False, _, alt], _) =
  return (vzSet alt z, [id])
stepEval z@(_, Call [Atom _ "if", _, _, _], _) =
  return (z, [vzRight . vzDown, vzUp])
stepEval z@(_, Call [Atom _ "apply", f, args], _)
  | isNormalForm f =
    return (vzSet (func "evaluating-apply" [f, args]) z, [vzRight . vzRight . vzDown, vzUp])
stepEval z@(_, Call [Atom _ "apply", f, args], _) =
  return (vzSet (func "evaluating-apply" [f, args]) z, [vzRight . vzDown, vzRight, vzUp])
stepEval z@(_, Call [Atom _ "evaluating-apply", f, List _ args], _) =
  return (vzSet (Call (f : args)) z, [id])
stepEval z@(_, Call [Atom _ "quote", val], _) =
  let correctedPath = correctQuoteEvalPath val
   in case length correctedPath of
        0 -> return (vzSet val z, [])
        _ -> return (vzSet (func "evaluating-unquote" [val]) z, correctedPath)
stepEval z@(_, Call [Atom _ "evaluating-unquote", val], _) =
  return (vzSet (performUnquoteSplicing val) z, [])
stepEval z@(_, Call [Atom _ "evaluating-unquote-list", val], _) =
  return (vzSet ((\(Call args) -> List Nothing args) $ performUnquoteSplicing val) z, [])
stepEval z@(_, Call [Atom _ "evaluating-unquote-map", val], _) =
  return (vzSet ((\(Call args) -> Map args) $ performUnquoteSplicing val) z, [])
stepEval z@(_, Call [Atom _ "unquote", val], _)
  | isNormalForm val =
    return (vzSet val z, [])
stepEval z@(_, Call [Atom _ "unquote", val], _) =
  return (vzSet val z, [id])
stepEval z@(_, Call [Atom _ "unquote-splicing", val], _) =
  return (vzSet (func "evaluating-unquote-splicing" [val]) z, [vzRight . vzDown, vzUp])
stepEval z@(_, Call [Atom _ "evaluating-unquote-splicing", List _ _], _) =
  return (z, [])
stepEval z@(_, Call [Atom _ "evaluating-unquote-splicing", Map _], _) =
  return (z, [])
stepEval z@(_, Call (function : args), _) =
  case function of
    PrimitiveFunc _ f -> do
      res <- liftThrows $ f $ unsplice args
      return (vzSet res z, [])
    IOFunc _ f -> do
      res <- f $ unsplice args
      return (vzSet res z, [])
    f@Func {} -> do
      res <- liftThrows $ applyFunc f z $ unsplice args
      return (res, [id])
    _ ->
      return (z, evalArgsPath)
  where
    unsplice args = (\(Call args) -> args) $ performUnquoteSplicing $ Call args
    evalArgsPath = reverse $ foldl foldFunc [vzDown] (zip [1 ..] (function : args))
      where
        foldFunc (t : ts) (i, arg) | i /= length args + 1 = if isNormalForm arg then vzRight . t : ts else vzRight : t : ts
        foldFunc (t : ts) (_, arg) = if isNormalForm arg then vzUp . t : ts else vzUp : t : ts
stepEval (_, badForm, _) = throwError $ BadSpecialForm "Unrecognized special form" badForm

isNormalForm :: Value -> Bool
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
isNormalForm (Map xs) | all isNormalForm xs = True
isNormalForm _ = False

applyFunc :: Value -> ValueZipper -> [Value] -> ThrowsError ValueZipper
applyFunc (Func params varargs body closure) z args =
  do
    let env = bindVarArgs varargs . bindVars (zip params args) $ closure
    return . vzSet body . vzSetEnv env $ z
  where
    remainingArgs = drop (length params) args
    bindVarArgs arg env = case arg of
      Just argName -> bindVars [(argName, list remainingArgs)] env
      Nothing -> env

quoteEvalPath :: Value -> [ValueZipperTurn]
quoteEvalPath val =
  case quoteEvalPath' val of
    Just path -> head path : composeUpDown (tail path)
    Nothing -> []
  where
    callPath args =
      let maybeUnquotePaths = quoteEvalPath' <$> args
          argPaths = scanl (\path _ -> vzRight . path) vzDown $ tail args
          unquotePaths =
            (\(Just [d, u], t) -> [d . t, vzUp . u])
              <$> filter (\(p, _) -> isJust p) (zip maybeUnquotePaths argPaths)
       in case length unquotePaths of
            0 -> Nothing
            _ -> Just $ concat unquotePaths
    quoteEvalPath' :: Value -> Maybe [ValueZipperTurn]
    quoteEvalPath' (Call [Atom _ "unquote", val]) = Just [id, id]
    quoteEvalPath' (Call [Atom _ "unquote-splicing", val]) = Just [id, id]
    quoteEvalPath' (Call args) = callPath args
    quoteEvalPath' _ = Nothing
    composeUpDown (x : y : xs) = (y . x) : composeUpDown xs
    composeUpDown [x] = [x]

performUnquoteSplicing :: Value -> Value
performUnquoteSplicing = performUnquoteSplicing'
  where
    performUnquoteSplicing' (Call vals) = Call (concat $ performUnquoteSplicing'' <$> vals)
    performUnquoteSplicing'' :: Value -> [Value]
    performUnquoteSplicing'' (Call [Atom _ "evaluating-unquote-splicing", List _ vals]) = vals
    performUnquoteSplicing'' (Call [Atom _ "evaluating-unquote-splicing", Map vals]) = vals
    performUnquoteSplicing'' v@(Call _) = [performUnquoteSplicing' v]
    performUnquoteSplicing'' other = [other]

evalUsingNode :: Value -> IOThrowsError String
evalUsingNode val = do
  let astVal = fromValue val
  case astVal of
    Left err -> throwError err
    Right astVal -> do
      let jsSource = emitJS astVal
      jsOptimizedSource <- liftIO $ closureCompilerPass jsSource
      Stdout out <- liftIO $ command [Stdin jsOptimizedSource] "node" []
      return out

createFn :: Env -> Value -> Value
createFn env (Call [Atom _ "fn", List _ params, body]) =
  makeNormalFunc env params body
createFn env (Call [Atom _ "fn", DottedList _ params varargs, body]) =
  makeVarArgs varargs env params body

correctQuoteEvalPath :: Value -> [ValueZipperTurn]
correctQuoteEvalPath val =
  let path = quoteEvalPath val
      correctPath path@(_ : _ : _) =
        let first = head path . vzRight . vzDown
            lst = vzUp . last path
         in [first] ++ init (tail path) ++ [lst]
      correctPath p = p
   in correctPath path
