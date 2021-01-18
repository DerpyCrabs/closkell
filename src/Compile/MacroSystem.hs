module Compile.MacroSystem (macroSystem) where

import Data.Env
import Data.Value
import Eval
import Eval.Primitive
import Types

macroSystem :: Value -> IOThrowsError Value
macroSystem val =
  let state = MacroExpansionState 0
      env = primitiveBindings
      z = vzSetEnv env . vzFromAST $ val
   in snd <$> macroExpand state z

macroExpand :: MacroExpansionState -> ValueZipper -> IOThrowsError (MacroExpansionState, Value)
macroExpand state (_, Call [Atom _ "gensym"], _) =
  return (state {gensymCounter = gensymCounter state + 1}, atom (show $ gensymCounter state))
macroExpand state (_, Call [Atom _ "gensym", String prefix], _) =
  return (state {gensymCounter = gensymCounter state + 1}, atom ((prefix ++) $ show $ gensymCounter state))
macroExpand state z@(env, Call (Atom _ "let" : bindsAndExpr), _) = do
  let binds = init bindsAndExpr
  let expr = last bindsAndExpr
  let newEnv = evalMacro newEnv <$> bindVars (vars binds) env
  (state, z) <- macroExpand state (vzSet expr . vzSetEnv newEnv $ z)
  return (state, makeLet (varsWithoutMacros binds) z)
  where
    matchVar (List _ [Atom _ name, var]) = (name, var)
    evalMacro env (name, Call [Atom _ "macro", body]) = (name, Macro body env)
    evalMacro _ other = other
    vars binds = matchVar <$> binds
    isMacro (_, Call (Atom _ "macro" : _)) = True
    isMacro _ = False
    varsWithoutMacros binds = (not . isMacro) `filter` vars binds
macroExpand state z@(env, Call (Atom _ function : args), _) = do
  let var = getVar env function
  case var of
    Right (Macro body macroEnv) ->
      evalMacro state (env ++ macroEnv) body args z
    _ -> do
      expanded <- mapM (macroExpand state) (vzSetEnv env . vzFromAST <$> (atom function : args))
      return (state, Call $ snd <$> expanded)
macroExpand state (env, Call args, _) = do
  expanded <- mapM (macroExpand state) (vzSetEnv env . vzFromAST <$> args)
  return (state, Call $ snd <$> expanded)
macroExpand state (_, val, _) = return (state, val)

evalMacro :: MacroExpansionState -> Env -> Value -> [Value] -> ValueZipper -> IOThrowsError (MacroExpansionState, Value)
evalMacro state env body args z = evalMacro' [id] state (vzSet body . vzSetEnv (("body", list args) : env) $ z)
  where
    evalMacro' steps state z@(_, Call [Atom _ "quote", val], _) =
      let correctedPath = correctQuoteEvalPath val
       in case length correctedPath of
            0 -> return (state, val)
            _ -> evalMacro' (tail correctedPath ++ steps) state (head correctedPath $ vzSet (func "evaluating-unquote" [val]) z)
    evalMacro' steps state z@(_, Call [Atom _ "unquote", val], _) =
      evalMacro' steps state (vzSet val z)
    evalMacro' steps state z@(_, Call [Atom _ "evaluating-unquote", val], _) =
      return (state, val)
    evalMacro' steps state z = do
      (newZ, newSteps) <- stepEval z
      (newState, newVal) <- macroExpand state newZ
      let nextSteps = newSteps ++ steps
      case nextSteps of
        (step : nextSteps) -> evalMacro' nextSteps newState $ step (vzSet newVal newZ)
        [] -> return (newState, newVal)
