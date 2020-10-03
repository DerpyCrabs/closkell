module Compile.MacroSystem (macroSystem) where

import Control.Monad.Except (lift, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Env
import Data.Error
import Data.Value
import Eval
import Eval.Primitive
import Types

macroSystem :: LispVal -> IOThrowsError LispVal
macroSystem val =
  let state = MacroExpansionState 0
      env = primitiveBindings
      z = lvSetEnv env . lvFromAST $ val
   in snd <$> macroExpand state z

macroExpand :: MacroExpansionState -> LVZipper -> IOThrowsError (MacroExpansionState, LispVal)
macroExpand state z@(_, List _ [Atom _ "gensym"], _) =
  return (state {gensymCounter = gensymCounter state + 1}, atom (show $ gensymCounter state))
macroExpand state z@(_, List _ [Atom _ "gensym", String prefix], _) =
  return (state {gensymCounter = gensymCounter state + 1}, atom ((prefix ++) $ show $ gensymCounter state))
macroExpand state z@(env, List _ (Atom _ "let" : bindsAndExpr), _) = do
  let binds = init bindsAndExpr
  let expr = last bindsAndExpr
  let newEnv = evalMacro newEnv <$> bindVars env (vars binds)
  (state, z) <- macroExpand state (lvSet expr . lvSetEnv newEnv $ z)
  return (state, makeLet (varsWithoutMacros binds) z)
  where
    matchVar (List _ [Atom _ name, var]) = (name, var)
    evalMacro env (name, List _ [Atom _ "macro", body]) = (name, Macro body env)
    evalMacro env other = other
    vars binds = matchVar <$> binds
    isMacro (name, List _ (Atom _ "macro" : _)) = True
    isMacro _ = False
    varsWithoutMacros binds = (not . isMacro) `filter` vars binds
macroExpand state z@(env, val@(List _ (Atom _ function : args)), _) = do
  let var = getVar env function
  case var of
    Right (Macro body macroEnv) ->
      evalMacro state (env ++ macroEnv) body args z
    _ -> do
      expanded <- mapM (macroExpand state) (lvSetEnv env . lvFromAST <$> (atom function : args))
      return (state, list $ snd <$> expanded)
macroExpand state z@(env, val@(List _ args), _) = do
  expanded <- mapM (macroExpand state) (lvSetEnv env . lvFromAST <$> args)
  return (state, list $ snd <$> expanded)
macroExpand state (_, val, _) = return (state, val)

evalMacro :: MacroExpansionState -> Env -> LispVal -> [LispVal] -> LVZipper -> IOThrowsError (MacroExpansionState, LispVal)
evalMacro state env body args z = evalMacro' [id] state (lvSet body . lvSetEnv (("body", func "quote" [list args]) : env) $ z)
  where
    evalMacro' (step : steps) state z = do
      (newZ, newSteps) <- stepEval (step z)
      (newState, newVal) <- macroExpand state newZ
      evalMacro' (newSteps ++ steps) newState (lvSet newVal newZ)
    evalMacro' [] state z = return (state, lvToAST z)
