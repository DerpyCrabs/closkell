module Compile.MacroSystem (macroSystem) where

import Control.Monad.Except
import Data.Env
import Data.State
import Data.Value
import Eval
import Eval.Primitive
import Types

macroSystem (val : _) = do
  env <- liftIO primitiveBindings
  state <- liftIO nullState
  (: []) <$> macroSystem' state env val

macroSystem' state env (List _ (Atom _ "let" : bindsAndExpr)) = do
  let binds = init bindsAndExpr
  let expr = last bindsAndExpr
  newEnv <- liftIO $ bindVars env (vars binds)
  evaledMacros <- mapM (evalMacro newEnv) (vars binds)
  mapM_ (uncurry $ setVar newEnv) evaledMacros
  makeLet (varsWithoutMacros binds) <$> macroSystem' state newEnv expr
  where
    matchVar (List _ [Atom _ name, var]) = (name, var)
    evalMacro env (name, List _ (Atom _ "macro" : body)) = return (name, Macro body env)
    evalMacro env other = return other
    vars binds = matchVar <$> binds
    isMacro (name, List _ (Atom _ "macro" : _)) = True
    isMacro _ = False
    varsWithoutMacros binds = (not . isMacro) `filter` vars binds
macroSystem' state env (List _ ((Atom _ function) : args)) = do
  var <- getVar env function
  args <- mapM (macroSystem' state env) args
  case var of
    (Macro _ _) -> apply state var args
    _ -> return $ list (atom function : args)
macroSystem' state env value = return value
