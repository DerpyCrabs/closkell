module Compile.MacroSystem (macroSystem) where

import Control.Monad.Except (lift, runExceptT)
import Data.Error
import Data.Value
import Eval.Primitive
import Types

macroSystem :: LispVal -> IOThrowsError LispVal
macroSystem val =
  let state = MacroExpansionState 0
      env = primitiveBindings
   in do
        steps <- lift $ macroExpandSteps state env val
        case last steps of
          Left err -> throwError err
          Right zipper -> return $ lvToAST zipper

macroExpandSteps :: MacroExpansionState -> Env -> LispVal -> IO [ThrowsError LVZipper]
macroExpandSteps state env val = macroExpandSteps' state [id] [Right zipper] zipper
  where
    zipper = lvSetEnv env . lvFromAST $ val
    macroExpandSteps' :: MacroExpansionState -> [LVZipperTurn] -> [ThrowsError LVZipper] -> LVZipper -> IO [ThrowsError LVZipper]
    macroExpandSteps' state (step : steps) acc z = do
      res <- runExceptT $ stepMacroExpand state (step z)
      case res of
        Right (newState, z, newSteps) -> macroExpandSteps' newState (newSteps ++ steps) (acc ++ [Right z]) z
        Left err -> return (acc ++ [Left err])
    macroExpandSteps' _ [] acc _ = return acc

-- if let then filter macros and save to own env
-- if list then look if its macro and substitute with macro body and env
-- if gensym then replace with new identifier
stepMacroExpand :: MacroExpansionState -> LVZipper -> IOThrowsError (MacroExpansionState, LVZipper, [LVZipperTurn])
stepMacroExpand state z = return (state, z, [])
