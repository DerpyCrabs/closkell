module Compile (compile) where

import Compile.MacroSystem
import Compile.ModuleSystem
import Control.Monad (foldM)
import Types

compile :: [LispVal] -> IOThrowsError [LispVal]
compile = compile' transformations

compile' :: [[LispVal] -> IOThrowsError [LispVal]] -> [LispVal] -> IOThrowsError [LispVal]
compile' transformations vals = foldM (\vals transformation -> transformation vals) vals transformations

transformations :: [[LispVal] -> IOThrowsError [LispVal]]
transformations = [moduleSystem, macroSystem]
