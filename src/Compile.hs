module Compile (compile) where

import Types
import Compile.ConstFolding
import Compile.ModuleSystem
import Control.Monad (foldM)


compile :: [LispVal] -> IOThrowsError [LispVal]
compile = compile' transformations

compile' :: [[LispVal] -> IOThrowsError [LispVal]] -> [LispVal] -> IOThrowsError [LispVal]
compile' transformations vals = foldM (\vals transformation -> transformation vals) vals transformations

transformations :: [[LispVal] -> IOThrowsError [LispVal]]
transformations = [moduleSystem, constFolding]