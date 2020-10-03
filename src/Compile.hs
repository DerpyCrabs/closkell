module Compile (compile) where

import Compile.MacroSystem
import Compile.ModuleSystem
import Control.Monad (foldM)
import Types

compile :: [LispVal] -> IOThrowsError LispVal
compile val = moduleSystem val >>= macroSystem
