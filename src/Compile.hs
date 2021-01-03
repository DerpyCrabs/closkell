module Compile (compile) where

import Compile.MacroSystem
import Compile.ModuleSystem
import Compile.TypeSystem
import Types

compile :: [Value] -> IOThrowsError Value
compile val = moduleSystem val >>= macroSystem >>= typeSystem
