module Compile.MacroSystem (macroSystem) where

import Types

macroSystem :: [LispVal] -> IOThrowsError [LispVal]
macroSystem = return
