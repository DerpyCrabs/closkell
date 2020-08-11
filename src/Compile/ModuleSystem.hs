module Compile.ModuleSystem (moduleSystem) where

import Control.Monad.Except
import Types
import Data.Value

moduleSystem (List _ (Atom _ "executable":loadExprs):exprs)  = return [func "do" exprs]
moduleSystem exprs  = moduleSystem $ func "executable" []:exprs