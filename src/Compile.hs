module Compile (compile) where

import Types

compile :: [LispVal] -> IOThrowsError [LispVal]
compile vals = do
  return vals