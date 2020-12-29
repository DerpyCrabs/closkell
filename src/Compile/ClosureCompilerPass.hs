module Compile.ClosureCompilerPass where

import System.Command

compilerConfig = ["--language_out", "ECMASCRIPT_2020", "--compilation_level", "ADVANCED"]

closureCompilerPass :: String -> IO String
closureCompilerPass source = do
  Stdout out <- command [Stdin source] "google-closure-compiler" compilerConfig
  return out
