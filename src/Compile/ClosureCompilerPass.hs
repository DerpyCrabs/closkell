module Compile.ClosureCompilerPass where

import System.Command

compilerConfig =
  [ "--language_out",
    "ECMASCRIPT_2020",
    "--compilation_level",
    "ADVANCED",
    "--warning_level",
    "QUIET"
  ]

closureCompilerPass :: String -> IO String
closureCompilerPass source = do
  Stdout out <- command [Stdin source, EchoStderr False] "google-closure-compiler" compilerConfig
  return out
