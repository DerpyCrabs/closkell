module Main where

import Control.Monad (when)
import Criterion.Main
import Data.Value
import TestUtil

benchEval fileName expectedOutput = do
  input <- readFile fileName
  output <- runEval input
  when (output /= Right expectedOutput) (error (show output))

benchTypeSystem fileName = do
  input <- readFile fileName
  output <- runTypeSystem input
  when (output /= Right Unit) (error (show output))

main =
  defaultMain
    [ bgroup
        "Eval"
        [ bench "fib(12)" $ nfIO (benchEval "bench/fib12.clsk" (int 144)),
          bench "remove(20)" $ nfIO (benchEval "bench/remove20.clsk" (list [int 1, int 3, int 5, int (-5), int (-3), int (-1), int 5, int 1, int 3]))
        ],
      bgroup
        "TypeSystem"
        [ bench "fib(12)" $ nfIO (benchTypeSystem "bench/fib12.clsk"),
          bench "remove(20)" $ nfIO (benchTypeSystem "bench/remove20.clsk")
        ]
    ]
