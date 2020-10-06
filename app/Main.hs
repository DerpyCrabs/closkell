module Main where

import Control.Monad.Except
import Lib
import Network.Wai.Handler.Warp
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "run" -> runCommand $ tail args
    "compile" -> compileCommand $ tail args
    "debug-server" -> debuggerCommand $ tail args

debuggerCommand :: [String] -> IO ()
debuggerCommand args = run 8081 (server args)

runCommand :: [String] -> IO ()
runCommand args = do
  let env = primitiveBindings ++ [("args", List Nothing $ map String $ drop 1 args)]
  input <- runExceptT (load (head args) >>= moduleSystem)
  case input of
    Right val -> runIOThrows (show <$> eval env val) >>= hPutStrLn stderr
    Left err -> print err
  return ()

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

compileCommand :: [String] -> IO ()
compileCommand ["-o", outFile, inFile] = do
  compiled <- compileFile inFile
  writeFile outFile (show compiled)
compileCommand [inFile] = do
  compiled <- compileFile inFile
  print compiled

compileFile filename = do
  contents <- readFile filename
  let ast = readExprList filename contents
  case ast of
    Left err -> putStrLn "Parsing error: " >> error (show err)
    Right ast -> do
      compiled <- runExceptT $ compile ast
      case compiled of
        Left err -> putStrLn "Compiling error: " >> error (show err)
        Right compiled -> return compiled
