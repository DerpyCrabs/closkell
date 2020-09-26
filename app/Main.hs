module Main where

import Control.Monad.Except
import Data.List (intercalate)
import Lib
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "run" -> runCommand $ tail args
    "compile" -> compileCommand $ tail args

-- flushStr :: String -> IO ()
-- flushStr str = putStr str >> hFlush stdout

-- readPrompt :: String -> IO String
-- readPrompt prompt = flushStr prompt >> getLine

-- evalString :: StateRef -> EnvRef -> String -> IO String
-- evalString state env expr = runIOThrows $ fmap show $ liftThrows (readExpr "repl" expr) >>= eval state env

-- evalAndPrint :: StateRef -> EnvRef -> String -> IO ()
-- evalAndPrint state env expr = evalString state env expr >>= putStrLn

until_ :: Monad m => (t -> Bool) -> m t -> (t -> m a) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runCommand :: [String] -> IO ()
runCommand args = do
  let env = primitiveBindings ++ [("args", List Nothing $ map String $ drop 1 args)]
  input <- runExceptT (load (head args) >>= moduleSystem)
  case input of
    Right val -> runIOThrows (show <$> eval env (head val)) >>= hPutStrLn stderr
    Left err -> print err
  return ()

-- runRepl :: IO ()
-- runRepl = do
--   env <- primitiveBindings
--   state <- nullState
--   until_ (== "quit") (readPrompt "> ") (evalAndPrint state env)

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

compileCommand :: [String] -> IO ()
compileCommand ["-o", outFile, inFile] = do
  compiled <- compileFile inFile
  let text = show <$> compiled
  writeFile outFile (intercalate "\n" text)
compileCommand [inFile] = do
  compiled <- compileFile inFile
  mapM_ print compiled

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
