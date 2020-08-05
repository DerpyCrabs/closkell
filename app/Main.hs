module Main where

import Control.Monad.Except
import Lib
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else case head args of
      "run" -> run $ tail args
      "compile" -> compile $ tail args

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: StateRef -> EnvRef -> String -> IO String
evalString state env expr = runIOThrows $ fmap show $ liftThrows (readExpr "repl" expr) >>= eval state env

evalAndPrint :: StateRef -> EnvRef -> String -> IO ()
evalAndPrint state env expr = evalString state env expr >>= putStrLn

until_ :: Monad m => (t -> Bool) -> m t -> (t -> m a) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

run :: [String] -> IO ()
run args = do
  env <- primitiveBindings >>= flip bindVars [("args", List Nothing $ map String $ drop 1 args)]
  state <- nullState
  _ <- runIOThrows (show <$> eval state env (List Nothing [Atom Nothing "load", String (head args)])) >>= hPutStrLn stderr
  return ()

runRepl :: IO ()
runRepl = do
  env <- primitiveBindings
  state <- nullState
  until_ (== "quit") (readPrompt "> ") (evalAndPrint state env)

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

compile :: [String] -> IO ()
compile args = do
  let filename = head args
  contents <- readFile filename
  let ast = readExprList filename contents
  case ast of
    Left err -> putStrLn "Parsing error: " >> print err
    Right ast -> mapM_ print ast
