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
    else runOne args

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr "repl" expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (t -> Bool) -> m t -> (t -> m a) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List Nothing $ map String $ drop 1 args)]
  _ <- runIOThrows (show <$> eval env (List Nothing [Atom Nothing "load", String (head args)]))
  return ()

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "> ") . evalAndPrint

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)
