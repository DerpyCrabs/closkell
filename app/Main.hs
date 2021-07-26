{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad.Except
import Data.Maybe (isNothing)
import Lib
import Network.Wai.Handler.Warp
import ParseOptions
import System.Environment
import System.IO

main :: IO ()
main = do
  options <- parseOptions
  case options of
    Serve (ServeOptions port) -> run port server
    Compile opts -> compileCommand opts
    Eval opts -> evalCommand opts

evalCommand :: EvalOptions -> IO ()
evalCommand EvalOptions {eoCommon = common, eoUsingNode = usingNode, eoArgs = args} = do
  let env = primitiveBindings ++ [("args", List Nothing $ String <$> args)]
  source <- getSource (optInput common)
  compiledSource <- compileSource source (optWithoutTypecheck common)
  if usingNode
    then do
      let sourceWithArgs = Call [atom "let", list [atom "args", list $ String <$> args], compiledSource]
      result <- runExceptT $ evalUsingNode sourceWithArgs
      case result of
        Right val -> putStrLn val
        Left err -> hPutStrLn stderr "Eval error: " >> error (show err)
    else do
      result <- runExceptT $ eval env compiledSource
      case result of
        Right val -> print val
        Left err -> hPutStrLn stderr "Eval error: " >> error (show err)

compileCommand :: CompileOptions -> IO ()
compileCommand CompileOptions {coCommon = common, coJSOutput = jsOutput, coCLSKOutput = clskOutput} = do
  source <- getSource (optInput common)
  compiledSource <- compileSource source (optWithoutTypecheck common)
  case jsOutput of
    Just jsOutput -> do
      ast <- case fromValue compiledSource of
        Left err -> hPutStrLn stderr "Value to AST conversion error: " >> error (show err)
        Right ast -> return ast
      let jsSource = emitJS ast
      optimized <- closureCompilerPass jsSource
      writeFile jsOutput optimized
    Nothing -> return ()
  case clskOutput of
    Just clskOutput -> do
      writeFile clskOutput (show compiledSource)
    Nothing -> return ()
  when (isNothing jsOutput && isNothing clskOutput) $ error "No output selected"

getSource :: Input -> IO (String, String)
getSource StdInput = ("stdin",) <$> getContents
getSource (FileInput path) = (path,) <$> readFile path

compileSource :: (String, String) -> Bool -> IO Value
compileSource (name, src) withoutTypecheck = do
  let ast = readExprList name src
  case ast of
    Left err -> hPutStrLn stderr "Parsing error: " >> error (show err)
    Right ast -> do
      withoutModules <- runExceptT $ moduleSystem ast
      case withoutModules of
        Left err -> hPutStrLn stderr "ModuleSystem error: " >> error (show err)
        Right withoutModules -> do
          withoutMacros <- runExceptT $ macroSystem withoutModules
          case withoutMacros of
            Left err -> hPutStrLn stderr "MacroSystem error: " >> error (show err)
            Right withoutMacros ->
              if withoutTypecheck
                then return withoutMacros
                else
                  runExceptT (typeSystem withoutMacros) >>= \case
                    Left err -> hPutStrLn stderr "TypeSystem error: " >> error (show err)
                    Right ast -> return ast