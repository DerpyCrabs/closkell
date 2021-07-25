module TestUtil (runModuleSystem, runTypeSystem, runMacroSystem, runEmitJS, runNodeTest, runEval, runParse, runWasmTest, runEmitLLVM) where

import Compile.EmitJS (emitJS, emitPrimitives)
import Compile.EmitLLVM (emitLLVM)
import Compile.MacroSystem (macroSystem)
import Compile.ModuleSystem (moduleSystem)
import Compile.TypeSystem (typeSystem)
import Control.Monad.Except
  ( MonadIO (liftIO),
    MonadTrans (lift),
    runExceptT,
  )
import Data.Char (isDigit, isSpace)
import Data.Error (extractValue, throwError)
import Data.List (isPrefixOf)
import Data.Value (Value (..))
import Eval (eval)
import Eval.Primitive
import Parse
import System.Command
import Test.Hspec
import Types

runModuleSystem :: String -> IO (Either Error Value)
runModuleSystem code = runExceptT $ do
  parsedVals <- lift $ runParse code
  moduleSystem parsedVals

runTypeSystem :: String -> IO (Either Error Value)
runTypeSystem code = runExceptT $ do
  parsedVals <- lift $ runParse code
  _ <- typeSystem (last parsedVals)
  return Unit

runMacroSystem :: String -> IO (Either Error Value)
runMacroSystem code = runExceptT $ do
  parsedVals <- lift $ runParse code
  macroSystem (last parsedVals)

runEmitJS :: String -> IO (Either Error String)
runEmitJS code = runExceptT $ do
  parsedVals <- lift $ runParse code
  return $ emitJS $ last parsedVals

runNodeTest :: String -> IO ()
runNodeTest testPath = do
  input <- readFile (testPath ++ "/input.clsk")
  expected <- readFile (testPath ++ "/expected.txt")
  emittedSource <- runEmitJS input
  case emittedSource of
    Right source -> do
      Stdout out <- command [Stdin source] "node" ["--stack-size=32000"]
      rstrip out `shouldBe` rstrip expected
    Left err -> error (show err)

runEval :: String -> IO (Either Error Value)
runEval code = runExceptT $ do
  parsedVals <- lift $ runParse code
  eval primitiveBindings $ last parsedVals

runParse :: String -> IO [Value]
runParse = return . extractValue . readExprList ""

runInterpret :: String -> IO (Either Error [Value])
runInterpret code = runExceptT $ do
  parsedVals <- lift $ runParse code
  mapM (eval primitiveBindings) parsedVals

rstrip :: String -> String
rstrip = reverse . dropWhile (\c -> isSpace c || (c == '\n')) . reverse

runEmitLLVM :: String -> IO (Either Error String)
runEmitLLVM code = runExceptT $ do
  parsedVals <- lift $ runParse code
  expandedMacros <- macroSystem (last parsedVals)
  return $ emitLLVM expandedMacros

runWasmTest :: String -> IO ()
runWasmTest testPath = do
  expected <- readFile (testPath ++ "/expected.txt")
  Stdout out <- command [] "bash" ["./test/EmitLLVM/compile_and_run_wasm.sh", testPath ++ "/input.clsk"]
  rstrip out `shouldBe` rstrip expected