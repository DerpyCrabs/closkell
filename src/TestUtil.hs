module TestUtil (runModuleSystem, runTypeSystem, runMacroSystem, runEmitJS, runNodeTest, runEval, runParse) where

import Compile.EmitJS (emitJS, emitPrimitives)
import Compile.MacroSystem (macroSystem)
import Compile.ModuleSystem (moduleSystem)
import Compile.TypeSystem (typeSystem)
import Control.Monad.Except
  ( MonadIO (liftIO),
    MonadTrans (lift),
    runExceptT,
  )
import Data.AST (fromValue)
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
  let astVal = fromValue (last parsedVals)
  case astVal of
    Left err -> throwError err
    Right astVal -> return $ emitJS astVal

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
