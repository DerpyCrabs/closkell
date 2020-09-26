import Compile.MacroSystem (macroSystem)
import Compile.ModuleSystem (moduleSystem)
import Control.Monad.Except
  ( MonadIO (liftIO),
    MonadTrans (lift),
    runExceptT,
  )
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Value
import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Parser" parsingTests
  describe "Eval" evaluationTests
  describe "Macro expansion" macrosTests
  describe "Module system" moduleSystemTests
  describe "Macro system" macroSystemTests
  describe "LispVal Zipper" zipperTests

parsingTests =
  do
    it "parses empty list" $
      testTable
        (fmap head . runParse)
        [ ("()", func "quote" [list []]),
          ("(   )", func "quote" [list []]),
          (" () ", func "quote" [list []])
        ]
    it "parses integers" $
      testTable
        (fmap head . runParse)
        [ ("20", int 20),
          ("  0x3F  ", int 63),
          ("  0x3f", int 63),
          ("0b101  ", int 5),
          ("0o77", int 63)
        ]
    it "parses booleans" $
      testTable
        (fmap head . runParse)
        [ ("true", Bool True),
          ("  false  ", Bool False)
        ]
    it "parses strings" $
      testTable
        (fmap head . runParse)
        [ ("\"test\"", String "test"),
          (" \"\\n \" ", String "\n "),
          ("\"\\0222\"", String "\0222"),
          ("\"\\f\"", String "\f")
        ]
    it "parses characters" $
      testTable
        (fmap head . runParse)
        [ ("\\t", Character 't'),
          (" \\formfeed ", Character '\f'),
          ("\\0222", Character '\0222')
        ]
    it "parses floats" $
      testTable
        (fmap head . runParse)
        [ ("0.345", Float 0.345)
        ]
    it "parses atoms" $
      testTable
        (fmap head . runParse)
        [ ("test", atom "test"),
          ("$", atom "$"),
          ("test5", atom "test5"),
          ("test!$%&|*+-/:<=>?&^_.", atom "test!$%&|*+-/:<=>?&^_.")
        ]
    it "parses lists" $
      testTable
        (fmap head . runParse)
        [(" ( 3 4 5)", list [int 3, int 4, int 5])]
    it "parses dotted lists" $
      testTable
        (fmap head . runParse)
        [ ("(tt1 tt2 . tt3)", dottedList [atom "tt1", atom "tt2"] (atom "tt3")),
          ("(. tt1)", dottedList [] (atom "tt1"))
        ]
    it "parses lambda shorthand" $
      testTable
        (fmap head . runParse)
        [("#(+ %&)", list (atom "lambda" : dottedList [] (atom "%&") : [func "+" [atom "%&"]]))]
    it "parses lambda shorthand arguments" $
      testTable
        (fmap head . runParse)
        [ ("#(+ %%)", lambda [] (Just $ atom "%&") [func "+" [func "car" [atom "%&"]]]),
          ("#(+ %1 %5)", lambda [] (Just $ atom "%&") [func "+" [func "nth" [int 0, atom "%&"], func "nth" [int 4, atom "%&"]]])
        ]
    it "parses quoted expressions" $
      testTable
        (fmap head . runParse)
        [("'(3 4 5)", func "quote" [list [int 3, int 4, int 5]])]
    it "parses unquoted expressions" $
      testTable
        (fmap head . runParse)
        [("~(3 4 5)", func "unquote" [list [int 3, int 4, int 5]])]
    it "parses unquote-spliced expressions" $
      testTable
        (fmap head . runParse)
        [ ("~@(3 4 5)", func "unquote-splicing" [list [int 3, int 4, int 5]]),
          ("'~@'(5 4)", func "quote" [func "unquote-splicing" [func "quote" [list [int 5, int 4]]]])
        ]
    it "parses top level expressions" $
      testTable
        runParse
        [ (" (t 5) (k 6)", [func "t" [int 5], func "k" [int 6]]),
          ("(t 5)\n(k 6) ", [func "t" [int 5], func "k" [int 6]])
        ]

evaluationTests =
  let test = testTable runEval
   in do
        it "evaluates primitive types" $ test [(int 1, Right $ int 1), (String "test", Right $ String "test")]
        it "evaluates primitive functions" $
          test
            [ (func "+" [int 1, int 2], Right $ int 3),
              (func "+" [int 1, func "+" [int 2, int 3]], Right $ int 6)
            ]
        it "can apply evaluated special forms to args" $
          test
            [ (list [func "car" [func "quote" [list [atom "quote"]]], list [int 4, int 5]], Right $ list [int 4, int 5])
            ]
        it "handles get function" $
          test
            [ (func "get" [String "k", func "quote" [list [String "b", int 5, String "k", int 6]]], Right $ int 6),
              (func "get" [String "b", func "quote" [list [String "b", int 5, String "k", int 6]]], Right $ int 5)
            ]

macrosTests =
  let test = testTable (fmap (fmap last) . runInterpret)
      getAtom (Right [Atom _ at]) = at
   in do
        it "supports quoting" $ test [("'(4 5)", Right $ list [int 4, int 5])]
        it "supports unquoting" $ test [("'(4 ~(+ 1 3))", Right $ list [int 4, int 4])]
        it "supports unquote-splicing" $ test [("'(4 ~@(quote (5 6)))", Right $ list [int 4, int 5, int 6])]
        it "supports gensym without prefix" $ do
          sym <- getAtom <$> runInterpret "(gensym)"
          sym `shouldSatisfy` all isDigit
        it "supports gensym with prefix" $ do
          sym <- getAtom <$> runInterpret "(gensym \"prefix\")"
          sym `shouldSatisfy` \s -> "prefix" `isPrefixOf` s
        it "unquotes inside of quote" $ test [("'(4 ~(+ 2 3) ~@(quote (6 7)))", Right $ list [int 4, int 5, int 6, int 7])]

moduleSystemTests =
  let test path = runFolderTest runModuleSystem ("test/ModuleSystem/" ++ path)
   in do
        it "transforms executable modules without executable header" $ test "test1"
        it "transforms executable modules without loads" $ test "test2"
        it "loads module with default prefix" $ test "test3"
        it "loads module with custom prefix" $ test "test4"
        it "loads module without prefix" $ test "test5"
        it "loads multiple modules" $ test "test6"

macroSystemTests =
  let test path = runFolderTest runMacroSystem ("test/MacroSystem/" ++ path)
   in do
        it "doesn't alter code without macros" $ test "test1"
        it "removes macro definitions from let bindings" $ test "test2"
        it "expands macro calls" $ test "test3"
        it "expands nested macro calls" $ test "test4"
        it "expands macro calls inside of macros" $ test "test5"
        it "handles nested let macro bindings" $ test "test6"

zipperTests = do
  it "can be converted from LispVal" $ lvFromAST (int 1) `shouldBe` ([], Just $ int 1, [])
  it "can be converted to LispVal" $ lvToAST ([], Just $ int 2, [LispValCrumb [] Nothing [int 1] [int 3]]) `shouldBe` list [int 1, int 2, int 3]
  it "can go down" $ (lvDown . lvFromAST . list $ [int 1, int 2, int 3]) `shouldBe` ([], Just $ int 1, [LispValCrumb [] Nothing [] [int 2, int 3]])
  it "can go up" $ lvUp ([], Just $ int 2, [LispValCrumb [] Nothing [int 1] [int 3]]) `shouldBe` ([], Just . list $ [int 1, int 2, int 3], [])
  it "can go right" $
    (lvRight . lvRight . lvDown . lvFromAST . list $ [int 1, int 2, int 3])
      `shouldBe` ([], Just $ int 3, [LispValCrumb [] Nothing [int 1, int 2] []])
  it "can modify current value" $ (lvModify (\(Integer n) -> Integer (n + 1)) . lvFromAST $ int 1) `shouldBe` lvFromAST (int 2)

runFolderTest runner testPath = do
  input <- readFile (testPath ++ "/input.clsk")
  expected <- readFile (testPath ++ "/expected.clsk")
  parsedExpected <- runParse expected
  runner input `shouldReturn` Right parsedExpected
  return ()

runModuleSystem :: String -> IO (Either LispError [LispVal])
runModuleSystem code = runExceptT $ do
  parsedVals <- lift $ runParse code
  moduleSystem parsedVals

runMacroSystem :: String -> IO (Either LispError [LispVal])
runMacroSystem code = runExceptT $ do
  parsedVals <- lift $ runParse code
  macroSystem parsedVals

runEval :: LispVal -> IO (Either LispError LispVal)
runEval val = runExceptT $ eval primitiveBindings val

runParse :: String -> IO [LispVal]
runParse = return . extractValue . readExprList ""

runInterpret :: String -> IO (Either LispError [LispVal])
runInterpret code = runExceptT $ do
  parsedVals <- lift $ runParse code
  mapM (eval primitiveBindings) parsedVals

testTable :: (Show b, Eq b) => (a -> IO b) -> [(a, b)] -> IO ()
testTable runTest [] = return ()
testTable runTest ((input, expected) : tests) = (runTest input `shouldReturn` expected) >> testTable runTest tests
