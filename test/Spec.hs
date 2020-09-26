import Compile.ConstFolding (constFolding)
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
  describe "Constant folding" constFoldingTests
  describe "Module system" moduleSystemTests
  describe "Macro system" macroSystemTests

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
        it "evaluates primitive functions" $ test [(func "+" [int 1, int 2], Right $ int 3)]
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

constFoldingTests =
  let testLast = testTable ((fmap (last <$>) .) runConstFolding)
   in do
        it "doesn't alter IO primitives" $ testLast [("(io.write 5)", Right $ func "io.write" [int 5])]
        it "evaluates pure code" $ testLast [("(+ 3 5)", Right $ int 8)]
        it "doesn't evaluate impure code" $
          testLast
            [ ("(+ (io.read) 5)", Right $ func "+" [func "io.read" [], int 5])
            ]
        it "evaluates pure functions" $
          testLast
            [ ("(#(+ %1 %2) 3 5)", Right $ int 8),
              ("(+ (#(+ %1 %2) 3 5) 2)", Right $ int 10)
            ]
        it "doesn't evaluate impure code inside of inline functions" $
          testLast
            [ ("(#(+ (io.read) %1) 4)", Right $ list [lambda [] (Just $ atom "%&") [func "+" [func "io.read" [], func "nth" [int 0, atom "%&"]]], int 4])
            ]
        it "handles apply" $
          testLast
            [ ("(apply + '(4 5))", Right $ int 9),
              ("(apply + '(~(io.read) 5))", Right $ list [atom "apply", atom "+", func "quote" [list [func "unquote" [func "io.read" []], int 5]]])
            ]
        it "handles macros" $
          testLast
            [ ("(let (sum (macro '(+ ~(car body) ~(car (cdr body))))) (sum 1 2))", Right $ list [atom "let", list [atom "sum", func "macro" [func "quote" [func "+" [func "unquote" [func "car" [atom "body"]], func "unquote" [func "car" [func "cdr" [atom "body"]]]]]]], int 3]),
              ("(let (sum (macro '(+ ~(io.read) ~(car (cdr body))))) (sum 1 2))", Right $ list [atom "let", list [atom "sum", func "macro" [func "quote" [func "+" [func "unquote" [func "io.read" []], func "unquote" [func "car" [func "cdr" [atom "body"]]]]]]], func "sum" [int 1, int 2]])
            ]
        it "handles quote" $
          testLast
            [ ("(car '(5 4))", Right $ int 5)
            ]
        it "handles unquote and unquote-splicing inside of quote" $
          testLast
            [ ("(car '((unquote-splicing '(1 4))))", Right $ int 1),
              ("(car '(~(+ 1 1) 4))", Right $ int 2)
            ]
        it "doesn't evaluate IO inside of unquote" $
          testLast
            [ ("'(~(io.read) 5)", Right $ func "quote" [list [func "unquote" [func "io.read" []], int 5]])
            ]
        it "evaluates pure code inside of impure" $
          testLast
            [ ("(io.dump (+ 3 5))", Right $ func "io.dump" [int 8]),
              ("(io.dump (+ (io.read) (+ 3 5)))", Right $ func "io.dump" [func "+" [func "io.read" [], int 8]])
            ]
        it "handles if" $
          testLast
            [ ("(if (= 1 1) 2 3)", Right $ int 2),
              ("(if (= 2 1) 2 3)", Right $ int 3),
              ("(if (= (io.read) \"k\") 2 3)", Right $ func "if" [func "=" [func "io.read" [], String "k"], int 2, int 3]),
              ("(if (= 1 1) (io.read) 3)", Right $ func "io.read" []),
              ("(if (= 1 2) (io.read) (io.write))", Right $ func "io.write" [])
            ]
        it "handles gensym" $
          testLast
            [ ("(gensym)", Right $ atom "1"),
              ("(gensym \"k\")", Right $ atom "k1")
            ]
        it "doesn't evaluate arg of forbid-folding special form" $
          testLast
            [ ("(+ (forbid-folding (+ 1 2)) 4)", Right $ func "+" [func "+" [int 1, int 2], int 4])
            ]
        it "supports do special form" $
          testLast
            [ ("(do (io.dump 5) (io.dump 6))", Right $ func "do" [func "io.dump" [int 5], func "io.dump" [int 6]]),
              ("(do (+ 4 5) (+ 7 8))", Right $ int 15),
              ("(do (io.dump 5) (+ 4 5))", Right $ func "do" [func "io.dump" [int 5], int 9])
            ]
        it "supports let special form" $
          testLast
            [ ("(let (tt1 5) (tt2 6) (+ tt1 tt2))", Right $ list [atom "let", list [atom "tt1", int 5], list [atom "tt2", int 6], int 11]),
              ("(let (tt1 (io.read)) (tt2 (+ 3 6)) (+ tt1 tt2))", Right $ list [atom "let", list [atom "tt1", func "io.read" []], list [atom "tt2", func "+" [int 3, int 6]], func "+" [atom "tt1", atom "tt2"]]),
              ("(let (tt1 5) (tt2 (+ 3 6)) (io.dump tt1 tt2 (+ 3 9)))", Right $ list [atom "let", list [atom "tt1", int 5], list [atom "tt2", func "+" [int 3, int 6]], func "io.dump" [int 5, int 9, int 12]]),
              ("(let (tt1 (lambda (x) (io.dump (tt2 x)))) (tt2 (lambda (y) (if (io.dump) (tt1 y) 0))) (io.dump (tt1 1)))", Right $ list [atom "let", list [atom "tt1", lambda [atom "x"] Nothing [func "io.dump" [func "tt2" [atom "x"]]]], list [atom "tt2", lambda [atom "y"] Nothing [list [atom "if", func "io.dump" [], func "tt1" [atom "y"], int 0]]], func "io.dump" [func "tt1" [int 1]]]),
              ("(let (sum 5) '(~sum))", Right $ list [atom "let", list [atom "sum", int 5], func "quote" [list [int 5]]])
            ]

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

runConstFolding :: String -> IO (Either LispError [LispVal])
runConstFolding code = runExceptT $ do
  parsedVals <- lift $ runParse code
  constFolding parsedVals

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
runEval val = runExceptT $ do
  env <- liftIO primitiveBindings
  state <- liftIO nullState
  eval state env val

runParse :: String -> IO [LispVal]
runParse = return . extractValue . readExprList ""

runInterpret :: String -> IO (Either LispError [LispVal])
runInterpret code = runExceptT $ do
  parsedVals <- lift $ runParse code
  env <- liftIO primitiveBindings
  state <- liftIO nullState
  mapM (eval state env) parsedVals

testTable :: (Show b, Eq b) => (a -> IO b) -> [(a, b)] -> IO ()
testTable runTest [] = return ()
testTable runTest ((input, expected) : tests) = (runTest input `shouldReturn` expected) >> testTable runTest tests
