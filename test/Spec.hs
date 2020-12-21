import Compile.MacroSystem (macroSystem)
import Compile.ModuleSystem (moduleSystem)
import Compile.TypeSystem (typeSystem)
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
  describe "Module system" moduleSystemTests
  describe "Macro system" macroSystemTests
  describe "Type system" typeSystemTests
  describe "LispVal Zipper" zipperTests

parsingTests =
  do
    it "parses empty list" $
      testTable
        (fmap head . runParse)
        [ ("[]", list []),
          ("[   ]", list []),
          (" [] ", list [])
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
        [(" [ 3 4 5]", list [int 3, int 4, int 5])]
    it "parses dotted lists" $
      testTable
        (fmap head . runParse)
        [ ("[tt1 tt2 . tt3]", dottedList [atom "tt1", atom "tt2"] (atom "tt3")),
          ("[. tt1]", dottedList [] (atom "tt1"))
        ]
    it "parses fn shorthand" $
      testTable
        (fmap head . runParse)
        [("#(+ %&)", Call (atom "fn" : dottedList [] (atom "%&") : [func "+" [atom "%&"]]))]
    it "parses fn shorthand arguments" $
      testTable
        (fmap head . runParse)
        [ ("#(+ %%)", fn [] (Just $ atom "%&") [func "+" [func "car" [atom "%&"]]]),
          ("#(+ %1 %5)", fn [] (Just $ atom "%&") [func "+" [func "nth" [int 0, atom "%&"], func "nth" [int 4, atom "%&"]]])
        ]
    it "parses quoted expressions" $
      testTable
        (fmap head . runParse)
        [("'[3 4 5]", func "quote" [list [int 3, int 4, int 5]])]
    it "parses unquoted expressions" $
      testTable
        (fmap head . runParse)
        [("~[3 4 5]", func "unquote" [list [int 3, int 4, int 5]])]
    it "parses unquote-spliced expressions" $
      testTable
        (fmap head . runParse)
        [ ("~@[3 4 5]", func "unquote-splicing" [list [int 3, int 4, int 5]]),
          ("'~@'[5 4]", func "quote" [func "unquote-splicing" [func "quote" [list [int 5, int 4]]]])
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
        it "evaluates primitive types" $ test [("1", Right $ int 1), ("\"test\"", Right $ String "test")]
        it "evaluates very primitive functions" $
          test
            [ ("(+ 1 2)", Right $ int 3)
            ]
        it "evaluates primitive functions" $
          test
            [ ("(+ 1 2)", Right $ int 3),
              ("(+ 1 (+ 2 3))", Right $ int 6)
            ]
        it "evaluates apply" $
          test
            [ ("(apply + [5 6])", Right $ int 11),
              ("(apply + (cdr [3 4 5]))", Right $ int 9)
            ]
        it "can throw errors from code" $
          test [("(io.throw \"Error\")", Left $ FromCode $ String "Error")]
        it "evaluates if" $
          test
            [ ("(if (== 3 3) (+ 1 2) (+ 5 6))", Right $ int 3),
              ("(if (== 2 3) (+ 1 2) (+ 5 6))", Right $ int 11),
              ("(if true (+ 1 2) (+ 5 6))", Right $ int 3),
              ("(if false (+ 1 2) (+ 5 6))", Right $ int 11)
            ]
        it "handles let bindings" $
          test
            [ ("(let [k 5] [g (+ 1 2)] (- k g))", Right $ int 2),
              ("(let (+ 1 2))", Right $ int 3),
              ("(let [k 5] [r (fn [a] (+ k a))] (let [k 7] (r 5)))", Right $ int 10),
              ("(let [k 5] (let [k 7] k))", Right $ int 7)
            ]
        it "supports function definition" $
          test
            [ ("(#(+ %1 %2) 2 3)", Right $ int 5),
              ("(let [f (fn [a b] (+ a b))] (f 2 3))", Right $ int 5)
            ]
        it "handles get function" $
          test
            [ ("(get \"k\" [\"b\" 5 \"k\" 6])", Right $ int 6),
              ("(get \"b\" [\"b\" 5 \"k\" 6])", Right $ int 5)
            ]
        it "supports quoting" $ test [("'[4 5]", Right $ list [int 4, int 5])]
        it "supports unquoting" $
          test
            [ ("(+ 4 ~(+ 1 5))", Right $ int 10),
              ("(+ 4 ~(+ 5 6) ~(+ 1 3))", Right $ int 19)
            ]
        it "supports unquote-splicing" $
          test
            [ ("(+ 4 ~@[5 6])", Right $ int 15),
              ("(+ 4 ~@[5 6] ~@[7 8])", Right $ int 30)
            ]
        it "supports list unquoting" $
          test
            [ ("[4 ~(+ 1 5)]", Right $ list [int 4, int 6]),
              ("[4 ~(+ 5 6) ~(+ 1 3)]", Right $ list [int 4, int 11, int 4])
            ]
        it "supports list unquote-splicing" $
          test
            [ ("[4 ~@[5 6]]", Right $ list [int 4, int 5, int 6]),
              ("[4 ~@[5 6] ~@[7 8]]", Right $ list [int 4, int 5, int 6, int 7, int 8])
            ]
        it "supports unquote outside of quote" $
          test
            [ ("(+ 4 ~(quote 5))", Right $ int 9)
            ]
        it "supports all of std" $
          test
            [ ("(let [sum #(+ %1 %2)] [null? #(if (eq? %% []) true false)] [foldr (fn [func end lst] (if (null? lst) end (func (car lst) (foldr func end (cdr lst)))))] (foldr sum 0 [1]))", Right $ int 1),
              ("(let [sum #(+ %1 %2)] [null? #(if (eq? %% []) true false)] [foldr (fn [func end lst] (if (null? lst) end (func (car lst) (foldr func end (cdr lst)))))] (foldr sum 5 []))", Right $ int 5),
              ("(let [sum #(+ %1 %2)] [null? #(if (eq? %% []) true false)] [foldr (fn [func end lst] (if (null? lst) end (func (car lst) (foldr func end (cdr lst)))))] (foldr sum 3 [1 2 4]))", Right $ int 10)
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
      getAtom (Right (Atom _ at)) = at
   in do
        it "doesn't alter code without macros" $ test "test1"
        it "removes macro definitions from let bindings" $ test "test2"
        it "expands macro calls" $ test "test3"
        it "expands nested macro calls" $ test "test4"
        it "expands macro calls inside of macros" $ test "test5"
        -- it "handles nested let macro bindings" $ test "test6"
        it "supports gensym without prefix" $ do
          sym <- getAtom <$> runMacroSystem "(gensym)"
          sym `shouldSatisfy` all isDigit
        it "supports gensym with prefix" $ do
          sym <- getAtom <$> runMacroSystem "(gensym \"prefix\")"
          sym `shouldSatisfy` \s -> "prefix" `isPrefixOf` s

zipperTests = do
  it "can be converted from LispVal" $ lvFromAST (int 1) `shouldBe` ([], int 1, [])
  it "can be converted to LispVal" $ lvToAST ([], int 2, [LVCrumb [] [int 1] [int 3]]) `shouldBe` Call [int 1, int 2, int 3]
  it "can go down" $ (lvDown . lvFromAST . Call $ [int 1, int 2, int 3]) `shouldBe` ([], int 1, [LVCrumb [] [] [int 2, int 3]])
  it "can go up" $ lvUp ([], int 2, [LVCrumb [] [int 1] [int 3]]) `shouldBe` ([], Call [int 1, int 2, int 3], [])
  it "can go right" $
    (lvRight . lvRight . lvDown . lvFromAST . Call $ [int 1, int 2, int 3])
      `shouldBe` ([], int 3, [LVCrumb [] [int 1, int 2] []])
  it "can modify current value" $ (lvModify (\(Integer n) -> Integer (n + 1)) . lvFromAST $ int 1) `shouldBe` lvFromAST (int 2)

typeSystemTests =
  let test = testTable runTypeSystem
   in do
        it "handles primitive function's primitive arguments mismatch" $
          test
            [ ("(+ \\a 5)", Left $ TypeMismatch (TSum [TInteger, TFloat]) TCharacter),
              ("(- (* \\b 5))", Left $ TypeMismatch (TSum [TInteger, TFloat]) TCharacter),
              ("(- \"s\" 5)", Left $ TypeMismatch (TSum [TInteger, TFloat]) TString)
            ]
        it "handles correct primitive function types" $
          test
            [ ("(+ 1 2.5 1)", Right Unit),
              ("(== 3 3)", Right Unit),
              ("(+ 1 (- 3 2) 2.5)", Right Unit)
            ]
        it "supports TList type" $
          test
            [ ("(string.concat \"5\" \"2\")", Right Unit),
              ("(&& true (|| false true))", Right Unit)
            ]
        it "supports parametric polymorphism" $
          test
            [ ("(car [1 2])", Right Unit),
              ("(+ (car [1 2]) 3.5)", Right Unit)
            ]
        it "supports if expressions" $
          test
            [ ("(if true 5 4)", Right Unit),
              ("(if 5 3 4)", Left $ TypeMismatch TBool TInteger),
              ("(if (== 5 4) 3 4)", Right Unit),
              ("(if (== 5 4) 5 true)", Right Unit),
              ("(+ 3 (if (== 5 4) 3 4))", Right Unit),
              ("(if true 5 (+ \\c \\d))", Left $ TypeMismatch (TSum [TInteger, TFloat]) TCharacter),
              ("(if true (if true 3 4) 5)", Right Unit)
            ]
        it "supports user-defined functions" $
          test
            [ ("(let [kek (fn [x y] (+ x y))] (kek 5 3))", Right Unit),
              ("(let [kek (fn [x y] (+ x y))] (kek \\c 3))", Left $ TypeMismatch (TSum [TInteger, TFloat]) TCharacter),
              ("(let [kek (fn [x y . pek] (+ x y ~@pek))] (kek 5 3 4 5))", Right Unit),
              ("(let [kek (fn [x y . pek] (+ x y ~@pek))] (kek 5 3 4 \\c))", Left $ TypeMismatch (TSum [TInteger, TFloat]) TCharacter)
            ]
        it "handles user-defined function argument number mismatch" $
          test
            [ ("(let [sum (fn [a b] 5)] (sum 5))", Left $ NumArgs 2 [int 5]),
              ("(let [sum (fn [a b] 5)] (sum 5 3 5))", Left $ NumArgs 2 [int 5, int 3, int 5]),
              ("(let [sum (fn [a b . c] 5)] (sum 5 3 5))", Right Unit),
              ("(let [sum (fn [. c] 5)] (sum 5))", Right Unit),
              ("(let [sum (fn [a b] 5)] (sum))", Left $ NumArgs 2 [])
            ]
        it "supports recursive functions" $
          test
            [ ("(let [rec (fn [a] (if (== a 5) true (rec a)))] (eq? false (rec 6)))", Right Unit),
              ("(let [rec (fn [a] (let [rec (fn [b] (rec a))] (rec 5)))] (rec 1))", Right Unit),
              ("(let [rec (fn [a] (if (== a 5) true (rec \\c)))] (eq? false (rec 6)))", Left $ TypeMismatch (TSum [TInteger, TFloat]) TCharacter)
            ]
        it "supports all std code" $
          test
            [ ("(let [not (fn [arg] arg)] [not2 #(if %% \"s\" true)] (not2 (not true)))", Right Unit),
              ("(let [not #(if %% false true)] [not2 #(if %% \\c true)] (not2 (not true)))", Right Unit),
              ("(let [not #(if %% false true)] [not2 #(if %% \"s\" true)] (if (not2 (not true)) 5 0))", Left $ TypeMismatch TBool (TSum [TString, TBool])),
              ("(if (eq? [] []) 5 0)", Right Unit),
              ("(let [not #(if %% 3 true)] [null? #(if (not (eq? %% [])) true false)] (if (not (not (null? []))) 5 0))", Left $ TypeMismatch TBool (TSum [TInteger, TBool])),
              ("(let [not #(if %% 3 true)] [null? #(if (not (eq? %% [])) true false)] (if (not (not (null? []))) 5 0))", Left $ TypeMismatch TBool (TSum [TInteger, TBool])),
              ("(eq? 5 (car [2 \\c]))", Right Unit),
              ("(eq? \\c (car [2 5]))", Left $ FailedToDeduceVar "a" [TCharacter, TInteger]),
              ("(let [get-second (fn [a b] b)] (eq? 5 (apply get-second [\\c 5])))", Right Unit),
              ("(let [get-second (fn [a b] b)] (eq? 5 (apply get-second [\\c \\с])))", Left $ FailedToDeduceVar "a" [TInteger, TCharacter]),
              ("(let [some-fn (fn [] (if (== 3 4) \\c 5))] (eq? true (some-fn)))", Left $ FailedToDeduceVar "a" [TBool, TSum [TCharacter, TInteger]]),
              ("(let [sum #(+ %1 %2)] [foldr (fn [func end lst] (if (eq? lst []) end (func (car lst) (foldr func end (cdr lst)))))] (foldr sum 3 [1 2 4]))", Right Unit),
              ("(let [sum #(+ %1 %2)] [foldr (fn [func end lst] (if (eq? lst []) end (func (car lst) (foldr sum end (cdr lst)))))] (foldr sum 8 [1 2 4]))", Right Unit),
              ("(let [sum #(+ %1 %2)] [foldr (fn [func end lst] (if (eq? lst []) end (func (car lst) (foldr end (cdr lst)))))] (foldr sum 3 [1 2 4]))", Left $ NumArgs 3 [int 3, Type $ TList TInteger]),
              ("(let [foldr (fn [func end lst] (if (eq? lst []) end (func (car lst) (foldr func end (cdr lst)))))] (foldr + 3 [1 2 4]))", Right Unit)
            ]

runFolderTest runner testPath = do
  input <- readFile (testPath ++ "/input.clsk")
  expected <- readFile (testPath ++ "/expected.clsk")
  parsedExpected <- runParse expected
  runner input `shouldReturn` Right (last parsedExpected)
  return ()

runModuleSystem :: String -> IO (Either LispError LispVal)
runModuleSystem code = runExceptT $ do
  parsedVals <- lift $ runParse code
  moduleSystem parsedVals

runTypeSystem :: String -> IO (Either LispError LispVal)
runTypeSystem code = runExceptT $ do
  parsedVals <- lift $ runParse code
  _ <- typeSystem (last parsedVals)
  return Unit

runMacroSystem :: String -> IO (Either LispError LispVal)
runMacroSystem code = runExceptT $ do
  parsedVals <- lift $ runParse code
  macroSystem (last parsedVals)

runEval :: String -> IO (Either LispError LispVal)
runEval code = runExceptT $ do
  parsedVals <- lift $ runParse code
  eval primitiveBindings $ last parsedVals

runParse :: String -> IO [LispVal]
runParse = return . extractValue . readExprList ""

runInterpret :: String -> IO (Either LispError [LispVal])
runInterpret code = runExceptT $ do
  parsedVals <- lift $ runParse code
  mapM (eval primitiveBindings) parsedVals

testTable :: (Show b, Eq b) => (a -> IO b) -> [(a, b)] -> IO ()
testTable _ [] = return ()
testTable runTest ((input, expected) : tests) = (runTest input `shouldReturn` expected) >> testTable runTest tests
