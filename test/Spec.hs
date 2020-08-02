import Control.Monad.Except
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Parsing" parsingTests
  describe "Evaluation" evaluationTests
  describe "Macros" macrosTests

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
        [ ("20", Integer 20),
          ("  0x3F  ", Integer 63),
          ("  0x3f", Integer 63),
          ("0b101  ", Integer 5),
          ("0o77", Integer 63)
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
        [(" ( 3 4 5)", list [Integer 3, Integer 4, Integer 5])]
    it "parses dotted lists" $
      testTable
        (fmap head . runParse)
        [("(tt1 tt2 . tt3)", DottedList Nothing [atom "tt1", atom "tt2"] (atom "tt3"))]
    it "parses top level expressions" $
      testTable
        runParse
        [ (" (t 5) (k 6)", [func "t" [Integer 5], func "k" [Integer 6]]),
          ("(t 5)\n(k 6) ", [func "t" [Integer 5], func "k" [Integer 6]])
        ]

evaluationTests =
  let test = testTable runEval
   in do
        it "evaluates primitive types" $ test [(Integer 1, Right $ Integer 1), (String "test", Right $ String "test")]
        it "evaluates primitive functions" $ test [(func "+" [Integer 1, Integer 2], Right $ Integer 3)]

macrosTests =
  let test = testTable (fmap (fmap last) . runInterpret)
      getAtom (Right [Atom _ at]) = at
   in do
        it "can expand macro" $ test [("(defmacro sum '(+ ~(car body) ~(car (cdr body)))) (sum 3 4)", Right $ Integer 7)]
        it "supports quoting" $ test [("'(4 5)", Right $ list [Integer 4, Integer 5])]
        it "supports unquoting" $ test [("'(4 ~(+ 1 3))", Right $ list [Integer 4, Integer 4])]
        it "supports unquote-splicing" $ test [("'(4 ~@(quote (5 6)))", Right $ list [Integer 4, Integer 5, Integer 6])]
        it "supports gensym without prefix" $ do
          sym <- getAtom <$> runInterpret "(gensym)"
          sym `shouldSatisfy` all isDigit
        it "supports gensym with prefix" $ do
          sym <- getAtom <$> runInterpret "(gensym \"prefix\")"
          sym `shouldSatisfy` \s -> "prefix" `isPrefixOf` s
        it "unquotes inside of quote" $ test [("'(4 ~(+ 2 3) ~@(quote (6 7)))", Right $ list [Integer 4, Integer 5, Integer 6, Integer 7])]

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
testTable runTest ((input, expected) : tests) = do
  res <- runTest input
  (res `shouldBe` expected) >> testTable runTest tests

list = List Nothing

atom = Atom Nothing

func f args = List Nothing (atom f : args)
