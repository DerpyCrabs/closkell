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
    it "parses top level expressions" $
      testTable
        runParse
        [ (" (t 5) (k 6)", [func "t" [Integer 5], func "k" [Integer 6]])
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
