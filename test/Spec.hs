import Control.Monad.Except
import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Parsing" parsingTests
  describe "Evaluation" evaluationTests

parsingTests =
  let test = testTable runParse
   in do
        it "parses empty list" $
          test
            [ ("()", func "quote" [list []]),
              ("(   )", func "quote" [list []]),
              (" () ", func "quote" [list []])
            ]
        it "parses integers" $
          test
            [ ("20", Integer 20),
              ("  0x3F  ", Integer 63),
              ("  0x3f", Integer 63),
              ("0b101  ", Integer 5),
              ("0o77", Integer 63)
            ]

evaluationTests =
  let test = testTable runEval
   in do
        it "evaluates primitive types" $ test [(Integer 1, Right $ Integer 1), (String "test", Right $ String "test")]
        it "evaluates primitive functions" $ test [(func "+" [Integer 1, Integer 2], Right $ Integer 3)]

runEval :: LispVal -> IO (Either LispError LispVal)
runEval val = runExceptT $ do
  env <- liftIO primitiveBindings
  state <- liftIO nullState
  eval state env val

runParse :: String -> IO LispVal
runParse = return . extractValue . readExpr ""

testTable :: (Show b, Eq b) => (a -> IO b) -> [(a, b)] -> IO ()
testTable runTest [] = return ()
testTable runTest ((input, expected) : tests) = do
  res <- runTest input
  (res `shouldBe` expected) >> testTable runTest tests

list = List Nothing

atom = Atom Nothing

func f args = List Nothing (atom f : args)
