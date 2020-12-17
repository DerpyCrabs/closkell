module Eval.Primitive
  ( primitiveBindings,
    isPrimitive,
    primitives,
  )
where

import Control.Monad.Except
import Data.Bifunctor (Bifunctor (first))
import Data.Error
import Data.Value
import Parse (load)
import System.IO (IOMode (..), hClose, hGetLine, hPutStrLn, openFile, stdin, stdout)
import Types

primitiveBindings :: Env
primitiveBindings = map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) (stripType <$> primitives)
  where
    makeFunc constructor (var, func) = (var, constructor var func)

tNumber = TSum [TInteger, TFloat]

stripType (name, val, _) = (name, val)

primitives :: [(String, [LispVal] -> ThrowsError LispVal, LispType)]
primitives =
  [ ("+", numericBinop (+) (+), TFunc [] (Just tNumber) TFloat),
    ("-", numericBinop (-) (-), TFunc [] (Just tNumber) TFloat),
    ("*", numericBinop (*) (-), TFunc [] (Just tNumber) TFloat),
    ("/", integerBinop div, TFunc [] (Just TInteger) TInteger),
    ("mod", integerBinop mod, TFunc [] (Just TInteger) TInteger),
    ("quotient", integerBinop quot, TFunc [] (Just TInteger) TInteger),
    ("remainder", integerBinop rem, TFunc [] (Just TInteger) TInteger),
    ("==", numBoolBinop (==) (==), TFunc [] (Just tNumber) TBool),
    ("<", numBoolBinop (<) (<), TFunc [] (Just tNumber) TBool),
    (">", numBoolBinop (>) (>), TFunc [] (Just tNumber) TBool),
    ("/=", numBoolBinop (/=) (/=), TFunc [] (Just tNumber) TBool),
    (">=", numBoolBinop (>=) (>=), TFunc [] (Just tNumber) TBool),
    ("<=", numBoolBinop (<=) (<=), TFunc [] (Just tNumber) TBool),
    ("&&", boolBoolBinop (&&), TFunc [] (Just TBool) TBool),
    ("||", boolBoolBinop (||), TFunc [] (Just TBool) TBool),
    ("!", notOp, TFunc [TBool] Nothing TBool),
    ("string.concat", strStringBinop (++), TFunc [] (Just TString) TString),
    ("string.from", stringFrom, TFunc [] (Just $ TSum [TInteger, TFloat, TBool, TCharacter, TString, TList (TVar "a")]) TString),
    ("string.toList", stringToList, TFunc [TString] Nothing (TList TCharacter)),
    ("car", car, TFunc [TList (TVar "a")] Nothing (TVar "a")),
    ("cdr", cdr, TFunc [TList (TVar "a")] Nothing (TList (TVar "a"))),
    ("cons", cons, TFunc [TVar "a", TList (TVar "a")] Nothing (TList (TVar "a"))),
    ("get", get, TFunc [TVar "a", TList $ TSum [TVar "a", TVar "b"]] Nothing (TVar "b")),
    ("nth", nth, TFunc [TInteger, TList (TVar "a")] Nothing (TVar "a")),
    ("eq?", eq, TFunc [] (Just (TVar "a")) TBool),
    ("list?", isList, TFunc [TVar "a"] Nothing TBool),
    ("atom?", isAtom, TFunc [TVar "a"] Nothing TBool),
    ("integer?", isInteger, TFunc [TVar "a"] Nothing TBool),
    ("string?", isString, TFunc [TVar "a"] Nothing TBool),
    ("character?", isCharacter, TFunc [TVar "a"] Nothing TBool),
    ("float?", isFloat, TFunc [TVar "a"] Nothing TBool),
    ("bool?", isBool, TFunc [TVar "a"] Nothing TBool),
    ("port?", isPort, TFunc [TVar "a"] Nothing TBool),
    ("dotted-list?", isDottedList, TFunc [TVar "a"] Nothing TBool),
    ("do", doFunc, TFunc [] (Just $ TSum [TVar "a", TUnit]) (TVar "a"))
  ]

isPrimitive :: String -> Bool
isPrimitive str = str `elem` ((fst . stripType <$> primitives) ++ (fst <$> ioPrimitives))

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  first ("io." ++)
    <$> [ ("open-input-file", makePort ReadMode),
          ("open-output-file", makePort WriteMode),
          ("close-input-port", closePort),
          ("close-output-port", closePort),
          ("read", readProc),
          ("write", writeProc),
          ("dump", dumpProc),
          ("read-contents", readContents),
          ("read-all", readAll),
          ("throw", throw)
        ]

throw :: [LispVal] -> IOThrowsError LispVal
throw [err] = liftThrows $ throwError $ FromCode err
throw other = liftThrows $ throwError $ NumArgs 1 other

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . Right . String

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [String obj] = writeProc [String obj, Port stdout]
writeProc [String obj, Port port] = liftIO $ hPutStrLn port obj >> return (Atom Nothing "nil")

dumpProc :: [LispVal] -> IOThrowsError LispVal
dumpProc [obj] = writeProc [String (show obj), Port stdout]
dumpProc [obj, Port port] = writeProc [String (show obj), Port port]

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = list <$> load filename

integerBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integerBinop _ [] = throwError $ NumArgs 2 []
integerBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
integerBinop op params = Integer . foldl1 op <$> mapM unpackInteger params

floatBinop :: (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
floatBinop _ [] = throwError $ NumArgs 2 []
floatBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
floatBinop op params = Float . foldl1 op <$> mapM unpackFloat params

numericBinop :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
numericBinop opInt opFloat xs
  | all isInteger xs = integerBinop opInt xs
  | all isFloat xs = floatBinop opFloat xs
  where
    isInteger (Integer _) = True
    isInteger _ = False

    isFloat (Float _) = True
    isFloat _ = False

unpackInteger :: LispVal -> ThrowsError Integer
unpackInteger (Integer n) = return n

unpackFloat :: LispVal -> ThrowsError Double
unpackFloat (Float n) = return n

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker $ head $ tail args
      return $ Bool $ left `op` right

stringBinop :: (LispVal -> ThrowsError a) -> (a -> a -> String) -> [LispVal] -> ThrowsError LispVal
stringBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker $ head $ tail args
      return $ String $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop opInt opFloat xs
  | all isInteger xs = intBoolBinop opInt xs
  | all isFloat xs = floatBoolBinop opFloat xs
  where
    isInteger (Integer _) = True
    isInteger _ = False

    isFloat (Float _) = True
    isFloat _ = False

intBoolBinop = boolBinop unpackInteger

floatBoolBinop = boolBinop unpackFloat

strStringBinop = stringBinop unpackStr

boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b

notOp :: [LispVal] -> ThrowsError LispVal
notOp [v] = Bool . not <$> unpackBool v

get :: [LispVal] -> ThrowsError LispVal
get [String key, List _ (String k : val : rest)]
  | key == k = return val
  | otherwise = get [String key, list rest]
get badArgList = throwError $ NumArgs 2 badArgList

isList [List _ _] = return $ Bool True
isList _ = return $ Bool False

isAtom [Atom _ _] = return $ Bool True
isAtom _ = return $ Bool False

isInteger [Integer _] = return $ Bool True
isInteger _ = return $ Bool False

isString [String _] = return $ Bool True
isString _ = return $ Bool False

isCharacter [Character _] = return $ Bool True
isCharacter _ = return $ Bool False

isFloat [Float _] = return $ Bool True
isFloat _ = return $ Bool False

isBool [Bool _] = return $ Bool True
isBool _ = return $ Bool False

isPort [Port _] = return $ Bool True
isPort _ = return $ Bool False

isDottedList [DottedList {}] = return $ Bool True
isDottedList _ = return $ Bool False

doFunc :: [LispVal] -> ThrowsError LispVal
doFunc [] = throwError $ NumArgs 0 []
doFunc args = return $ last args

stringFrom :: [LispVal] -> ThrowsError LispVal
stringFrom [List _ xs] = return $ String $ foldl1 (++) $ map showString xs
  where
    showString (String s) = s
    showString (Character s) = [s]
    showString other = show other
stringFrom xs = stringFrom [List Nothing xs]

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [String str] = return $ list (Character <$> str)

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List _ (_ : xs)] = return $ list xs
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List _ xs] = return $ list $ x : xs
cons badArgList = throwError $ NumArgs 2 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List _ (x : _)] = return x
car badArgList = throwError $ NumArgs 1 badArgList

nth :: [LispVal] -> ThrowsError LispVal
nth [Integer i, List _ xs] | length xs > fromIntegral i = return $ xs !! fromIntegral i
nth k = throwError $ Default ("nth error: " ++ show k)

eq :: [LispVal] -> ThrowsError LispVal
eq (x : xs) = return $ Bool (foldr (\elem acc -> elem == x && acc) True xs)
