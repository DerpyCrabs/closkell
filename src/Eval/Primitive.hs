module Eval.Primitive
  ( primitiveBindings,
  )
where

import Control.Monad.Except
import Data.Bifunctor (Bifunctor (first))
import Data.Env
import Data.Error
import Data.Value
import Parse (load)
import System.IO (IOMode (..), hClose, hGetLine, hPutStrLn, openFile, stdin, stdout)
import Types

primitiveBindings :: IO EnvRef
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+) (+)),
    ("-", numericBinop (-) (-)),
    ("*", numericBinop (*) (-)),
    ("/", integerBinop div),
    ("mod", integerBinop mod),
    ("quotient", integerBinop quot),
    ("remainder", integerBinop rem),
    ("=", numBoolBinop (==) (==)),
    ("<", numBoolBinop (<) (<)),
    (">", numBoolBinop (>) (>)),
    ("/=", numBoolBinop (/=) (/=)),
    (">=", numBoolBinop (>=) (>=)),
    ("<=", numBoolBinop (<=) (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string.concat", strStringBinop (++)),
    ("string.from", stringFrom),
    ("string.toList", stringToList),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("nth", nth),
    ("eq?", eq),
    ("list?", isList),
    ("atom?", isAtom),
    ("integer?", isInteger),
    ("string?", isString),
    ("character?", isCharacter),
    ("float?", isFloat),
    ("bool?", isBool),
    ("port?", isPort),
    ("dotted-list?", isDottedList)
  ]

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
          ("read-all", readAll)
        ]

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . Right . String

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [String obj] = writeProc [String obj, Port stdout]
writeProc [String obj, Port port] = liftIO $ hPutStrLn port obj >> (return $ Atom Nothing "nil")
writeProc (obj : _) = throwError $ TypeMismatch "string" obj

dumpProc :: [LispVal] -> IOThrowsError LispVal
dumpProc [obj] = writeProc [String (show obj), Port stdout]
dumpProc [obj, Port port] = writeProc [String (show obj), Port port]

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = fmap list $ load filename

integerBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integerBinop op [] = throwError $ NumArgs 2 []
integerBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
integerBinop op params = Integer . foldl1 op <$> mapM unpackInteger params

floatBinop :: (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
floatBinop op [] = throwError $ NumArgs 2 []
floatBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
floatBinop op params = Float . foldl1 op <$> mapM unpackFloat params

numericBinop :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
numericBinop opInt opFloat xs
  | all isInteger xs = integerBinop opInt xs
  | all isFloat xs = floatBinop opFloat xs
  | otherwise = throwError $ TypeMismatch "number" $ List Nothing xs
  where
    isInteger (Integer _) = True
    isInteger _ = False

    isFloat (Float _) = True
    isFloat _ = False

unpackInteger :: LispVal -> ThrowsError Integer
unpackInteger (Integer n) = return n
unpackInteger notInteger = throwError $ TypeMismatch "integer" notInteger

unpackFloat :: LispVal -> ThrowsError Double
unpackFloat (Float n) = return n
unpackFloat notFloat = throwError $ TypeMismatch "float" notFloat

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
  | otherwise = throwError $ TypeMismatch "number" $ List Nothing xs
  where
    isInteger (Integer _) = True
    isInteger _ = False

    isFloat (Float _) = True
    isFloat _ = False

intBoolBinop = boolBinop unpackInteger

floatBoolBinop = boolBinop unpackFloat

strBoolBinop = boolBinop unpackStr

strStringBinop = stringBinop unpackStr

boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List _ (x : xs)] = return x
car [DottedList _ (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

isList [(List _ _)] = return $ Bool True
isList _ = return $ Bool False

isAtom [(Atom _ _)] = return $ Bool True
isAtom _ = return $ Bool False

isInteger [(Integer _)] = return $ Bool True
isInteger _ = return $ Bool False

isString [(String _)] = return $ Bool True
isString _ = return $ Bool False

isCharacter [(Character _)] = return $ Bool True
isCharacter _ = return $ Bool False

isFloat [(Float _)] = return $ Bool True
isFloat _ = return $ Bool False

isBool [(Bool _)] = return $ Bool True
isBool _ = return $ Bool False

isPort [(Port _)] = return $ Bool True
isPort _ = return $ Bool False

isDottedList [(DottedList _ _ _)] = return $ Bool True
isDottedList _ = return $ Bool False

stringFrom :: [LispVal] -> ThrowsError LispVal
stringFrom [List _ xs] = return $ String $ foldl1 (++) $ map showString xs
  where
    showString (String s) = s
    showString (Character s) = [s]
    showString other = show other
stringFrom xs = stringFrom [List Nothing xs]

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [String str] = return $ list (Character <$> str)
stringToList other = throwError $ TypeMismatch "string" (list other)

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List _ (x : xs)] = return $ list xs
cdr [DottedList _ [_] x] = return x
cdr [DottedList _ (_ : xs) x] = return $ dottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List _ []] = return $ list [x1]
cons [x, List _ xs] = return $ list $ x : xs
cons [x, DottedList _ xs xlast] = return $ dottedList (x : xs) xlast
cons [x1, x2] = return $ dottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

nth :: [LispVal] -> ThrowsError LispVal
nth [Integer i, List _ xs] | length xs > (fromIntegral i) = return $ xs !! (fromIntegral i)
nth k = throwError $ Default ("nth error: " ++ show k)

eq :: [LispVal] -> ThrowsError LispVal
eq (x : xs) = return $ Bool $ (foldr (\elem acc -> elem == x && acc) True xs)
