module Eval.Primitive
  ( primitiveBindings,
    isPrimitive,
    primitives,
    ioPrimitives,
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
primitiveBindings = map (makeFunc IOFunc) (stripType <$> ioPrimitives) ++ map (makeFunc PrimitiveFunc) (stripType <$> primitives)
  where
    makeFunc constructor (var, func) = (var, constructor var func)

tNumber = TSum [TInteger, TFloat]

stripType (name, val, _) = (name, val)

primitives :: [(String, [LispVal] -> ThrowsError LispVal, LispType)]
primitives =
  [ ("+", numericBinop (+) (+), TFunc [tNumber, tNumber] Nothing tNumber),
    ("-", numericBinop (-) (-), TFunc [tNumber, tNumber] Nothing tNumber),
    ("*", numericBinop (*) (*), TFunc [tNumber, tNumber] Nothing tNumber),
    ("/", integerBinop div, TFunc [TInteger, TInteger] Nothing TInteger),
    ("rem", integerBinop rem, TFunc [TInteger, TInteger] Nothing TInteger),
    ("==", numBoolBinop (==) (==), TFunc [tNumber, tNumber] Nothing TBool),
    ("/=", numBoolBinop (/=) (/=), TFunc [tNumber, tNumber] Nothing TBool),
    ("<", numBoolBinop (<) (<), TFunc [tNumber, tNumber] Nothing TBool),
    (">", numBoolBinop (>) (>), TFunc [tNumber, tNumber] Nothing TBool),
    ("<=", numBoolBinop (<=) (<=), TFunc [tNumber, tNumber] Nothing TBool),
    (">=", numBoolBinop (>=) (>=), TFunc [tNumber, tNumber] Nothing TBool),
    ("&&", boolBoolBinop (&&), TFunc [TBool, TBool] Nothing TBool),
    ("||", boolBoolBinop (||), TFunc [TBool, TBool] Nothing TBool),
    ("!", notOp, TFunc [TBool] Nothing TBool),
    ("string.concat", strStringBinop (++), TFunc [TString, TString] Nothing TString),
    ("string.from", stringFrom, TFunc [TVar "a"] Nothing TString),
    ("string.toList", stringToList, TFunc [TString] Nothing (TList TCharacter)),
    ("car", car, TFunc [TList (TVar "a")] Nothing (TVar "a")),
    ("cdr", cdr, TFunc [TList (TVar "a")] Nothing (TList (TVar "a"))),
    ("cons", cons, TFunc [TVar "a", TList (TVar "a")] Nothing (TList (TVar "a"))),
    ("get", get, TFunc [TVar "a", TList $ TSum [TVar "a", TVar "b"]] Nothing (TVar "b")),
    ("nth", nth, TFunc [TInteger, TList (TVar "a")] Nothing (TVar "a")),
    ("eq?", eq, TFunc [TVar "a", TVar "a"] Nothing TBool),
    ("list?", isList, TFunc [TVar "a"] Nothing TBool),
    ("atom?", isAtom, TFunc [TVar "a"] Nothing TBool),
    ("integer?", isInteger, TFunc [TVar "a"] Nothing TBool),
    ("float?", isFloat, TFunc [TVar "a"] Nothing TBool),
    ("string?", isString, TFunc [TVar "a"] Nothing TBool),
    ("character?", isCharacter, TFunc [TVar "a"] Nothing TBool),
    ("bool?", isBool, TFunc [TVar "a"] Nothing TBool),
    ("dotted-list?", isDottedList, TFunc [TVar "a"] Nothing TBool),
    ("do", doFunc, TFunc [] (Just $ TSum [TVar "a", TUnit]) (TVar "a"))
  ]

isPrimitive :: String -> Bool
isPrimitive str = str `elem` ((fst . stripType <$> primitives) ++ (fst . stripType <$> ioPrimitives))

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal, LispType)]
ioPrimitives =
  [ ("io.read", readProc, TFunc [] Nothing TString),
    ("io.write", writeProc, TFunc [TString] Nothing TUnit),
    ("io.panic", panic, TFunc [TVar "a"] Nothing (TVar "b"))
  ]

panic :: [LispVal] -> IOThrowsError LispVal
panic [err] = liftThrows $ throwError $ FromCode err

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = liftIO getLine >>= liftThrows . Right . String

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [String obj] = liftIO $ putStrLn obj >> return Unit

integerBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integerBinop op [Integer a, Integer b] = return $ Integer $ op a b

floatBinop :: (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
floatBinop op [Float a, Float b] = return $ Float $ op a b

unpackInteger :: LispVal -> ThrowsError Integer
unpackInteger (Integer n) = return n

unpackFloat :: LispVal -> ThrowsError Double
unpackFloat (Float n) = return n

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [arg, arg2] =
  do
    left <- unpacker arg
    right <- unpacker arg2
    return $ Bool $ left `op` right

stringBinop :: (LispVal -> ThrowsError a) -> (a -> a -> String) -> [LispVal] -> ThrowsError LispVal
stringBinop unpacker op [arg, arg2] =
  do
    left <- unpacker arg
    right <- unpacker arg2
    return $ String $ left `op` right

createNumericBinop ::
  ((Integer -> Integer -> a) -> [LispVal] -> ThrowsError LispVal) ->
  ((Double -> Double -> b) -> [LispVal] -> ThrowsError LispVal) ->
  (Integer -> Integer -> a) ->
  (Double -> Double -> b) ->
  [LispVal] ->
  ThrowsError LispVal
createNumericBinop intBinop floatBinop opInt opFloat xs
  | all isInteger xs = intBinop opInt xs
  | all isFloat xs = floatBinop opFloat xs
  | otherwise = floatBinop opFloat (toFloat <$> xs)
  where
    isInteger (Integer _) = True
    isInteger _ = False

    isFloat (Float _) = True
    isFloat _ = False

    toFloat (Integer i) = Float $ fromInteger i
    toFloat f = f

numBoolBinop :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = createNumericBinop intBoolBinop floatBoolBinop

numericBinop :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
numericBinop = createNumericBinop integerBinop floatBinop

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

isDottedList [DottedList {}] = return $ Bool True
isDottedList _ = return $ Bool False

doFunc :: [LispVal] -> ThrowsError LispVal
doFunc [] = throwError $ NumArgs 0 []
doFunc args = return $ last args

stringFrom :: [LispVal] -> ThrowsError LispVal
stringFrom [x] = return $ String $ show x
stringFrom xs = stringFrom [list xs]

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [String str] = return $ list (Character <$> str)

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List _ (_ : xs)] = return $ list xs

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List _ xs] = return $ list $ x : xs

car :: [LispVal] -> ThrowsError LispVal
car [List _ (x : _)] = return x

nth :: [LispVal] -> ThrowsError LispVal
nth [Integer i, List _ xs] | length xs > fromIntegral i = return $ xs !! fromIntegral i

eq :: [LispVal] -> ThrowsError LispVal
eq [arg, arg2] = return $ Bool (arg == arg2)
