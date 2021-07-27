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

primitives :: [(String, [Value] -> ThrowsError Value, Type)]
primitives =
  [ ("+", numericBinop (+) (+), TFunc [tNumber, tNumber] tNumber),
    ("-", numericBinop (-) (-), TFunc [tNumber, tNumber] tNumber),
    ("*", numericBinop (*) (*), TFunc [tNumber, tNumber] tNumber),
    ("/", integerBinop div, TFunc [TInteger, TInteger] TInteger),
    ("rem", integerBinop rem, TFunc [TInteger, TInteger] TInteger),
    ("==", numBoolBinop (==) (==), TFunc [tNumber, tNumber] TBool),
    ("/=", numBoolBinop (/=) (/=), TFunc [tNumber, tNumber] TBool),
    ("<", numBoolBinop (<) (<), TFunc [tNumber, tNumber] TBool),
    (">", numBoolBinop (>) (>), TFunc [tNumber, tNumber] TBool),
    ("<=", numBoolBinop (<=) (<=), TFunc [tNumber, tNumber] TBool),
    (">=", numBoolBinop (>=) (>=), TFunc [tNumber, tNumber] TBool),
    ("&&", boolBoolBinop (&&), TFunc [TBool, TBool] TBool),
    ("||", boolBoolBinop (||), TFunc [TBool, TBool] TBool),
    ("!", notOp, TFunc [TBool] TBool),
    ("string.concat", strStringBinop (++), TFunc [TString, TString] TString),
    ("string.from", stringFrom, TFunc [TVar "a"] TString),
    ("string.toList", stringToList, TFunc [TString] (TList TCharacter)),
    ("car", car, TFunc [TList (TVar "a")] (TVar "a")),
    ("cdr", cdr, TFunc [TList (TVar "a")] (TList (TVar "a"))),
    ("cons", cons, TFunc [TVar "a", TList (TVar "a")] (TList (TVar "a"))),
    ("get", get, TFunc [TVar "a", TMap (TVar "a") (TVar "b")] (TVar "b")),
    ("nth", nth, TFunc [TInteger, TList (TVar "a")] (TVar "a")),
    ("eq?", eq, TFunc [TVar "a", TVar "a"] TBool),
    ("list?", isList, TFunc [TVar "a"] TBool),
    ("atom?", isAtom, TFunc [TVar "a"] TBool),
    ("integer?", isInteger, TFunc [TVar "a"] TBool),
    ("float?", isFloat, TFunc [TVar "a"] TBool),
    ("string?", isString, TFunc [TVar "a"] TBool),
    ("character?", isCharacter, TFunc [TVar "a"] TBool),
    ("bool?", isBool, TFunc [TVar "a"] TBool),
    ("dotted-list?", isDottedList, TFunc [TVar "a"] TBool),
    ("do", doFunc, TFunc [TAny, TVar "a"] (TVar "a"))
  ]

isPrimitive :: String -> Bool
isPrimitive str = str `elem` ((fst . stripType <$> primitives) ++ (fst . stripType <$> ioPrimitives))

ioPrimitives :: [(String, [Value] -> IOThrowsError Value, Type)]
ioPrimitives =
  [ ("io.read", readProc, TFunc [] TString),
    ("io.write", writeProc, TFunc [TString] TUnit),
    ("io.panic", panic, TFunc [TVar "a"] (TVar "b"))
  ]

panic :: [Value] -> IOThrowsError Value
panic [err] = liftThrows $ throwError $ FromCode err

readProc :: [Value] -> IOThrowsError Value
readProc [] = liftIO getLine >>= liftThrows . Right . String

writeProc :: [Value] -> IOThrowsError Value
writeProc [String obj] = liftIO $ putStrLn obj >> return Unit

integerBinop :: (Integer -> Integer -> Integer) -> [Value] -> ThrowsError Value
integerBinop op [Integer a, Integer b] = return $ Integer $ op a b

floatBinop :: (Double -> Double -> Double) -> [Value] -> ThrowsError Value
floatBinop op [Float a, Float b] = return $ Float $ op a b

unpackInteger :: Value -> ThrowsError Integer
unpackInteger (Integer n) = return n

unpackFloat :: Value -> ThrowsError Double
unpackFloat (Float n) = return n

boolBinop :: (Value -> ThrowsError a) -> (a -> a -> Bool) -> [Value] -> ThrowsError Value
boolBinop unpacker op [arg, arg2] =
  do
    left <- unpacker arg
    right <- unpacker arg2
    return $ Bool $ left `op` right

stringBinop :: (Value -> ThrowsError a) -> (a -> a -> String) -> [Value] -> ThrowsError Value
stringBinop unpacker op [arg, arg2] =
  do
    left <- unpacker arg
    right <- unpacker arg2
    return $ String $ left `op` right

createNumericBinop ::
  ((Integer -> Integer -> a) -> [Value] -> ThrowsError Value) ->
  ((Double -> Double -> b) -> [Value] -> ThrowsError Value) ->
  (Integer -> Integer -> a) ->
  (Double -> Double -> b) ->
  [Value] ->
  ThrowsError Value
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

numBoolBinop :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> [Value] -> ThrowsError Value
numBoolBinop = createNumericBinop intBoolBinop floatBoolBinop

numericBinop :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> [Value] -> ThrowsError Value
numericBinop = createNumericBinop integerBinop floatBinop

intBoolBinop = boolBinop unpackInteger

floatBoolBinop = boolBinop unpackFloat

strStringBinop = stringBinop unpackStr

boolBoolBinop = boolBinop unpackBool

unpackStr :: Value -> ThrowsError String
unpackStr (String s) = return s

unpackBool :: Value -> ThrowsError Bool
unpackBool (Bool b) = return b

notOp :: [Value] -> ThrowsError Value
notOp [v] = Bool . not <$> unpackBool v

get :: [Value] -> ThrowsError Value
get [key, Map (k : val : rest)]
  | key == k = return val
  | otherwise = get [key, Map rest]

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

doFunc :: [Value] -> ThrowsError Value
doFunc [] = throwError $ NumArgs 0 []
doFunc args = return $ last args

stringFrom :: [Value] -> ThrowsError Value
stringFrom [x] = return $ String $ show x
stringFrom xs = stringFrom [list xs]

stringToList :: [Value] -> ThrowsError Value
stringToList [String str] = return $ list (Character <$> str)

cdr :: [Value] -> ThrowsError Value
cdr [List _ (_ : xs)] = return $ list xs

cons :: [Value] -> ThrowsError Value
cons [x, List _ xs] = return $ list $ x : xs

car :: [Value] -> ThrowsError Value
car [List _ (x : _)] = return x

nth :: [Value] -> ThrowsError Value
nth [Integer i, List _ xs] | length xs > fromIntegral i = return $ xs !! fromIntegral i

eq :: [Value] -> ThrowsError Value
eq [arg, arg2] = return $ Bool (arg == arg2)
