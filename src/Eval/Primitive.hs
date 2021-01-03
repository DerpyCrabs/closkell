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

primitives :: [(String, [Value] -> ThrowsError Value, LispType)]
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
    ("get", get, TFunc [TVar "a", TMap (TVar "a") (TVar "b")] Nothing (TVar "b")),
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

ioPrimitives :: [(String, [Value] -> IOThrowsError Value, LispType)]
ioPrimitives =
  [ ("io.read", readProc, TFunc [] Nothing TString),
    ("io.write", writeProc, TFunc [TString] Nothing TUnit),
    ("io.panic", panic, TFunc [TVar "a"] Nothing (TVar "b"))
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
