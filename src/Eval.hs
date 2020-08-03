{-# LANGUAGE ExistentialQuantification #-}

module Eval
  ( eval,
    primitiveBindings,
  )
where

import Control.Monad.Except
import Data.Bifunctor (Bifunctor (first))
import Data.Env
import Data.Error
import Data.State
import Parse
import System.IO (IOMode (..), hClose, hGetLine, hPutStrLn, openFile, stdin, stdout)
import Types

nothingList = List Nothing

nothingDottedList = DottedList Nothing

eval :: StateRef -> EnvRef -> LispVal -> IOThrowsError LispVal
eval state env val@(String _) = return val
eval state env val@(Character _) = return val
eval state env val@(Integer _) = return val
eval state env val@(Float _) = return val
eval state env val@(Bool _) = return val
eval state env (Atom _ id) = do
  isMacro <- liftIO $ isBound envMacros env id
  if isMacro
    then do
      return (Atom Nothing id)
    else do
      getVar env id
eval state env (List _ [Atom _ "quote", val]) = evalUnquote state env val
eval state env (List _ [Atom _ "apply", func, args@(List _ _)]) = do
  func <- eval state env func
  (List _ args) <- eval state env args
  apply state func args
eval state env (List _ (Atom _ "apply" : func : args)) = do
  func <- eval state env func
  args <- mapM (eval state env) args
  apply state func args
eval state env (List _ [Atom _ "unquote", val]) = eval state env val >>= eval state env
eval state env (List _ [Atom _ "gensym"]) = do
  counter <- liftIO $ nextGensymCounter state
  return $ Atom Nothing (show counter)
eval state env (List _ [Atom _ "gensym", (String prefix)]) = do
  counter <- liftIO $ nextGensymCounter state
  return $ Atom Nothing ((prefix ++) $ show counter)
eval state env (List _ [Atom _ "if", pred, conseq, alt]) =
  do
    result <- eval state env pred
    case result of
      Bool False -> eval state env alt
      Bool True -> eval state env conseq
      _ -> throwError $ TypeMismatch "boolean" result
eval state env (List _ [Atom _ "set!", Atom _ var, form]) = eval state env form >>= setVar env var
eval state env (List _ [Atom _ "io.throw!", obj]) = eval state env obj >>= throwError . FromCode
eval state env (List _ (Atom _ "defmacro" : Atom _ name : body)) = return (Func [] (Just "body") body env) >>= defineMacro env name
eval state env (List _ (Atom _ "begin" : body)) = nothingList <$> mapM (eval state env) body
eval state env (List _ [Atom _ "define", Atom _ var, form]) = eval state env form >>= defineVar env var
eval state env (List _ (Atom _ "define" : List _ (Atom _ var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval state env (List _ (Atom _ "define" : DottedList _ (Atom _ var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval state env (List _ (Atom _ "lambda" : (List _ [Atom _ "quote", List _ []]) : body)) =
  makeNormalFunc env [] body
eval state env (List _ (Atom _ "lambda" : List _ params : body)) =
  makeNormalFunc env params body
eval state env (List _ (Atom _ "lambda" : DottedList _ params varargs : body)) =
  makeVarArgs varargs env params body
eval state env (List _ (Atom _ "lambda" : varargs@(Atom _ _) : body)) =
  makeVarArgs varargs env [] body
eval state env (List _ [Atom _ "load", String filename]) =
  load filename >>= fmap last . mapM (eval state env)
eval state env (List _ (function : args)) = do
  func <- eval state env function
  case func of
    (Atom _ name) -> do
      isMacro <- liftIO $ isBound envMacros env name
      if isMacro
        then do
          macro <- getMacro env name
          result <- apply state macro args
          eval state env result
        else do
          argVals <- mapM (eval state env) args
          apply state func argVals
    _ -> do
      argVals <- mapM (eval state env) args
      apply state func argVals
eval state env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalUnquote :: StateRef -> EnvRef -> LispVal -> IOThrowsError LispVal
evalUnquote state env (List pos exprs) = List pos <$> concat <$> mapM evalUnquoteSplicing exprs
  where
    evalUnquoteSplicing (List _ [Atom _ "unquote-splicing", vals]) = do
      vals <- eval state env vals
      case vals of
        (List _ vals) -> return vals
        _ -> throwError $ Default "failed unquote-splicing"
    evalUnquoteSplicing (List _ [Atom _ "unquote", val]) = (\el -> [el]) <$> eval state env val
    evalUnquoteSplicing other = (\el -> [el]) <$> evalUnquote state env other
evalUnquote state env other = return other

apply :: StateRef -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply state (PrimitiveFunc func) args = liftThrows $ func args
apply state (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval state env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, nothingList $ remainingArgs)]
      Nothing -> return env
apply state (IOFunc func) args = func args
apply state k _ = throwError $ Default $ "Invalid apply " ++ show k

-- TODO error on applying not function

primitiveBindings :: IO EnvRef
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

makeFunc varargs env params body = return $ Func (map show params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarArgs = makeFunc . Just . show

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
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . (readExprList filename)

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM nothingList $ load filename

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
stringToList [String str] = return $ nothingList (Character <$> str)
stringToList other = throwError $ TypeMismatch "string" (nothingList other)

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List _ (x : xs)] = return $ nothingList xs
cdr [DottedList _ [_] x] = return x
cdr [DottedList _ (_ : xs) x] = return $ nothingDottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List _ []] = return $ nothingList [x1]
cons [x, List _ xs] = return $ nothingList $ x : xs
cons [x, DottedList _ xs xlast] = return $ nothingDottedList (x : xs) xlast
cons [x1, x2] = return $ nothingDottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

nth :: [LispVal] -> ThrowsError LispVal
nth [Integer i, List _ xs] | length xs > (fromIntegral i) = return $ xs !! (fromIntegral i)
nth k = throwError $ Default ("nth error: " ++ show k)

eq :: [LispVal] -> ThrowsError LispVal
eq (x : xs) = return $ Bool $ (foldr (\elem acc -> elem == x && acc) True xs)
