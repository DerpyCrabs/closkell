{-# LANGUAGE ExistentialQuantification #-}

module Eval
  ( eval,
    primitiveBindings,
  )
where

import Control.Monad.Except
import Data.Env
import Data.Error
import Parse
import System.IO (IOMode (..), hClose, hGetLine, hPutStrLn, openFile, stdin, stdout)
import Types

nothingList = List Nothing

nothingDottedList = DottedList Nothing

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom _ id) = getVar env id
eval env (List _ [Atom _ "quote", val]) = return val
eval env (List _ [Atom _ "if", pred, conseq, alt]) =
  do
    result <- eval env pred
    case result of
      Bool False -> eval env alt
      Bool True -> eval env conseq
      _ -> throwError $ TypeMismatch "boolean" result
eval env (List _ [Atom _ "set!", Atom _ var, form]) = eval env form >>= setVar env var
eval env (List _ [Atom _ "define", Atom _ var, form]) = eval env form >>= defineVar env var
eval env (List _ (Atom _ "define" : List _ (Atom _ var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List _ (Atom _ "define" : DottedList _ (Atom _ var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List _ (Atom _ "lambda" : List _ params : body)) =
  makeNormalFunc env params body
eval env (List _ (Atom _ "lambda" : DottedList _ params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List _ (Atom _ "lambda" : varargs@(Atom _ _) : body)) =
  makeVarArgs varargs env [] body
eval env (List _ [Atom _ "load", String filename]) =
  load filename >>= fmap last . mapM (eval env)
eval env (List _ (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, nothingList $ remainingArgs)]
      Nothing -> return env
apply (IOFunc func) args = func args

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

makeFunc varargs env params body = return $ Func (map show params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarArgs = makeFunc . Just . show

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("concat", strStringBinop (++)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)
  ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll)
  ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List _ args] = apply func args
applyProc (func : args) = apply func args

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
writeProc [String obj, Port port] = liftIO $ hPutStrLn port obj >> (return $ Bool True)
writeProc (obj : _) = throwError $ TypeMismatch "string" obj

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . (readExprList filename)

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM nothingList $ load filename

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)]
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return . fst . head $ parsed
unpackNum (List _ [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

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

numBoolBinop = boolBinop unpackNum

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

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom _ arg1), (Atom _ arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList _ xs x), (DottedList _ ys y)] = eqv [nothingList $ xs ++ [x], nothingList $ ys ++ [y]]
eqv [(List _ arg1), (List _ arg2)] =
  return $
    Bool $
      (length arg1 == length arg2)
        && (all eqvPair $ zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
      Left err -> False
      Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <-
    liftM or $
      mapM
        (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList
