module Data.Value
  ( LispVal (..),
    list,
    dottedList,
    atom,
    int,
    func,
    lambda,
    makeNormalFunc,
    makeVarArgs,
    makeLet,
  )
where

import Types

list :: [LispVal] -> LispVal
list = List Nothing

dottedList :: [LispVal] -> LispVal -> LispVal
dottedList = DottedList Nothing

atom :: String -> LispVal
atom = Atom Nothing

func :: String -> [LispVal] -> LispVal
func f args = List Nothing (atom f : args)

int :: Integer -> LispVal
int = Integer

lambda :: [LispVal] -> Maybe LispVal -> [LispVal] -> LispVal
lambda args Nothing body = list (atom "lambda" : list args : body)
lambda args (Just vararg) body = list (atom "lambda" : dottedList args vararg : body)

makeFunc :: Maybe String -> EnvRef -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map show params) varargs body env

makeNormalFunc :: EnvRef -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> EnvRef -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . show

makeLet :: [(String, LispVal)] -> LispVal -> LispVal
makeLet binds expr = list $ (atom "let" : ((\(name, val) -> list [atom name, val]) <$> binds)) ++ [expr]
