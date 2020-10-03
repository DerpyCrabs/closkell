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
    lvFromAST,
    lvToAST,
    lvRight,
    lvDown,
    lvUp,
    lvModify,
    lvModifyEnv,
    lvSet,
    lvSetEnv,
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

makeFunc :: Maybe String -> Env -> [LispVal] -> LispVal -> LispVal
makeFunc varargs env params body = Func (map show params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> LispVal -> LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> LispVal -> LispVal
makeVarArgs = makeFunc . Just . show

makeLet :: [(String, LispVal)] -> LispVal -> LispVal
makeLet binds expr = list $ (atom "let" : ((\(name, val) -> list [atom name, val]) <$> binds)) ++ [expr]

lvUp :: LVZipperTurn
lvUp (_, val, LVCrumb env pos ls rs : bs) = (env, List pos (ls ++ [val] ++ rs), bs)

lvDown :: LVZipperTurn
lvDown (env, List pos (val : rest), crumbs) = (env, val, LVCrumb env pos [] rest : crumbs)

lvRight :: LVZipperTurn
lvRight (env, val, LVCrumb crumbEnv pos ls (newVal : rs) : bs) = (env, newVal, LVCrumb crumbEnv pos (ls ++ [val]) rs : bs)

lvModify :: (LispVal -> LispVal) -> LVZipperTurn
lvModify f (env, val, crumbs) = (env, f val, crumbs)

lvModifyEnv :: (Env -> Env) -> LVZipperTurn
lvModifyEnv f (env, val, crumbs) = (f env, val, crumbs)

lvSet :: LispVal -> LVZipperTurn
lvSet = lvModify . const

lvSetEnv :: Env -> LVZipperTurn
lvSetEnv = lvModifyEnv . const

lvFromAST :: LispVal -> LVZipper
lvFromAST val = ([], val, [])

lvToAST :: LVZipper -> LispVal
lvToAST (_, val, []) = val
lvToAST z = lvToAST $ lvUp z
