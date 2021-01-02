module Data.Value
  ( LispVal (..),
    list,
    dottedList,
    atom,
    int,
    float,
    func,
    fn,
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
    mapToList,
    mapFromList,
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
func f args = Call (atom f : args)

int :: Integer -> LispVal
int = Integer

float :: Double -> LispVal
float = Float

fn :: [LispVal] -> Maybe LispVal -> [LispVal] -> LispVal
fn args Nothing body = Call (atom "fn" : list args : body)
fn args (Just vararg) body = Call (atom "fn" : dottedList args vararg : body)

makeFunc :: Maybe String -> Env -> [LispVal] -> LispVal -> LispVal
makeFunc varargs env params body = Func (map show params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> LispVal -> LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> LispVal -> LispVal
makeVarArgs = makeFunc . Just . show

makeLet :: [(String, LispVal)] -> LispVal -> LispVal
makeLet binds expr = Call $ (atom "let" : ((\(name, val) -> list [atom name, val]) <$> binds)) ++ [expr]

lvUp :: LVZipperTurn
lvUp (_, val, LVCrumb env ls rs : bs) = (env, Call (ls ++ [val] ++ rs), bs)

lvDown :: LVZipperTurn
lvDown (env, Call (val : rest), crumbs) = (env, val, LVCrumb env [] rest : crumbs)

lvRight :: LVZipperTurn
lvRight (env, val, LVCrumb crumbEnv ls (newVal : rs) : bs) = (crumbEnv, newVal, LVCrumb crumbEnv (ls ++ [val]) rs : bs)

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

mapFromList :: [LispVal] -> LispVal
mapFromList (key : value : rest) = Map ((key, value) : (\(Map binds) -> binds) (mapFromList rest))
mapFromList [] = Map []

mapToList :: LispVal -> [LispVal]
mapToList (Map binds) = concat $ (\(key, value) -> [key, value]) <$> binds
