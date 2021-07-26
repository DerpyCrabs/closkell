module Data.Value
  ( Value (..),
    list,
    dottedList,
    atom,
    int,
    float,
    func,
    fn,
    makeLet,
    vzFromValue,
    vzToValue,
    vzRight,
    vzDown,
    vzUp,
    vzModify,
    vzModifyEnv,
    vzSet,
    vzSetEnv,
  )
where

import Types

list :: [Value] -> Value
list = List Nothing

dottedList :: [Value] -> Value -> Value
dottedList = DottedList Nothing

atom :: String -> Value
atom = Atom Nothing

func :: String -> [Value] -> Value
func f args = Call (atom f : args)

int :: Integer -> Value
int = Integer

float :: Double -> Value
float = Float

fn :: [Value] -> [Value] -> Value
fn args body = Call (atom "fn" : list args : body)

makeLet :: [(String, Value)] -> Value -> Value
makeLet binds expr = Call $ (atom "let" : ((\(name, val) -> list [atom name, val]) <$> binds)) ++ [expr]

vzUp :: ValueZipperTurn
vzUp (_, val, ValueCrumb env ls rs : bs) = (env, Call (ls ++ [val] ++ rs), bs)

vzDown :: ValueZipperTurn
vzDown (env, Call (val : rest), crumbs) = (env, val, ValueCrumb env [] rest : crumbs)

vzRight :: ValueZipperTurn
vzRight (env, val, ValueCrumb crumbEnv ls (newVal : rs) : bs) = (crumbEnv, newVal, ValueCrumb crumbEnv (ls ++ [val]) rs : bs)

vzModify :: (Value -> Value) -> ValueZipperTurn
vzModify f (env, val, crumbs) = (env, f val, crumbs)

vzModifyEnv :: (Env -> Env) -> ValueZipperTurn
vzModifyEnv f (env, val, crumbs) = (f env, val, crumbs)

vzSet :: Value -> ValueZipperTurn
vzSet = vzModify . const

vzSetEnv :: Env -> ValueZipperTurn
vzSetEnv = vzModifyEnv . const

vzFromValue :: Value -> ValueZipper
vzFromValue val = ([], val, [])

vzToValue :: ValueZipper -> Value
vzToValue (_, val, []) = val
vzToValue z = vzToValue $ vzUp z
