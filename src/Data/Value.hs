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
    lvEnd,
    lvNext,
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

lvUp :: LispValZipper -> LispValZipper
lvUp (_, Just val, LispValCrumb env pos ls rs : bs) = (env, Just $ List pos (ls ++ [val] ++ rs), bs)
lvUp (_, _, crumbs) = lvEnd

lvDown :: LispValZipper -> LispValZipper
lvDown (env, Just (List pos (val : rest)), crumbs) = (env, Just val, LispValCrumb env pos [] rest : crumbs)
lvDown (_, _, crumbs) = lvEnd

lvRight :: LispValZipper -> LispValZipper
lvRight (env, Just val, LispValCrumb crumbEnv pos ls (newVal : rs) : bs) = (env, Just newVal, LispValCrumb crumbEnv pos (ls ++ [val]) rs : bs)
lvRight (_, _, crumbs) = lvEnd

lvModify :: (LispVal -> LispVal) -> LispValZipper -> LispValZipper
lvModify f (env, val, crumbs) = (env, f <$> val, crumbs)

lvModifyEnv :: (Env -> Env) -> LispValZipper -> LispValZipper
lvModifyEnv f (env, val, crumbs) = (f env, val, crumbs)

lvSet = lvModify . const

lvSetEnv = lvModifyEnv . const

lvFromAST :: LispVal -> LispValZipper
lvFromAST val = ([], Just val, [])

lvToAST :: LispValZipper -> LispVal
lvToAST zipper@(_, Just val, crumbs) =
  let upZipper = lvUp zipper
   in case upZipper of
        (_, Just _, _) -> lvToAST upZipper
        (_, Nothing, _) -> val

lvEnd :: LispValZipper
lvEnd = ([], Nothing, [])

lvNext :: LispValZipper -> LispValZipper
lvNext z = case lvRight z of
  z@(_, Just _, _) -> z
  _ -> lvUp z
