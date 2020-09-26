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

lvUp :: LispValZipper -> LispValZipper
lvUp (Just val, LispValCrumb pos ls rs : bs) = (Just $ List pos (ls ++ [val] ++ rs), bs)
lvUp (_, crumbs) = (Nothing, crumbs)

lvDown :: LispValZipper -> LispValZipper
lvDown (Just (List pos (val : rest)), crumbs) = (Just val, LispValCrumb pos [] rest : crumbs)
lvDown (_, crumbs) = (Nothing, crumbs)

lvRight :: LispValZipper -> LispValZipper
lvRight (Just val, LispValCrumb pos ls (newVal : rs) : bs) = (Just newVal, LispValCrumb pos (ls ++ [val]) rs : bs)
lvRight (_, crumbs) = (Nothing, crumbs)

lvModify :: (LispVal -> LispVal) -> LispValZipper -> LispValZipper
lvModify f (Just val, crumbs) = (Just . f $ val, crumbs)
lvModify _ other = other

lvFromAST :: LispVal -> LispValZipper
lvFromAST val = (Just val, [])

lvToAST :: LispValZipper -> LispVal
lvToAST zipper@(Just val, crumbs) =
  let upZipper = lvUp zipper
   in case upZipper of
        (Just _, _) -> lvToAST upZipper
        (Nothing, _) -> val
