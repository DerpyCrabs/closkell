module Compile.TypeSystem (typeSystem) where

import Control.Applicative
import Control.Monad (unless, when, zipWithM)
import Control.Monad.IO.Class
import Data.Env
import Data.Error
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Maybe (catMaybes, isNothing)
import Data.Ord (comparing)
import Data.Value
import Eval
import Eval.Primitive
import Types

typeSystem :: Value -> IOThrowsError Value
typeSystem val =
  let env = typeBindings
      z = vzSetEnv env . vzFromValue $ val
   in do
        _ <- typeSystem' [] [id] z
        return val

typeSystem' :: [String] -> [ValueZipperTurn] -> ValueZipper -> IOThrowsError Value
typeSystem' stack steps z@(env, Call [Atom _ "if", pred, conseq, alt], _) = do
  predType <- typeSystem' stack [id] $ vzSet pred z
  conseqType <- typeSystem' stack [id] $ vzSet conseq z
  altType <- typeSystem' stack [id] $ vzSet alt z
  unless (checkType predType TBool) $ throwError $ TypeMismatch TBool (unwrapType predType)
  if unwrapType conseqType == unwrapType altType
    then typeSystem' stack steps $ vzSet conseqType z
    else typeSystem' stack steps $ vzSet (Type $ deduceReturnType (unwrapType conseqType) (unwrapType altType)) z
  where
    deduceReturnType (TVar var) (TVar _) = TVar var
    deduceReturnType (TVar _) t = t
    deduceReturnType t (TVar _) = t
    deduceReturnType t1 t2 = flattenSum $ TSum [t1, t2]
typeSystem' stack steps z@(_, Call (Func {params = params} : args), _) = do
  if num params /= num args || num params > num args
    then throwError $ NumArgs (num params) args
    else do
      (newZ, newSteps) <- stepEval z
      case newSteps ++ steps of
        [] -> typeSystem' stack [] newZ
        (step : nextSteps) -> typeSystem' stack nextSteps (step newZ)
  where
    num = toInteger . length
typeSystem' stack steps z@(env, Call (Atom _ func : _), crumbs) = do
  let newStack = if isFunc env func then func : stack else stack
  if func `elem` stack && isFunc env func
    then typeSystem' newStack steps $ vzSet (Type (TVar func)) z
    else do
      (newZ, newSteps) <- stepEval z
      case newSteps ++ steps of
        [] -> typeSystem' newStack [] newZ
        (step : nextSteps) -> typeSystem' newStack nextSteps (step newZ)
  where
    isFunc env func = case getVar env func of
      Right (Call (Atom _ "fn" : _)) -> True
      _ -> False
typeSystem' _ [] (_, val, _) = return $ Type $ typeOf val
typeSystem' stack steps z = do
  (newZ, newSteps) <- stepEval z
  case newSteps ++ steps of
    [] -> typeSystem' stack [] newZ
    (step : nextSteps) -> typeSystem' stack nextSteps (step newZ)

typeBindings :: Env
typeBindings = primitiveBindings ++ ioBindings
  where
    makeFunc constructor (var, func) = (var, constructor var func)
    transformType (name, _, typ) = (name, createType typ)
    primitiveBindings = makeFunc PrimitiveFunc <$> (transformType <$> primitives)
    ioBindings = makeFunc PrimitiveFunc <$> (transformType <$> ioPrimitives)

createType :: Type -> ([Value] -> ThrowsError Value)
createType (TFunc argTypes retType) args = do
  when (length argTypes /= length args) (throwError $ NumArgs (toInteger $ length argTypes) args)
  deducedArgTypes <- zipWithM deduceArgType argTypes (typeOf <$> args)
  deducedArgTypes2 <- mapM tryDeduceVariables $ groupVariables $ catMaybes $ fst <$> deducedArgTypes
  mapM_ (uncurry checkTypeCompatibility) (zip (snd <$> deducedArgTypes) (typeOf <$> args))
  Type <$> applyVarsToType (catMaybes $ fst <$> deducedArgTypes) retType
  where
    checkTypeCompatibility :: Type -> Type -> ThrowsError ()
    checkTypeCompatibility t1 t2 | typeCompatibleWith t1 t2 = return ()
    checkTypeCompatibility t1 t2 = throwError $ TypeMismatch t1 t2
    tryDeduceVariables :: (String, [Type]) -> ThrowsError (String, Type)
    tryDeduceVariables (name, types) = case deduceVariables types of
      Nothing -> throwError $ FailedToDeduceVar name types
      Just t -> return (name, t)
    deduceVariables :: [Type] -> Maybe Type
    deduceVariables [t] = Just t
    deduceVariables (t : types) = do
      deducedType <- deduceVariables types
      either (const Nothing) Just $ snd <$> deduceArgType deducedType t
createType _ _ = error "createType: unsupported"

applyVarsToType :: [(String, Type)] -> Type -> ThrowsError Type
applyVarsToType vars (TVar var) = case lookup var vars of
  Just t -> return t
  Nothing -> return (TVar var)
applyVarsToType vars (TList t) = TList <$> applyVarsToType vars t
applyVarsToType vars (TMap keyType valType) = do
  key <- applyVarsToType vars keyType
  val <- applyVarsToType vars valType
  return $ TMap key val
applyVarsToType vars (TFunc argTypes retType) = do
  args <- mapM (applyVarsToType vars) argTypes
  ret <- applyVarsToType vars retType
  return $ TFunc args ret
applyVarsToType _ t = return t

deduceArgType :: Type -> Type -> ThrowsError (Maybe (String, Type), Type)
deduceArgType (TVar v) t = return (Just (v, t), t)
deduceArgType t (TVar v) = return (Just (v, t), t)
deduceArgType (TList t1) (TList t2) = (TList <$>) <$> deduceArgType t1 t2
deduceArgType (TMap k1 v1) (TMap k2 v2) = do
  (var, key) <- deduceArgType k1 k2
  (var2, val) <- deduceArgType v1 v2
  return (var2 <|> var, TMap key val)
deduceArgType t1 t2 | typeCompatibleWith t1 t2 = return (Nothing, t1)
deduceArgType t1 t2 = throwError $ TypeMismatch t1 t2

typeCompatibleWith :: Type -> Type -> Bool
typeCompatibleWith (TSum sumTypes) (TSum sumTypes2) | isSumTypeDeducibleTo sumTypes2 sumTypes = True
typeCompatibleWith (TSum sumTypes) argType | any (typeCompatibleWith argType) sumTypes = True
typeCompatibleWith (TSum sumTypes) (TProd prodTypes) | all (`elem` sumTypes) prodTypes = True
typeCompatibleWith (TProd prodTypes1) (TProd prodTypes2) | all (uncurry typeCompatibleWith) (zip prodTypes1 prodTypes2) = True
typeCompatibleWith (TList listType) (TList listType2) | typeCompatibleWith listType listType2 = True
typeCompatibleWith (TList listType) (TProd types) | all (typeCompatibleWith listType) types = True
typeCompatibleWith (TMap k1 v1) (TMap k2 v2) | typeCompatibleWith k1 k2 && typeCompatibleWith v1 v2 = True
typeCompatibleWith (TProd types) (TList listType) | all (typeCompatibleWith listType) types = True
typeCompatibleWith (TVar _) (TVar _) = True
typeCompatibleWith (TVar _) t = True
typeCompatibleWith t (TVar _) = True
typeCompatibleWith t1 t2 | t1 == t2 = True
typeCompatibleWith _ _ = False

typeOf :: Value -> Type
typeOf (Integer _) = TInteger
typeOf (Character _) = TCharacter
typeOf (String _) = TString
typeOf (Bool _) = TBool
typeOf (Float _) = TFloat
typeOf Unit = TUnit
typeOf (Type t) = t
typeOf (List _ []) = TList $ TVar "l"
typeOf (List _ xs@(x : _)) | allEqual $ typeOf <$> xs = TList $ typeOf x
typeOf (List _ xs) = TProd $ typeOf <$> xs
typeOf (Map xs) =
  let keys = [key | (i, key) <- zip [0 ..] xs, even i]
      values = [val | (i, val) <- zip [0 ..] xs, odd i]
   in TMap (deduceSum (typeOf <$> keys)) (deduceSum (typeOf <$> values))
typeOf t = error $ "typeOf error " ++ show t

deduceSum :: [Type] -> Type
deduceSum types | allEqual types = head types
deduceSum types = flattenSum $ TSum $ deduplicate types

isSumTypeDeducibleTo :: [Type] -> [Type] -> Bool
isSumTypeDeducibleTo from = all (`elem` from)

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

unwrapType :: Value -> Type
unwrapType (Type t) = t
unwrapType t = error $ "Failed to unwrap type " ++ show t

checkType :: Value -> Type -> Bool
checkType (Type t) t2 | t == t2 = True
checkType _ _ = False

groupVariables :: [(String, Type)] -> [(String, [Type])]
groupVariables =
  map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst)
    . sortBy (comparing fst)

flattenSum :: Type -> Type
flattenSum (TSum elements) = TSum $ deduplicate $ concatMap go elements
  where
    go :: Type -> [Type]
    go (TSum innerElements) = concatMap go innerElements
    go v = [v]

isSum :: Type -> Bool
isSum (TSum _) = True
isSum _ = False

deduplicate :: Eq a => [a] -> [a]
deduplicate [] = []
deduplicate (x : xs) = x : filter (/= x) (deduplicate xs)
