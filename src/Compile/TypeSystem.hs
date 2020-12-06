module Compile.TypeSystem (typeSystem) where

import Control.Monad (unless, zipWithM)
import Data.Error
import Data.Maybe (catMaybes)
import Data.Value
import Eval
import Eval.Primitive
import Types

typeSystem :: LispVal -> IOThrowsError LispVal
typeSystem val =
  let env = typeBindings
      z = lvSetEnv env . lvFromAST $ val
   in do
        _ <- typeSystem' [id] z
        return val

typeSystem' :: [LVZipperTurn] -> LVZipper -> IOThrowsError LispVal
typeSystem' steps z@(env, List _ [Atom _ "if", pred, conseq, alt], _) = do
  predType <- typeSystem' [id] $ lvSetEnv env (lvFromAST pred)
  conseqType <- typeSystem' [id] $ lvSetEnv env (lvFromAST conseq)
  altType <- typeSystem' [id] $ lvSetEnv env (lvFromAST alt)
  unless (checkType predType TBool) $ throwError $ TypeMismatch TBool (unwrapType predType)
  if unwrapType conseqType == unwrapType altType
    then typeSystem' steps $ lvSet conseqType z
    else throwError $ TypeMismatch (unwrapType conseqType) (unwrapType altType)
typeSystem' [] (_, val, _) = return $ Type $ typeOf val
typeSystem' steps z = do
  (newZ, newSteps) <- stepEval z
  case newSteps ++ steps of
    [] -> typeSystem' [] newZ
    (step : nextSteps) -> typeSystem' nextSteps (step newZ)

typeBindings :: Env
typeBindings = map (makeFunc PrimitiveFunc) (transformType <$> primitives)
  where
    makeFunc constructor (var, func) = (var, constructor var func)
    transformType (name, _, typ) = (name, createType typ)

createType :: LispType -> ([LispVal] -> ThrowsError LispVal)
createType (TFunc argTypes varArg retType) args = do
  argTypesWithVarArgs <- case varArg of
    Nothing ->
      if length argTypes /= length args
        then throwError $ NumArgs (toInteger $ length argTypes) args
        else return argTypes
    Just varArgType -> return (argTypes ++ replicate (length args - length argTypes) varArgType)
  deducedArgTypes <- zipWithM deduceArgType argTypesWithVarArgs (typeOf <$> args)
  mapM_ (uncurry checkTypeCompatibility) (zip (snd <$> deducedArgTypes) (typeOf <$> args))
  Type <$> applyVarsToType (catMaybes $ fst <$> deducedArgTypes) retType
  where
    checkTypeCompatibility :: LispType -> LispType -> ThrowsError ()
    checkTypeCompatibility t1 t2 | typeCompatibleWith t1 t2 = return ()
    checkTypeCompatibility t1 t2 = throwError $ TypeMismatch t1 t2
createType _ _ = error "createType: unsupported"

applyVarsToType :: [(String, LispType)] -> LispType -> ThrowsError LispType
applyVarsToType vars (TVar var) = case lookup var vars of
  Just t -> return t
  Nothing -> throwError $ Default "Failed to find var"
applyVarsToType vars (TList t) = TList <$> applyVarsToType vars t
applyVarsToType vars (TFunc argTypes varArgType retType) = do
  args <- mapM (applyVarsToType vars) argTypes
  varArg <- mapM (applyVarsToType vars) varArgType
  ret <- applyVarsToType vars retType
  return $ TFunc args varArg ret
applyVarsToType _ t = return t

deduceArgType :: LispType -> LispType -> ThrowsError (Maybe (String, LispType), LispType)
deduceArgType (TVar v) t = return (Just (v, t), t)
deduceArgType (TList t1) (TList t2) = (TList <$>) <$> deduceArgType t1 t2
deduceArgType t1 _ = return (Nothing, t1)

typeCompatibleWith :: LispType -> LispType -> Bool
typeCompatibleWith (TSum sumTypes) (TSum sumTypes2) | isSumTypeDeducibleTo sumTypes2 sumTypes = True
typeCompatibleWith (TSum sumTypes) argType | argType `elem` sumTypes = True
typeCompatibleWith (TSum sumTypes) (TProd prodTypes) | all (`elem` sumTypes) prodTypes = True
typeCompatibleWith (TProd prodTypes1) (TProd prodTypes2) | all (uncurry typeCompatibleWith) (zip prodTypes1 prodTypes2) = True
typeCompatibleWith (TList listType) (TList listType2) | typeCompatibleWith listType listType2 = True
typeCompatibleWith t1 t2 | t1 == t2 = True
typeCompatibleWith _ _ = False

typeOf :: LispVal -> LispType
typeOf (Integer _) = TInteger
typeOf (Character _) = TCharacter
typeOf (String _) = TString
typeOf (Bool _) = TBool
typeOf (Float _) = TFloat
typeOf Unit = TUnit
typeOf (Type t) = t
typeOf (List _ xs@(x : _)) | allEqual $ typeOf <$> xs = TList $ typeOf x
typeOf (List _ xs) = TList $ TProd $ typeOf <$> xs
typeOf t = error $ "typeOf error " ++ show t

isSumTypeDeducibleTo :: [LispType] -> [LispType] -> Bool
isSumTypeDeducibleTo from = all (`elem` from)

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

unwrapType :: LispVal -> LispType
unwrapType (Type t) = t
unwrapType t = error $ "Failed to unwrap type " ++ show t

checkType :: LispVal -> LispType -> Bool
checkType (Type t) t2 | t == t2 = True
checkType _ _ = False
