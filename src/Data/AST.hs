{-# LANGUAGE TupleSections #-}

module Data.AST where

import Control.Monad (forM)
import Data.Value
import Types

fromValue :: Value -> ThrowsError AST
fromValue (List _ args) = ASTList <$> mapM fromValue args
fromValue (Map args) = do
  keyValues <- forM args $ \(key, value) ->
    (,) <$> fromValue key <*> fromValue value
  return $ ASTMap keyValues
fromValue (Call [Atom _ "fn", List _ params, body]) = do
  astBody <- fromValue body
  let paramNames = (\(Atom _ s) -> s) <$> params
  return $ ASTFunc paramNames astBody []
fromValue (Atom _ name) = return $ ASTAtom name
fromValue (Call [Atom _ "if", pred, conseq, alt]) = do
  astPred <- fromValue pred
  astConseq <- fromValue conseq
  astAlt <- fromValue alt
  return $ ASTIf astPred astConseq astAlt
fromValue (Call (Atom _ "let" : bindsAndExpr)) = do
  let binds = vars $ init bindsAndExpr
  let expr = last bindsAndExpr
  astExpr <- fromValue expr
  astVars <- mapM (\(name, var) -> (name,) <$> fromValue var) binds
  return $ ASTLet astVars astExpr
  where
    matchVars (List _ [Atom _ name, var]) = (name, var)
    vars binds = matchVars <$> binds
fromValue (Call (function : args)) = do
  astFunction <- fromValue function
  astArgs <- mapM fromValue args
  return $ ASTCall astFunction astArgs
fromValue Unit = return ASTUnit
fromValue (String s) = return $ ASTString s
fromValue (Character s) = return $ ASTCharacter s
fromValue (Integer s) = return $ ASTInteger s
fromValue (Float s) = return $ ASTFloat s
fromValue (Bool s) = return $ ASTBool s
fromValue (PrimitiveFunc name) = return $ ASTPrimitiveFunc name
fromValue (Type s) = return $ ASTType s
fromValue badForm = Left $ BadSpecialForm "Unrecognized special form" badForm