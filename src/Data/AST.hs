{-# LANGUAGE TupleSections #-}

module Data.AST where

import Data.Value
import Types

fromValue :: Value -> ThrowsError AST
fromValue (List _ args) = ASTList False <$> mapM fromValue args
fromValue (Map args) = ASTMap False <$> mapM fromValue args
fromValue (Call [Atom _ "fn", List _ params, body]) = do
  astBody <- fromValue body
  let paramNames = (\(String s) -> s) <$> params
  return $ ASTFunc paramNames Nothing astBody []
fromValue (Call [Atom _ "fn", DottedList _ params varargs, body]) = do
  astBody <- fromValue body
  let paramNames = (\(String s) -> s) <$> params
  let (String varargName) = varargs
  return $ ASTFunc paramNames (Just varargName) astBody []
fromValue (Atom _ name) = return $ ASTAtom name
fromValue (Call [Atom _ "if", pred, conseq, alt]) = do
  astPred <- fromValue pred
  astConseq <- fromValue conseq
  astAlt <- fromValue alt
  return $ ASTIf astPred astConseq astAlt
fromValue (Call [Atom _ "apply", f, args]) = do
  astF <- fromValue f
  astArgs <- fromValue args
  return $ ASTApply False astF astArgs
fromValue (Call [Atom _ "unquote-splicing", val]) = do
  astVal <- fromValue val
  return $ ASTUnquoteSplicing False astVal
fromValue (Call (Atom _ "let" : bindsAndExpr)) = do
  let binds = vars $ init bindsAndExpr
  let expr = last bindsAndExpr
  astExpr <- fromValue expr
  astVars <- mapM (\(name, var) -> (name,) <$> fromValue var) binds
  return $ ASTLet False astVars astExpr
  where
    matchVars (List _ [Atom _ name, var]) = (name, var)
    vars binds = matchVars <$> binds
fromValue (Call (function : args)) = do
  astFunction <- fromValue function
  astArgs <- mapM fromValue args
  return $ ASTCall False astFunction astArgs
fromValue Unit = return ASTUnit
fromValue (String s) = return $ ASTString s
fromValue (Character s) = return $ ASTCharacter s
fromValue (Integer s) = return $ ASTInteger s
fromValue (Float s) = return $ ASTFloat s
fromValue (Bool s) = return $ ASTBool s
fromValue (PrimitiveFunc name f) = return $ ASTPrimitiveFunc name f
fromValue (IOFunc name f) = return $ ASTIOFunc name f
fromValue (Type s) = return $ ASTType s
fromValue badForm = Left $ BadSpecialForm "Unrecognized special form" badForm
