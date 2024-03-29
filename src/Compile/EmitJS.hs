{-# LANGUAGE TemplateHaskell #-}

module Compile.EmitJS (emitJS, emitPrimitives) where

import Data.FileEmbed (embedStringFile)
import Data.List (intercalate)
import Eval.Primitive (ioPrimitives, primitives)
import Types (AST (..))

emitJS :: AST -> String
emitJS val = emitPrimitives ++ emitJS' val

emitJS' :: AST -> String
emitJS' (ASTFunc params body env) = "((" ++ intercalate "," (escapeName <$> params) ++ ") => " ++ emitJS' body ++ ")"
emitJS' (ASTLet binds expr) = "(function(){" ++ concat (emitBind <$> binds) ++ "return " ++ emitJS' expr ++ ";" ++ "})()"
  where
    emitBind (name, val) = "const " ++ escapeName name ++ " = " ++ emitJS' val ++ ";"
emitJS' (ASTIf pred conseq alt) = "(" ++ emitJS' pred ++ ") ? (" ++ emitJS' conseq ++ ") : (" ++ emitJS' alt ++ ")"
emitJS' (ASTCall (ASTAtom func) args) | isPrimitive func = primitiveName func ++ "(" ++ intercalate "," (emitJS' <$> args) ++ ")"
emitJS' (ASTCall func args) = emitJS' func ++ "(" ++ intercalate "," (emitJS' <$> args) ++ ")"
emitJS' (ASTList xs) = "[" ++ intercalate "," (emitJS' <$> xs) ++ "]"
emitJS' (ASTMap binds) = "{" ++ intercalate "," (emitBinds binds) ++ "}"
  where
    emitBinds ((key, value) : rest) = (emitJS' key ++ ":" ++ emitJS' value) : emitBinds rest
    emitBinds [] = []
emitJS' (ASTInteger n) = show n
emitJS' (ASTString n) = show n
emitJS' (ASTCharacter n) = show n
emitJS' (ASTFloat n) = show n
emitJS' (ASTBool n) = if n then "true" else "false"
emitJS' ASTUnit = "null"
emitJS' (ASTAtom name) | isPrimitive name = primitiveName name
emitJS' (ASTAtom name) = escapeName name
emitJS' (ASTType _) = ""
emitJS' (ASTPrimitiveFunc name) = primitiveName name

escapeName "new" = "$$new"
escapeName n = escape <$> n
  where
    escape '!' = 'A'
    escape '$' = 'B'
    escape '&' = 'C'
    escape '|' = 'D'
    escape '*' = 'E'
    escape '+' = 'F'
    escape '-' = 'G'
    escape '/' = 'H'
    escape ':' = 'I'
    escape '<' = 'M'
    escape '=' = 'N'
    escape '>' = 'O'
    escape '?' = 'P'
    escape '^' = 'Q'
    escape '%' = 'X'
    escape '.' = 'Y'
    escape c = c

emitPrimitives :: String
emitPrimitives = $(embedStringFile "src/Eval/Primitive.js")

isPrimitive :: String -> Bool
isPrimitive func = func `elem` (getName <$> primitives) ++ (getName <$> ioPrimitives)
  where
    getName (name, _, _) = name

primitiveName :: String -> String
primitiveName "+" = "$$sum"
primitiveName "-" = "$$sub"
primitiveName "*" = "$$prod"
primitiveName "/" = "$$div"
primitiveName "==" = "$$num_eq"
primitiveName "/=" = "$$num_not_eq"
primitiveName "<" = "$$lt"
primitiveName ">" = "$$gt"
primitiveName "<=" = "$$lte"
primitiveName ">=" = "$$gte"
primitiveName "&&" = "$$and"
primitiveName "||" = "$$or"
primitiveName "!" = "$$not"
primitiveName "eq?" = "$$eq"
primitiveName "list?" = "$$list"
primitiveName "integer?" = "$$integer"
primitiveName "float?" = "$$float"
primitiveName "string?" = "$$string"
primitiveName "character?" = "$$character"
primitiveName "bool?" = "$$bool"
primitiveName n = "$$" ++ replace '.' '$' n

replace a b = map $ \c -> if c == a then b else c
