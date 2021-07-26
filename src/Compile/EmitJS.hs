{-# LANGUAGE TemplateHaskell #-}

module Compile.EmitJS (emitJS, emitPrimitives) where

import Data.FileEmbed (embedStringFile)
import Data.List (intercalate)
import Eval.Primitive (ioPrimitives, primitives)
import Types (Value (..))

emitJS :: Value -> String
emitJS val = emitPrimitives ++ emitJS' val

emitJS' :: Value -> String
emitJS' (Call [Atom _ "fn", List _ params, body]) = "((" ++ intercalate "," (emitJS' <$> params) ++ ") => " ++ emitJS' body ++ ")"
emitJS' (Call (Atom _ "let" : bindsAndExpr)) = "(function(){" ++ concat (emitBind <$> binds) ++ "return " ++ emitJS' expr ++ ";" ++ "})()"
  where
    binds = init bindsAndExpr
    expr = last bindsAndExpr
    emitBind (List _ [name@(Atom _ _), val]) = "const " ++ emitJS' name ++ " = " ++ emitJS' val ++ ";"
emitJS' (Call [Atom _ "if", pred, conseq, alt]) = "(" ++ emitJS' pred ++ ") ? (" ++ emitJS' conseq ++ ") : (" ++ emitJS' alt ++ ")"
emitJS' (Call (Atom _ func : args)) | isPrimitive func = primitiveName func ++ "(" ++ intercalate "," (emitJS' <$> args) ++ ")"
emitJS' (Call (func : args)) = emitJS' func ++ "(" ++ intercalate "," (emitJS' <$> args) ++ ")"
emitJS' (List _ xs) = "[" ++ intercalate "," (emitJS' <$> xs) ++ "]"
emitJS' (Map binds) = "{" ++ intercalate "," (emitBinds binds) ++ "}"
  where
    emitBinds (key : value : rest) = (emitJS' key ++ ":" ++ emitJS' value) : emitBinds rest
    emitBinds [] = []
emitJS' (Integer n) = show n
emitJS' (String n) = show n
emitJS' (Character n) = show n
emitJS' (Float n) = show n
emitJS' (Bool n) = if n then "true" else "false"
emitJS' Unit = "null"
emitJS' (Atom _ name) | isPrimitive name = primitiveName name
emitJS' (Atom _ name) = escapeName name

escapeName "new" = "$$new"
escapeName "%&" = "$$vararg"
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
