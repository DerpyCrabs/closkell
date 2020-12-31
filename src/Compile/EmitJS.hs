module Compile.EmitJS (emitJS, emitPrimitives) where

import Data.List (intercalate)
import Eval.Primitive (ioPrimitives, primitives)
import Types (LispVal (..))

emitJS :: LispVal -> String
emitJS val = emitPrimitives ++ emitJS' val

emitJS' :: LispVal -> String
emitJS' (Call [Atom _ "fn", List _ [List _ []], body]) = "() => " ++ emitJS' body
emitJS' (Call [Atom _ "fn", List _ params, body]) = "(" ++ intercalate "," (emitJS' <$> params) ++ ") => " ++ emitJS' body
emitJS' (Call [Atom _ "fn", DottedList _ [] (Atom _ "%&"), body]) = "(..." ++ "$$vararg" ++ ") => " ++ emitJS' body
emitJS' (Call [Atom _ "fn", DottedList _ params varargs, body]) = "(" ++ intercalate "," (emitJS' <$> params) ++ (if null params then "" else ",") ++ "..." ++ show varargs ++ ") => " ++ emitJS' body
emitJS' (Call [Atom _ "unquote", val]) = emitJS' val
emitJS' (Call [Atom _ "unquote-splicing", val]) = "...(" ++ emitJS' val ++ ")"
emitJS' (Call (Atom _ "let" : bindsAndExpr)) = "(function(){" ++ concat (emitBind <$> binds) ++ "return " ++ emitJS' expr ++ ";" ++ "})()"
  where
    binds = init bindsAndExpr
    expr = last bindsAndExpr
    emitBind (List _ [name@(Atom _ _), val]) = "const " ++ emitJS' name ++ " = " ++ emitJS' val ++ ";"
emitJS' (Call [Atom _ "if", pred, conseq, alt]) = "(" ++ emitJS' pred ++ ") ? (" ++ emitJS' conseq ++ ") : (" ++ emitJS' alt ++ ")"
emitJS' (Call [Atom _ "apply", f, args]) = "(" ++ emitJS' f ++ ").apply(" ++ emitJS' args ++ ")"
emitJS' (Call (Atom _ func : args)) | isPrimitive func = primitiveName func ++ "(" ++ intercalate "," (emitJS' <$> args) ++ ")"
emitJS' (Call (func@(Atom _ f) : args)) = emitJS' func ++ "(" ++ intercalate "," (emitJS' <$> args) ++ ")"
emitJS' (List _ xs) = "[" ++ intercalate "," (emitJS' <$> xs) ++ "]"
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
emitPrimitives =
  "\
  \ const $$sum = (a, b) => a + b; \
  \ const $$sub = (a, b) => a - b; \
  \ const $$prod = (a, b) => a * b; \
  \ const $$div = (a, b) => a / b ; \
  \ const $$rem = (a, b) => a % b ; \
  \ const $$num_eq = (a, b) => a === b; \
  \ const $$num_not_eq = (a, b) => a !== b; \
  \ const $$lt = (a, b) => a < b; \
  \ const $$gt = (a, b) => a > b; \
  \ const $$lte = (a, b) => a <= b; \
  \ const $$gte = (a, b) => a >= b; \
  \ const $$and = (a, b) => a && b; \
  \ const $$or = (a, b) => a || b; \
  \ const $$not = (a) => !a; \
  \ const $$string$concat = (a, b) => a.concat(b); \
  \ const $$string$from = (a) => JSON.stringify(a); \
  \ const $$string$toList = (a) => a.split(''); \
  \ const $$car = l => l[0]; \
  \ const $$cdr = l => l.slice(1); \
  \ const $$cons = (x, xs) => [x, ...xs]; \
  \ const $$get = (x, xs) => xs[xs.findIndex(el => el === x) + 1]; \
  \ const $$nth = (n, xs) => xs[n]; \
  \ const $$equal = (a, b) => (a >= b) && (a <= b); \
  \ const $$eq = (a, b) => $$equal(a, b); \
  \ const $$list = (a) => Array.isArray(a); \
  \ const $$integer = (a) => Number.isInteger(a); \
  \ const $$float = (a) => !$$integer && (typeof(a) === 'number'); \
  \ const $$string = (a) => (typeof(a) === 'string'); \
  \ const $$character = (a) => $$string(a) && a.length === 1; \
  \ const $$bool = (a) => (typeof(a) === 'boolean'); \
  \ const $$do = (...k) => k[k.length - 1]; \
  \ const $$io$read = () => { \
  \  const fs = require('fs'); \
  \  let rtnval = ''; \
  \  const buffer = Buffer.alloc ? Buffer.alloc(1) : new Buffer(1); \
  \  for (;;) { \
  \      fs.readSync(0, buffer, 0, 1); \
  \      if (buffer[0] === 10) { \
  \          break; \
  \      } else if (buffer[0] !== 13) { \
  \          rtnval += new String(buffer); \
  \      } \
  \  } \
  \  return rtnval; \
  \ }; \
  \ const $$io$write = s => {console.log(s); return null}; \
  \ const $$io$panic = (e) => {throw (JSON.stringify(e)); return null}; \
  \"

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
