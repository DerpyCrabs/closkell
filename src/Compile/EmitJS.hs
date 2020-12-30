module Compile.EmitJS (emitJS, emitPrimitives) where

import Data.List (intercalate)
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
    escape '?' = 'Q'
    escape '%' = 'P'
    escape '.' = 'D'
    escape c = c

emitPrimitives :: String
emitPrimitives =
  "\
  \ const $$sum = (...k) => k.reduce((a, b) => a + b, 0); \
  \ const $$sub = (...k) => k.slice(1).reduce((a, b) => a - b, k[0]); \
  \ const $$prod = (...k) => k.reduce((a, b) => a * b, 1); \
  \ const $$div = (...k) => k.slice(1).reduce((a, b) => a / b, k[0]); \
  \ const $$car = l => l[0]; \
  \ const $$cdr = l => l.slice(1); \
  \ const $$io$write = s => {console.log(s); return null}; \
  \ const $$num_eq = (...k) => k.reduce((a, b) => a && k[0] == b, true); \
  \ const $$cons = (x, xs) => [x, ...xs]; \
  \ const $$get = (x, xs) => xs[xs.findIndex(el => el === x) + 1]; \
  \ const $$nth = (n, xs) => xs[n]; \
  \ const $$equal = (a, b) => (a >= b) && (a <= b); \
  \ const $$eq = (...k) => k.reduce((a, b) => a && $$equal(k[0], b), true); \
  \ const $$do = (...k) => k[k.length - 1]; \
  \ const $$lt = (a, b) => a < b; \
  \ const $$gt = (a, b) => a > b; \
  \ const $$and = (...k) => k.reduce((a, b) => a && b, true); \
  \ const $$or = (...k) => k.reduce((a, b) => a || b, false); \
  \ const $$string$from = (...k) => k.length === 1 ? JSON.stringify(k[0]) : JSON.stringify(k); \
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
  \ const $$io$panic = (e) => {throw (JSON.stringify(e)); return null}; \
  \"

isPrimitive :: String -> Bool
isPrimitive func = func `elem` ["+", "-", "*", "/", "car", "cdr", "cons", "==", "get", "nth", "eq?", "do", "io.read", "io.write", "string.from", "io.panic", "<", ">", "&&", "||"]

primitiveName :: String -> String
primitiveName "+" = "$$sum"
primitiveName "-" = "$$sub"
primitiveName "*" = "$$prod"
primitiveName "/" = "$$div"
primitiveName "<" = "$$lt"
primitiveName ">" = "$$gt"
primitiveName "==" = "$$num_eq"
primitiveName "&&" = "$$and"
primitiveName "||" = "$$or"
primitiveName "eq?" = "$$eq"
primitiveName n = "$$" ++ replace '.' '$' n

replace a b = map $ \c -> if c == a then b else c
