module Compile.EmitLLVM (emitLLVM) where

import Data.List (intercalate)
import Types (Value (..))

emitLLVM :: Value -> String
emitLLVM = emitLLVM'

emitLLVM' :: Value -> String
emitLLVM' v =
  let mainDefinition = emitDefinition "i64" "main" [] v
   in intercalate "\n" mainDefinition

emitDefinition :: String -> String -> [String] -> Value -> [String]
emitDefinition retType name args asm =
  let header = sig retType name args <> " {"
      content = emitValue retType asm
      footer = "}"
   in [header] <> content <> [footer]

sig :: String -> String -> [String] -> String
sig retType name args =
  "define fastcc " <> retType <> " @" <> name <> showLocals args

showLocals :: [String] -> String
showLocals ds =
  "(" <> showItems showLocal ds <> ")"

showLocal :: String -> String
showLocal x =
  "i8* " <> x

showItems :: (a -> String) -> [a] -> String
showItems f itemList =
  case itemList of
    [] ->
      ""
    [a] ->
      f a
    a : as ->
      f a <> ", " <> showItems f as

emitValue :: String -> Value -> [String]
emitValue _ (Call [Atom _ func, Integer i1, Integer i2]) | func == "+" = ["%add = add i64 " <> show i1 <> ", " <> show i2, "ret i64 %add"]
emitValue _ _ = error "not implemented"