{-# LANGUAGE DeriveGeneric #-}

module Types
  ( ThrowsError,
    IOThrowsError,
    Error (..),
    Value (..),
    Parser,
    MacroExpansionState (..),
    ValueCrumb (..),
    ValueZipper,
    ValueZipperTurn,
    Env,
    Type (..),
    AST (..),
    FocusedValPath,
  )
where

import Control.Monad.Except
import Data.List (intercalate)
import Data.Void
import GHC.Generics
import System.IO (Handle)
import Text.Megaparsec hiding (State)

newtype MacroExpansionState = MacroExpansionState {gensymCounter :: Integer}

type ThrowsError = Either Error

type IOThrowsError = ExceptT Error IO

type Parser = Parsec Void String

data Error
  = NumArgs Integer [Value]
  | TypeMismatch Type Type
  | FailedToDeduceVar String [Type]
  | BadSpecialForm String Value
  | NotFunction String String
  | Parsing (ParseErrorBundle String Void)
  | UnboundVar String String
  | FromCode Value
  | Default String
  deriving (Eq)

data Value
  = Atom (Maybe SourcePos) String
  | Character Char
  | List (Maybe SourcePos) [Value]
  | Map [(Value, Value)]
  | DottedList (Maybe SourcePos) [Value] Value
  | Integer Integer
  | Float Double
  | String String
  | Bool Bool
  | Unit
  | Call [Value]
  | PrimitiveFunc String
  | Func {params :: [String], body :: Value, closure :: Env}
  | Macro {body :: Value, closure :: Env}
  | Type Type

data Type
  = TBottom
  | TUnit
  | TCharacter
  | TInteger
  | TFloat
  | TString
  | TBool
  | TList Type
  | TMap Type Type
  | TFunc [Type] Type
  | TSum [Type]
  | TProd [Type]
  | TVar String
  deriving (Eq, Generic)

type Env = [(String, Value)]

data ValueCrumb = ValueCrumb Env [Value] [Value] deriving (Show, Eq)

type ValueZipper = (Env, Value, [ValueCrumb])

type ValueZipperTurn = ValueZipper -> ValueZipper

type FocusedValPath = [Int]

data AST
  = ASTUnit
  | ASTCharacter Char
  | ASTInteger Integer
  | ASTFloat Double
  | ASTString String
  | ASTBool Bool
  | ASTList [AST]
  | ASTMap [(AST, AST)]
  | ASTAtom String
  | ASTCall AST [AST]
  | ASTPrimitiveFunc String
  | ASTFunc [String] AST ASTEnv
  | ASTIf AST AST AST
  | ASTLet [(String, AST)] AST
  | ASTType Type

type ASTEnv = [(String, AST)]

instance Eq Value where
  (Atom _ s1) == (Atom _ s2) = s1 == s2
  (List _ l1) == (List _ l2) = l1 == l2
  (Map l1) == (Map l2) = l1 == l2
  (Call l1) == (Call l2) = l1 == l2
  (DottedList _ l1 e1) == (DottedList _ l2 e2) = l1 == l2 && e1 == e2
  (Integer i1) == (Integer i2) = i1 == i2
  (String s1) == (String s2) = s1 == s2
  (Float i1) == (Float i2) = i1 == i2
  (Bool i1) == (Bool i2) = i1 == i2
  (Character c1) == (Character c2) = c1 == c2
  (PrimitiveFunc n1) == (PrimitiveFunc n2) = n1 == n2
  (Macro b1 c1) == (Macro b2 c2) = b1 == b2 && c1 == c2
  (Type t1) == (Type t2) = t1 == t2
  Unit == Unit = True
  (Func p1 b1 c1) == (Func p2 b2 c2) = p1 == p2 && b1 == b2 && c1 == c2
  _ == _ = False

instance Show Value where
  show (String contents) = "\"" ++ contents ++ "\""
  show (Character contents) = "'" ++ [contents] ++ "'"
  show (Atom _ name) = name
  show (Integer contents) = show contents
  show (Float contents) = show contents
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (List _ contents) = "[" ++ unwordsList contents ++ "]"
  show (Map contents) = "{" ++ unwordsList (concat $ (\(key, val) -> [key, val]) <$> contents) ++ "}"
  show (Call contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList _ head tail) = "[" ++ unwordsList head ++ " . " ++ show tail ++ "]"
  show (PrimitiveFunc name) = "<primitive " ++ name ++ ">"
  show Func {params = args, body = body} =
    "{fn [" ++ unwords (map show args)
      ++ "] "
      ++ show body
      ++ "}"
  show (Macro body _) = "<Macro " ++ show body ++ ">"
  show (Type t) = show t
  show Unit = "unit"

unwordsList :: [Value] -> String
unwordsList = unwords . map show

instance Show Error where
  show (UnboundVar message var) = message ++ ": " ++ var
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) =
    "Expected " ++ show expected
      ++ " args; found values "
      ++ unwords (map show found)
  show (TypeMismatch expected found) =
    "Invalid type: expected " ++ show expected
      ++ " but found "
      ++ show found
  show (Parsing parseErr) = errorBundlePretty parseErr
  show (FromCode obj) = "Error from code: " ++ show obj
  show (Default err) = err
  show (FailedToDeduceVar name varTypes) = "Failed to deduce variable '" ++ name ++ "' from types: " ++ intercalate ", " (show <$> varTypes)

instance Show Type where
  show TCharacter = "TCharacter"
  show (TList t) = "TList[" ++ show t ++ "]"
  show (TMap key val) = "TMap[" ++ show key ++ "," ++ show val ++ "]"
  show TInteger = "TInteger"
  show TFloat = "TFloat"
  show TString = "TString"
  show TBool = "TBool"
  show (TFunc args ret) =
    let listArgs = intercalate " -> " (show <$> args)
     in "TFunc[" ++ listArgs ++ " -> " ++ show ret ++ "]"
  show (TSum variants) = intercalate " | " (show <$> variants)
  show (TProd elements) = intercalate " & " (show <$> elements)
  show TUnit = "TUnit"
  show (TVar var) = var
  show TBottom = "TBottom"
