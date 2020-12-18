{-# LANGUAGE DeriveGeneric #-}

module Types
  ( ThrowsError,
    IOThrowsError,
    LispError (..),
    LispVal (..),
    Parser,
    MacroExpansionState (..),
    LVCrumb (..),
    LVZipper,
    LVZipperTurn,
    Env,
    LispType (..),
  )
where

import Control.Monad.Except
import Data.List (intercalate)
import Data.Void
import GHC.Generics
import System.IO (Handle)
import Text.Megaparsec hiding (State)

newtype MacroExpansionState = MacroExpansionState {gensymCounter :: Integer}

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO

type Parser = Parsec Void String

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch LispType LispType
  | FailedToDeduceVar String [LispType]
  | BadSpecialForm String LispVal
  | NotFunction String String
  | Parsing (ParseErrorBundle String Void)
  | UnboundVar String String
  | FromCode LispVal
  | Default String
  deriving (Eq)

data LispVal
  = Atom (Maybe SourcePos) String
  | Character Char
  | List (Maybe SourcePos) [LispVal]
  | DottedList (Maybe SourcePos) [LispVal] LispVal
  | Integer Integer
  | Float Double
  | String String
  | Bool Bool
  | PrimitiveFunc String ([LispVal] -> ThrowsError LispVal)
  | IOFunc String ([LispVal] -> IOThrowsError LispVal)
  | Port Handle
  | Unit
  | Func {params :: [String], vararg :: Maybe String, body :: LispVal, closure :: Env}
  | Macro {body :: LispVal, closure :: Env}
  | Type LispType

data LispType
  = TCharacter
  | TList LispType
  | TInteger
  | TFloat
  | TString
  | TBool
  | TFunc [LispType] (Maybe LispType) LispType
  | TSum [LispType]
  | TProd [LispType]
  | TUnit
  | TVar String
  deriving (Eq, Generic)

data LVCrumb = LVCrumb Env (Maybe SourcePos) [LispVal] [LispVal] deriving (Show, Eq)

type Env = [(String, LispVal)]

type LVZipper = (Env, LispVal, [LVCrumb])

type LVZipperTurn = LVZipper -> LVZipper

instance Eq LispVal where
  (Atom _ s1) == (Atom _ s2) = s1 == s2
  (List _ l1) == (List _ l2) = l1 == l2
  (DottedList _ l1 e1) == (DottedList _ l2 e2) = l1 == l2 && e1 == e2
  (Integer i1) == (Integer i2) = i1 == i2
  (String s1) == (String s2) = s1 == s2
  (Float i1) == (Float i2) = i1 == i2
  (Bool i1) == (Bool i2) = i1 == i2
  (Port h1) == (Port h2) = h1 == h2
  (Character c1) == (Character c2) = c1 == c2
  (PrimitiveFunc n1 _) == (PrimitiveFunc n2 _) = n1 == n2
  (IOFunc n1 _) == (IOFunc n2 _) = n1 == n2
  (Macro b1 c1) == (Macro b2 c2) = b1 == b2 && c1 == c2
  Unit == Unit = True
  (Func p1 v1 b1 c1) == (Func p2 v2 b2 c2) = p1 == p2 && v1 == v2 && b1 == b2 && c1 == c2
  _ == _ = False

instance Show LispVal where
  show (String contents) = "\"" ++ contents ++ "\""
  show (Character contents) = "'" ++ [contents] ++ "'"
  show (Atom _ name) = name
  show (Integer contents) = show contents
  show (Float contents) = show contents
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (List _ contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList _ head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
  show (PrimitiveFunc name _) = "<primitive " ++ name ++ ">"
  show Func {params = args, vararg = varargs, body = body} =
    "{fn [" ++ unwords (map show args)
      ++ ( case varargs of
             Nothing -> ""
             Just arg -> " . " ++ arg
         )
      ++ "] "
      ++ show body
      ++ "}"
  show (Port _) = "<IO port>"
  show (IOFunc name _) = "<IO primitive " ++ name ++ ">"
  show (Macro body _) = "<Macro " ++ show body ++ ">"
  show (Type t) = show t
  show Unit = "unit"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

instance Show LispError where
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

instance Show LispType where
  show TCharacter = "TCharacter"
  show (TList t) = "TList[" ++ show t ++ "]"
  show TInteger = "TInteger"
  show TFloat = "TFloat"
  show TString = "TString"
  show TBool = "TBool"
  show (TFunc args varArg ret) =
    let listArgs = intercalate " -> " (show <$> args)
     in "TFunc[" ++ listArgs ++ showVarArg varArg ++ " -> " ++ show ret ++ "]"
    where
      showVarArg (Just t) = " ~> " ++ show t
      showVarArg Nothing = ""
  show (TSum variants) = intercalate " | " (show <$> variants)
  show (TProd elements) = intercalate " & " (show <$> elements)
  show TUnit = "TUnit"
  show (TVar var) = var
