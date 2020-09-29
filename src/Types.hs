module Types
  ( ThrowsError,
    IOThrowsError,
    LispError (..),
    LispVal (..),
    Parser,
    State (..),
    StateRef,
    LispValCrumb (..),
    LispValZipper,
    Env,
  )
where

import Control.Monad.Except
import Data.IORef
import Data.Void
import System.IO (Handle)
import Text.Megaparsec hiding (State)

newtype State = State {gensymCounter :: Integer}

type StateRef = IORef State

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO

type Parser = Parsec Void String

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
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
  | Func {params :: [String], vararg :: Maybe String, body :: LispVal, closure :: Env}
  | Macro {body :: LispVal, closure :: Env}

data LispValCrumb = LispValCrumb Env (Maybe SourcePos) [LispVal] [LispVal] deriving (Show, Eq)

type Env = [(String, LispVal)]

type LispValZipper = (Env, Maybe LispVal, [LispValCrumb])

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
  show Func {params = args, vararg = varargs, body = body, closure = env} =
    "{lambda [" ++ unwords (map show args)
      ++ ( case varargs of
             Nothing -> ""
             Just arg -> " . " ++ arg
         )
      ++ "] "
      ++ show body
      ++ "}"
  show (Port _) = "<IO port>"
  show (IOFunc name _) = "<IO primitive>"

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
    "Invalid type: expected " ++ expected
      ++ ", found "
      ++ show found
  show (Parsing parseErr) = errorBundlePretty parseErr
  show (FromCode obj) = "Error from code: " ++ show obj
  show (Default err) = err
