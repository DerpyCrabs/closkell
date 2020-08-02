module Types
  ( ThrowsError,
    IOThrowsError,
    LispError (..),
    LispVal (..),
    Env (..),
    EnvRef,
    Parser,
    State (..),
    StateRef,
  )
where

import Control.Monad.Except
import Data.IORef
import Data.Void
import System.IO (Handle)
import Text.Megaparsec hiding (State)

data Env = Env {functions :: [(String, IORef LispVal)], macros :: [(String, IORef LispVal)]}

type EnvRef = IORef Env

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
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle
  | Func {params :: [String], vararg :: Maybe String, body :: [LispVal], closure :: EnvRef}

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
