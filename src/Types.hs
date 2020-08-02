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
