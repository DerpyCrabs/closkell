module Data.Error (liftThrows, throwError, trapError, extractValue) where

import Control.Monad.Except
import Data.Value
import Text.Megaparsec (sourcePosPretty)
import Text.Megaparsec.Error (errorBundlePretty)
import Types

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

showError :: LispError -> String
showError (UnboundVar message var) = message ++ ": " ++ var
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected
    ++ " args; found values "
    ++ unwords (map show found)
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected
    ++ ", found "
    ++ show found
showError (Parsing parseErr) = errorBundlePretty parseErr
showError (Default err) = err

instance Show LispError where show = showError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
