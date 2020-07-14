module Lib
  ( readExpr,
    eval,
    extractValue,
    trapError,
    IOThrowsError,
    Env,
    liftThrows,
    primitiveBindings,
    LispVal (..),
    bindVars,
  )
where

import Control.Monad.Except
import Data.Env
import Data.Error
import Data.Value
import Eval
import Parse
import Types
