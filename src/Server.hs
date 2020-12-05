{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server (server) where

import Compile.MacroSystem (macroSystem)
import Compile.TypeSystem (typeSystem)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Eval (evalSteps)
import Eval.Primitive
import GHC.Generics
import JSONInstances ()
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Parse (readExpr)
import Servant
import Types

data EvalBody = EvalBody {typeCheck :: Bool, macroExpand :: Bool, expression :: String} deriving (Generic)

instance FromJSON EvalBody

type EvalAPI = "eval" :> ReqBody '[JSON] EvalBody :> Post '[JSON] [ThrowsError LVZipper]

evalAPI :: Proxy EvalAPI
evalAPI = Proxy

evalServer :: Server EvalAPI
evalServer = eval
  where
    eval EvalBody {typeCheck, macroExpand, expression} = do
      let expr = readExpr "/eval" expression
      case expr of
        Right val -> do
          let env = primitiveBindings
          macroExpandedExpr <-
            if macroExpand
              then liftIO $ runExceptT $ macroSystem val
              else return $ Right val
          case macroExpandedExpr of
            Left err -> return [Left err]
            Right expr -> do
              typeCheckedExpr <-
                if typeCheck
                  then liftIO $ runExceptT $ typeSystem expr
                  else return macroExpandedExpr
              case typeCheckedExpr of
                Left err -> return [Left err]
                Right expr -> do
                  steps <- liftIO $ evalSteps env expr
                  return $ filterEnv <$> steps
        Left err -> return [Left err]
    filterEnv :: ThrowsError LVZipper -> ThrowsError LVZipper
    filterEnv =
      fmap
        ( \(env, val, crumbs) ->
            ( filter notIntrinsic env,
              val,
              map (\(LVCrumb env _ ls rs) -> LVCrumb (filter notIntrinsic env) Nothing ls rs) crumbs
            )
        )
    notIntrinsic (_, IOFunc _ _) = False
    notIntrinsic (_, PrimitiveFunc _ _) = False
    notIntrinsic (_, _) = True

server :: [String] -> Application
server args = cors (const $ Just policy) $ provideOptions evalAPI $ serve evalAPI evalServer
  where
    policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}
