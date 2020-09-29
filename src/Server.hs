{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server (server) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Eval (evalSteps)
import Eval.Primitive
import GHC.TypeLits
import JSONInstances
import Network.Wai.Handler.Warp
import Parse (readExpr)
import Servant
import Servant.API
import Types

type EvalAPI = "eval" :> ReqBody '[JSON] String :> Get '[JSON] [ThrowsError LispValZipper]

evalAPI :: Proxy EvalAPI
evalAPI = Proxy

evalServer :: Server EvalAPI
evalServer = eval
  where
    eval source = do
      let expr = readExpr "/eval" source
      case expr of
        Right val -> do
          let env = primitiveBindings
          steps <- liftIO $ evalSteps env val
          return $ filterEnv <$> steps
        Left err -> return [Left err]
    filterEnv :: ThrowsError LispValZipper -> ThrowsError LispValZipper
    filterEnv =
      fmap
        ( \(env, val, crumbs) ->
            ( filter notIntrinsic env,
              val,
              map (\(LispValCrumb env _ ls rs) -> LispValCrumb (filter notIntrinsic env) Nothing ls rs) crumbs
            )
        )
    notIntrinsic (_, IOFunc _ _) = False
    notIntrinsic (_, PrimitiveFunc _ _) = False
    notIntrinsic (_, _) = True

server :: [String] -> Application
server args = serve evalAPI evalServer
