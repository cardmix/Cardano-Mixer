{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Requests where

import           Control.Monad.IO.Class                       (MonadIO (..))
import           Data.Aeson
import           Data.Proxy                                   (Proxy (..))
import           Data.Text                                    (Text, pack)
import           Data.UUID                                    (UUID)
import           Network.HTTP.Req
import           Plutus.PAB.Events.ContractInstanceState      (PartiallyDecodedResponse(..))
import           Plutus.PAB.Webserver.Types                   (ContractInstanceClientState(..), ContractActivationArgs (..))
import           Wallet.Emulator.Wallet                       (Wallet(..))
import           Wallet.Types                                 (ContractInstanceId (..))

import           PAB

------------------------------- API Requests -------------------------------------

-- Activate a contract for a given wallet
activateRequest :: MixerContracts -> Maybe Wallet -> IO UUID
activateRequest x w = runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api" /: "contract" /: "activate")
        (ReqBodyJson $ ContractActivationArgs x w)
        (Proxy :: Proxy (JsonResponse ContractInstanceId))
        (port 9080)
    return $ unContractInstanceId $ responseBody v

-- Call an endpoint
endpointRequest :: (ToJSON p, Show p) => Text -> UUID -> p -> IO ()
endpointRequest endp uuid x = runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: endp)
        (ReqBodyJson x)
        (Proxy :: Proxy (JsonResponse ()))
        (port 9080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "Request processed: " ++ show x
        else "Error processing request!"

statusRequest :: (FromJSON p) => UUID -> IO (Maybe p)
statusRequest uuid = runReq defaultHttpConfig $ do
    v <- req
        GET
        (http "127.0.0.1" /: "api" /: "contract" /: "instance" /: pack (show uuid) /: "status")
        NoReqBody
        (Proxy :: Proxy (JsonResponse (ContractInstanceClientState MixerContracts)))
        (port 9080)
    let x = fromJSON $ observableState $ cicCurrentState $ responseBody v
    case x of
        Success tt -> return (Just tt)
        Error _    -> return Nothing

stopRequest :: UUID -> IO ()
stopRequest uuid = runReq defaultHttpConfig $ do
    v <- req
        PUT
        (http "127.0.0.1" /: "api" /: "contract" /: "instance" /: pack (show uuid) /: "stop")
        NoReqBody
        (Proxy :: Proxy (JsonResponse ()))
        (port 9080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "Request processed!"
        else "Error processing request!"
