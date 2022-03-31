{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}


module Main
    (
        main
    ) where

import           Data.ByteString                              (ByteString, writeFile, readFile)
import           Data.Text                                    (Text)
import           Ledger                                       (PaymentPubKeyHash, Value)
import           Plutus.V1.Ledger.Ada                         (lovelaceValueOf)
import           PlutusTx.Prelude                             hiding ((<$>))
import           Prelude                                      (IO, String, print)
import           System.Environment                           (getArgs)
import           Wallet.Emulator.Wallet                       (Wallet)

import           ClientLog                                    (getSecrets, logSecrets)
import           Crypto                                       (Fr, mimcHash)
import           MixerFrontendContracts                       (MixerFrontendContracts(..))
import           MixerProofs                                  (generateSimulatedWithdrawProof)
import           MixerState                                   (MixerState)
import           MixerUserData
import           PABContracts                                 (PABContracts(..))
import           Requests
import           Types.MixerContractTypes                     (DepositParams(..), WithdrawParams (..))


pabIP :: Text
pabIP = "127.0.0.1"

mixVal :: Value
mixVal = lovelaceValueOf 20_000

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["deposit",  str] -> depositProcedure str
        ["submit"]        -> depositSubmitProcedure
        ["withdraw", str] -> withdrawProcedure str
        _                 -> print ("Unknown command" :: String)

--------------------------------- Use mixer logic --------------------------------

depositProcedure :: String -> IO ()
depositProcedure str = do
    (pkh, _, w) <- mixerConnectProcedure str

    ds   <- generateDepositSecret
    sas  <- generateShieldedAccountSecret
    logSecrets ds sas
    let leaf = mimcHash (getR1 ds) (getR2 ds)
    print $ mimcHash zero (getR1 ds)
    cidMixerUse <- activateRequest pabIP (FrontendContracts MixerUse) (Just w)
    endpointRequest pabIP "deposit" cidMixerUse (DepositParams pkh (1, 1) leaf)
    bs <- awaitStatusUpdate pabIP cidMixerUse :: IO ByteString
    writeFile "tx.raw" bs

depositSubmitProcedure :: IO ()
depositSubmitProcedure = do
    (_, _, w) <- mixerConnectProcedure ""

    bs <- readFile "tx.signed"
    cidMixerUse <- activateRequest pabIP (FrontendContracts MixerUse) (Just w)
    endpointRequest pabIP "depositSubmit" cidMixerUse bs

withdrawProcedure :: String -> IO ()
withdrawProcedure str = do
    (pkh, a, w) <- mixerConnectProcedure str

    secret <- getSecrets
    case secret of
      Nothing        -> print @String "Nothing to withdraw."
      Just (ds, sas) -> do
        state <- mixerStateProcedure
        (lastDeposit, subs, proof) <- generateSimulatedWithdrawProof a ds sas state
        let params = WithdrawParams mixVal lastDeposit pkh subs proof
        cidMixerUse <- activateRequest pabIP (FrontendContracts MixerUse) (Just w)
        endpointRequest pabIP "withdraw" cidMixerUse params

------------------------------- Query mixer logic --------------------------------

mixerConnectProcedure :: String -> IO (PaymentPubKeyHash, Fr, Wallet)
mixerConnectProcedure str = do
    cidPAB <- activateRequest pabIP (FrontendContracts ConnectToPAB) (Nothing :: Maybe Wallet)
    endpointRequest pabIP "Connect to PAB" cidPAB str
    awaitStatusUpdate pabIP cidPAB

mixerStateProcedure :: IO MixerState
mixerStateProcedure = do
    cidQueryMixer <- activateRequest pabIP (FrontendContracts MixerStateQuery) (Nothing :: Maybe Wallet)
    endpointRequest pabIP "Get Mixer state" cidQueryMixer mixVal
    awaitStatusUpdate pabIP cidQueryMixer