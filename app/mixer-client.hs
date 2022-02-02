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

import           Data.Map                                     (singleton)
import           Data.Text                                    (Text)
import           PlutusTx.Prelude                             hiding ((<$>))
import           Prelude                                      (IO, String, print)
import           System.Environment                           (getArgs)

import           ClientLog
import           Crypto                                       (mimcHash, Fr)
import           MixerContractsDefinition 
import           MixerProofs                                  (generateSimulatedWithdrawProof)
import           MixerState                                   (MixerState)
import           MixerUserData
import           Requests


pabIP :: Text
pabIP = "127.0.0.1"

mixVal :: Value
mixVal = Value $ singleton (CurrencySymbol "") (singleton (TokenName "") 400_000)


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["deposit"]       -> depositProcedure
        ["withdraw", str] -> withdrawProcedure str
        _                 -> print ("Unknown command" :: String)

--------------------------------- Use mixer logic --------------------------------

depositProcedure :: IO ()
depositProcedure = do
    ds   <- generateDepositSecret
    sas  <- generateShieldedAccountSecret
    logSecrets ds sas
    let leaf = mimcHash (getR1 ds) (getR2 ds)
    print $ mimcHash zero (getR1 ds)
    cidUseMixer <- activateRequest pabIP UseMixer (Nothing :: Maybe Wallet)
    endpointRequest pabIP "deposit" cidUseMixer (DepositParams mixVal leaf)
    s <- awaitStatusUpdate pabIP cidUseMixer :: IO String
    print s

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
        cidUseMixer <- activateRequest pabIP UseMixer (Just w)
        endpointRequest pabIP "withdraw" cidUseMixer params

------------------------------- Query mixer logic --------------------------------

mixerConnectProcedure :: String -> IO (PaymentPubKeyHash, Fr, Wallet)
mixerConnectProcedure str = do
    cidPAB <- activateRequest pabIP ConnectToPAB (Nothing :: Maybe Wallet)
    endpointRequest pabIP "Connect to PAB" cidPAB str
    awaitStatusUpdate pabIP cidPAB

mixerStateProcedure :: IO MixerState
mixerStateProcedure = do
    cidQueryMixer <- activateRequest pabIP MixerStateQuery (Nothing :: Maybe Wallet)
    endpointRequest pabIP "Get Mixer state" cidQueryMixer mixVal
    awaitStatusUpdate pabIP cidQueryMixer




    