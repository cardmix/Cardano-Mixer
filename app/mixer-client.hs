{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main
    (
        main
    ) where

import           Control.Concurrent                           (threadDelay)
import           Ledger.Ada                                   (lovelaceValueOf)
import           PlutusTx.Prelude                             hiding ((<$>))
import           Prelude                                      (IO, String, print)
import           System.Environment                           (getArgs)
import           Wallet.Emulator.Types                        (mockWalletPaymentPubKeyHash)
import           Wallet.Emulator.Wallet                       (Wallet (..))

import           AdminKey                                     (adminKeyTokenName)
import           ClientLog
import           Configuration.PABConfig                      (pabWallet, pabWalletPKH)
import           Contracts.Currency                           (SimpleMPS(..))
import           Crypto                                       (mimcHash)
import           MixerContract
import           MixerProofs                                  (generateSimulatedWithdrawProof)
import           MixerState                                   (MixerState)
import           MixerUserData
import           PAB
import           Requests



main :: IO ()
main = do
    print pabWallet
    args <- getArgs
    case args of
        ["admin"]    -> mintAdminKeyProcedure pabWallet   -- for testing purposes
        ["deposit"]  -> depositProcedure
        ["withdraw"] -> withdrawProcedure
        ["retrieve"] -> retrieveTimeLockedProcedure
        _            -> print ("Unknown command" :: String)

--------------------------------- Use mixer logic --------------------------------

depositProcedure :: IO ()
depositProcedure = do
    ds   <- generateDepositSecret
    sas  <- generateShieldedAccountSecret
    logSecrets ds sas
    let leaf = mimcHash (getR1 ds) (getR2 ds)
    cidUseMixer <- activateRequest UseMixer (Just pabWallet)
    endpointRequest "deposit" cidUseMixer (DepositParams (lovelaceValueOf 200_000_000) leaf)

withdrawProcedure :: IO ()
withdrawProcedure = do
    secret <- getSecrets
    case secret of
      Nothing        -> print @String "Nothing to withdraw."
      Just (ds, sas) -> do
        state <- mixerStateProcedure
        (lastDeposit, subs, proof) <- generateSimulatedWithdrawProof pabWalletPKH ds sas state
        let params = WithdrawParams (lovelaceValueOf 200_000_000) lastDeposit pabWalletPKH subs proof
        cidUseMixer <- activateRequest UseMixer (Just pabWallet)
        endpointRequest "withdraw" cidUseMixer params

------------------------------- Query mixer logic --------------------------------

mixerStateProcedure :: IO MixerState
mixerStateProcedure = do
    cidQueryMixer <- activateRequest QueryMixer (Just pabWallet)
    endpointRequest "Get Mixer state" cidQueryMixer (lovelaceValueOf 200_000_000)
    go cidQueryMixer
  where go cid = do
                resp <- statusRequest cid
                case resp of
                    Just state -> return state
                    Nothing    -> do
                        threadDelay 1_000_000
                        go cid

---------------------------------- Relayer logic ---------------------------------

retrieveTimeLockedProcedure :: IO ()
retrieveTimeLockedProcedure = do
    cidTimeLocked <- activateRequest RetrieveTimeLocked (Just pabWallet)
    endpointRequest "retrieve funds" cidTimeLocked ()
    
----------------------- Create mixer admin key -----------------------------------

mintAdminKeyProcedure :: Wallet -> IO ()
mintAdminKeyProcedure w = do
    cidAdmin <- activateRequest MintAdminKey (Just w)
    endpointRequest "Create native token" cidAdmin (SimpleMPS adminKeyTokenName 1 (mockWalletPaymentPubKeyHash pabWallet))


    