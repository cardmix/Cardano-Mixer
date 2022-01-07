{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
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

import           Data.Aeson                                   (encode, decode)
import           Data.ByteString.Lazy                         (writeFile, readFile)
import           Ledger.Ada                                   (lovelaceValueOf)
import           Prelude                                      hiding (readFile, writeFile)
import           System.Environment                           (getArgs)
import           Wallet.Emulator.Types                        (mockWalletPaymentPubKeyHash)
import           Wallet.Emulator.Wallet                       (Wallet (..))

import           AdminKey                                     (adminKeyTokenName)
import           Configuration.PABConfig                      (pabWallet, pabTestValues, pabWalletPKH)
import           Contracts.Currency                           (SimpleMPS(..))
import           MixerContract
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
        _            -> print ("Unknown command" :: String)

--------------------------------- Use mixer logic --------------------------------

depositProcedure :: IO ()
depositProcedure = do
    ds   <- generateDepositSecret 1
    sas  <- generateShieldedAccountSecret
    writeFile "DepositSecret" (encode ds)
    writeFile "ShieldedAccountSecret" (encode sas)
    (leaf, _, _, _, _, _) <- pabTestValues ds sas
    cidUseMixer <- activateRequest UseMixer (Just pabWallet)
    endpointRequest "deposit" cidUseMixer (DepositParams (lovelaceValueOf 200_000_000) leaf)

withdrawProcedure :: IO ()
withdrawProcedure = do
    Just ds <- decode <$> readFile "DepositSecret"
    Just sas <- decode <$> readFile "ShieldedAccountSecret"
    (_, proof, key, keyA, oh, nh) <- pabTestValues ds sas
    state <- mixerStateProcedure
    print $ head $ head state
    let params = WithdrawParams (lovelaceValueOf 200_000_000) pabWalletPKH key keyA oh nh proof
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
                    Nothing    -> go cid
    

----------------------- Create mixer admin key -----------------------------------

mintAdminKeyProcedure :: Wallet -> IO ()
mintAdminKeyProcedure w = do
    cidAdmin <- activateRequest MintAdminKey (Just w)
    endpointRequest "Create native token" cidAdmin (SimpleMPS adminKeyTokenName 1 (mockWalletPaymentPubKeyHash pabWallet))


    