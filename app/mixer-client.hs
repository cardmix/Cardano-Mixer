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

import           Cardano.Api.Shelley                          (AsType (AsShelleyAddress), shelleyPayAddrToPlutusPubKHash, deserialiseFromBech32)
import           Control.Concurrent                           (threadDelay)
import           Data.Either                                  (fromRight)
import           Data.Text                                    (pack)
import           Ledger.Ada                                   (lovelaceValueOf)
import           Ledger.Address                               (PaymentPubKeyHash (..))
import           Ledger.Constraints.OffChain                  (UnbalancedTx(..))
import           PlutusTx.Prelude                             hiding ((<$>))
import           Prelude                                      (IO, String, print)
import           System.Environment                           (getArgs)
import           Wallet.Emulator.Types                        (mockWalletPaymentPubKeyHash)
import           Wallet.Emulator.Wallet                       (Wallet (..))

import           ClientLog
import           Configuration.PABConfig                      (pabWallet, pabWalletPKH)
import           Contracts.Currency                           (SimpleMPS(..))
import           Crypto                                       (mimcHash)
import           Crypto.Conversions                           (dataToZp)
import           MixerContract
import           MixerContractsDefinition
import           MixerProofs                                  (generateSimulatedWithdrawProof)
import           MixerState                                   (MixerState)
import           MixerUserData
import           Requests
import           Tokens.AdminToken                            (adminTokenName)


main :: IO ()
main = do
    print pabWallet
    args <- getArgs
    case args of
        ["admin"]         -> mintAdminKeyProcedure pabWallet   -- for testing purposes
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
    cidUseMixer <- activateRequest UseMixer Nothing
    endpointRequest "deposit" cidUseMixer (DepositParams (lovelaceValueOf 400_000) leaf)
    s <- go cidUseMixer :: IO UnbalancedTx
    print s
    where go cid = do
                resp <- statusRequest cid
                case resp of
                    Just state -> return state
                    Nothing    -> do
                        threadDelay 1_000_000
                        go cid

withdrawProcedure :: String -> IO ()
withdrawProcedure str = do
    let pkh = strToPKH str
    secret <- getSecrets
    case secret of
      Nothing        -> print @String "Nothing to withdraw."
      Just (ds, sas) -> do
        state <- mixerStateProcedure
        (lastDeposit, subs, proof) <- generateSimulatedWithdrawProof (dataToZp pkh) ds sas state
        let params = WithdrawParams (lovelaceValueOf 400_000) lastDeposit pkh subs proof
        cidUseMixer <- activateRequest UseMixer (Just pabWallet)
        endpointRequest "withdraw" cidUseMixer params

strToPKH :: String -> PaymentPubKeyHash
strToPKH str = case str of
                "" -> pabWalletPKH
                _  -> maybe (error ()) PaymentPubKeyHash $ shelleyPayAddrToPlutusPubKHash $
                        fromRight (error ()) $ deserialiseFromBech32 AsShelleyAddress $ pack str

------------------------------- Query mixer logic --------------------------------

mixerStateProcedure :: IO MixerState
mixerStateProcedure = do
    cidQueryMixer <- activateRequest QueryMixer (Just pabWallet)
    endpointRequest "Get Mixer state" cidQueryMixer (lovelaceValueOf 400_000)
    go cidQueryMixer
  where go cid = do
                resp <- statusRequest cid
                case resp of
                    Just state -> return state
                    Nothing    -> do
                        threadDelay 1_000_000
                        go cid

----------------------- Create mixer admin key -----------------------------------

mintAdminKeyProcedure :: Wallet -> IO ()
mintAdminKeyProcedure w = do
    cidAdmin <- activateRequest MintAdminKey (Just w)
    endpointRequest "Create native token" cidAdmin (SimpleMPS adminTokenName 1 (mockWalletPaymentPubKeyHash pabWallet))


    