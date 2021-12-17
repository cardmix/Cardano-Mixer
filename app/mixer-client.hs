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

import           Ledger                                       (PaymentPubKeyHash)
import           Ledger.Ada                                   (lovelaceValueOf)
import           Plutus.Contracts.Currency                    (SimpleMPS(..))
import           PlutusTx.Prelude                             (zero, one)
import           Prelude                                      hiding (readFile)
import           System.CPUTime                               (getCPUTime)
import           System.Environment                           (getArgs)
import           Wallet.Emulator.Types                        (mockWalletPaymentPubKeyHash)
import           Wallet.Emulator.Wallet                       (Wallet (..))

import           AdminKey                                     (adminKeyTokenName)
import           Configuration.PABConfig                         (pabWallet)
import           Crypto
import           Mixer                                        (DepositParams(..), WithdrawParams(..))
import           MixerFactory                                 (StartParams(..))
import           MixerProofs                                  (generateWithdrawProof, verifyWithdraw)
import           PAB
import           Requests



main :: IO ()
main = do
    let pkh = mockWalletPaymentPubKeyHash pabWallet
    print pabWallet
    args <- getArgs
    case args of
        ["admin"]    -> mintAdminKeyProcedure pabWallet   -- for testing purposes
        ["start", s] -> startMixerProcedure pabWallet (startParams !! (read s -1))
        ["deposit"]  -> depositProcedure pabWallet pkh pkh
        ["withdraw"] -> do
                    proof <- withdrawTest pabWallet
                    withdrawProcedure pabWallet pkh pkh proof
        _            -> print ("Unknown command" :: String)

---------------------- Withdraw test ----------------------------------

withdrawTest :: Wallet -> IO Proof
withdrawTest w = do
    let r1 = toZp 12451 :: Fr
        r2 = toZp 6788546 :: Fr
        h  = mimcHash (toZp 0) r1
        hA = mimcHash (dataToZp $ mockWalletPaymentPubKeyHash w) r2
        v1 = toZp 890523 :: Fr
        v2 = toZp 35656 :: Fr
        v3 = toZp 97346 :: Fr
        oh = mimcHash v1 v2
        nh = mimcHash v1 v3
    let d = 10
        pkh = mockWalletPaymentPubKeyHash w
        
        a   = dataToZp pkh
        cp0 = replicate d zero
        c0  = 0
        k = mimcHash r1 r2
        cp  = addMerkleLeaf k (c0+1) cp0
        root = last cp

        l = replicate d zero :: [Fr]

        subsPub = [one, zero, zero, zero, zero, zero, root, a, h, hA, one, oh, nh] :: [Fr]
    t1 <- getCPUTime
    proof <- generateWithdrawProof (root, a, h, hA, one, oh, nh, r1, r2, init cp, l, v1, v2, v3)
    print proof
    t2 <- getCPUTime
    print $ (fromIntegral (t2 - t1) :: Double) / (10^(12 :: Integer))

    print $ verifyWithdraw subsPub proof
    t3 <- getCPUTime
    print $ (fromIntegral (t3 - t2) :: Double) / (10^(12 :: Integer))
    return proof

----------------------- Create mixer dApp logic ------------------------------

mintAdminKeyProcedure :: Wallet -> IO ()
mintAdminKeyProcedure w = do
    cidAdmin <- activateRequest MintAdminKey (Just w)
    endpointRequest "Create native token" cidAdmin (SimpleMPS adminKeyTokenName 1)

----------------------------- Start mixer logic ------------------------------

startParams :: [StartParams]
startParams = map (`StartParams` 10) lst
    where lst = [lovelaceValueOf 200_000_000, lovelaceValueOf 1000_000_000, lovelaceValueOf 10000_000_000]

startMixerProcedure :: Wallet -> StartParams -> IO ()
startMixerProcedure w p = do
    cidStart <- activateRequest Start (Just w)
    endpointRequest "start" cidStart p

--------------------------------- Use mixer logic --------------------------------

depositProcedure :: Wallet -> PaymentPubKeyHash -> PaymentPubKeyHash -> IO ()
depositProcedure w _ _ = do
    let r1 = toZp 12451 :: Fr
        r2 = toZp 6788546 :: Fr
        k = mimcHash r1 r2
    cidUseMixer <- activateRequest UseMixer (Just w)
    endpointRequest "deposit" cidUseMixer (DepositParams (lovelaceValueOf 200_000_000) k)
    -- stopRequest cidUseMixer

withdrawProcedure :: Wallet -> PaymentPubKeyHash -> PaymentPubKeyHash -> Proof -> IO ()
withdrawProcedure w _ _ proof = do
    let r1 = toZp 12451 :: Fr
        r2 = toZp 6788546 :: Fr
        h  = mimcHash (toZp 0) r1
        hA = mimcHash (dataToZp $ mockWalletPaymentPubKeyHash w) r2
        v1 = toZp 890523 :: Fr
        v2 = toZp 35656 :: Fr
        v3 = toZp 97346 :: Fr
        oh = mimcHash v1 v2
        nh = mimcHash v1 v3
    cidUseMixer <- activateRequest UseMixer (Just w)
    endpointRequest "withdraw" cidUseMixer (WithdrawParams (lovelaceValueOf 200_000_000) (mockWalletPaymentPubKeyHash w)
     h hA oh nh proof)
    -- stopRequest cidUseMixer
