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

import           Ledger.Ada                                   (lovelaceValueOf)
import           Prelude                                      hiding (readFile)
import           System.Environment                           (getArgs)
import           Wallet.Emulator.Types                        (mockWalletPaymentPubKeyHash)
import           Wallet.Emulator.Wallet                       (Wallet (..))

import           AdminKey                                     (adminKeyTokenName)
import           Configuration.PABConfig                      (pabWallet, pabTestValues, pabWalletPKH)
import           Contracts.Currency                           (SimpleMPS(..))
import           MixerScript
import           MixerState                                   (MixerState)
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
    (leaf, _, _, _, _, _) <- pabTestValues
    cidUseMixer <- activateRequest UseMixer (Just pabWallet)
    endpointRequest "deposit" cidUseMixer (DepositParams (lovelaceValueOf 200_000_000) leaf)

withdrawProcedure :: IO ()
withdrawProcedure = do
    (_, proof, key, keyA, oh, nh) <- pabTestValues
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

---------------------- Withdraw test ----------------------------------

-- withdrawTest :: Wallet -> IO Proof
-- withdrawTest w = do
--     let r1 = toZp 12451 :: Fr
--         r2 = toZp 6788546 :: Fr
--         h  = mimcHash (toZp 0) r1
--         hA = mimcHash (dataToZp $ mockWalletPaymentPubKeyHash w) r2
--         v1 = toZp 890523 :: Fr
--         v2 = toZp 35656 :: Fr
--         v3 = toZp 97346 :: Fr
--         oh = mimcHash v1 v2
--         nh = mimcHash v1 v3
--     let d = 10
--         pkh = mockWalletPaymentPubKeyHash w

--         a   = dataToZp pkh
--         cp0 = replicate d zero
--         c0  = 0
--         k = mimcHash r1 r2
--         cp  = addMerkleLeaf k (c0+1) cp0
--         root = last cp

--         l = replicate d zero :: [Fr]

--         subsPub = [one, zero, zero, zero, zero, zero, root, a, h, hA, one, oh, nh] :: [Fr]
--     t1 <- getCPUTime
--     proof <- generateWithdrawProof (root, a, h, hA, one, oh, nh, r1, r2, init cp, l, v1, v2, v3)
--     print proof
--     t2 <- getCPUTime
--     print $ (fromIntegral (t2 - t1) :: Double) / (10^(12 :: Integer))

--     print $ verifyWithdraw subsPub proof
--     t3 <- getCPUTime
--     print $ (fromIntegral (t3 - t2) :: Double) / (10^(12 :: Integer))
--     return proof