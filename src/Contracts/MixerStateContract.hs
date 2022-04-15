{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Contracts.MixerStateContract where

import           Cardano.Api                              (FromJSON, ToJSON)
import           Data.Semigroup                           (Last (..))
import           GHC.Generics                             (Generic)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Value                             (geq)
import           Plutus.ChainIndex.Tx                     (ChainIndexTx)
import           Plutus.Contract                          (Promise, ContractError, Endpoint, tell, Contract, logInfo, currentTime, endpointWithMeta)
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<>), (<$>), unless, find, toList, fromInteger, check)
import           Prelude                                  (Show)

import           Crypto
import           MixerState
import           Scripts.MixerScript
import           Tokens.DepositToken                      (depositTokenTargetAddress, depositToken)
import           Utils.Contracts                          (txosTxTxOutAt)

--------------------------- Types -----------------------------------

data MixerStateCache = MixerStateCache
    {
        mixerStateCacheTxs  :: [(ChainIndexTx, ChainIndexTxOut)],
        mixerStateCacheTime :: POSIXTime
    }
    deriving (Show, Generic, FromJSON, ToJSON)

-- cache validity is 10s
cacheValidityPeriod :: POSIXTime
cacheValidityPeriod = 10000

-- instance Ord (ChainIndexTx, ChainIndexTxOut) where
--     (compare) (tx1, txo1) (tx2, txo2) = t1 `compare` t2
--         where
--             t1 = ivTo $ _citxValidRange tx1
--             t2 = ivTo $ _citxValidRange tx2

--------------------------- Off-Chain -------------------------------

getMixerState :: MixerStateCache -> Value -> Contract w s ContractError MixerState
getMixerState (MixerStateCache cTxs cTime) v = do
    let mixer = makeMixerFromFees v
        val   = mValue mixer + mTotalFees mixer
    curTime <- currentTime
    txTxos  <- if curTime - cTime <= cacheValidityPeriod then pure cTxs else txosTxTxOutAt depositTokenTargetAddress
    -- TODO: implement proper sort?
    let outs  = map snd txTxos
    let f o = do
            d  <- either (const Nothing) Just $ _ciTxOutDatum o
            ((addr, mv), (leaf, t)) <- fromBuiltinData $ getDatum d :: Maybe ((Address, Value), (Fr, POSIXTime))
            let tokenCheck = _ciTxOutValue o `geq` depositToken (addr, mv) (leaf, t)
                addrCheck  = addr == mixerAddress mixer
                valCheck   = mv   == val
            if tokenCheck && addrCheck && valCheck then Just leaf else Nothing
        leafs = mapMaybe f outs
        state = snd $ constructStateFromList (leafs, [])
    return state

type MixerStateSchema = Endpoint "Get Mixer state" [Value]

getMixerStatePromise :: MixerStateCache -> Promise (Maybe (Last [MixerState])) MixerStateSchema ContractError ()
getMixerStatePromise cache = endpointWithMeta @"Get Mixer state" @[Value] cache $ \vals -> do
    states <- mapM (getMixerState cache) vals
    logInfo states
    tell $ Just $ Last states
