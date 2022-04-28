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
import           Plutus.Contract                          (Promise, Contract, ContractError, Endpoint, currentTime, endpoint, tell, logInfo)
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<>), (<$>), unless, find, toList, fromInteger, check)
import           Prelude                                  (Show, String)

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

getMixerState :: MixerStateCache -> Value -> Contract w s ContractError (MixerState, MixerStateCache)
getMixerState oldCache@(MixerStateCache cTxs cTime) v curTime = do
    let mixer = makeMixerFromFees v
    
    txTxos  <- mixerStateCacheIsValid curTime (pure cTxs) (txosTxTxOutAt depositTokenTargetAddress)
    cache   <- mixerStateCacheIsValid curTime (pure oldCache) (pure $ MixerStateCache txTxos curTime)
    
    -- TODO: implement proper sort?
    let outs  = map snd txTxos
    let f o = do
            d  <- either (const Nothing) Just $ _ciTxOutDatum o
            ((addr, mv), (leaf, t)) <- fromBuiltinData $ getDatum d :: Maybe ((Address, Value), (Fr, POSIXTime))
            let tokenCheck = _ciTxOutValue o `geq` depositToken (addr, mv) (leaf, t)
                addrCheck  = addr == mixerAddress mixer
                valCheck   = mv   == v
            if tokenCheck && addrCheck && valCheck then Just leaf else Nothing
        leafs = mapMaybe f outs
        state = snd $ constructStateFromList (leafs, [])
    return (state, cache)
  where
      mixerStateCacheIsValid :: POSIXTime -> Contract w s ContractError a -> Contract w s ContractError a -> Contract w s ContractError a
      mixerStateCacheIsValid ct y n = if ct - cTime <= cacheValidityPeriod then y else n

type MixerStateSchema = Endpoint "get-mixer-state" [Value]

getMixerStatePromise :: Promise (Maybe (Last [MixerState])) MixerStateSchema ContractError ()
getMixerStatePromise = endpoint @"get-mixer-state" @[Value] $ \vals -> do
    (_, cache) <- getMixerState (MixerStateCache [] 0) zero zero
    logInfo @String "Cached txos"
    curTime <- currentTime
    states <- mapM (fmap fst . getMixerState cache) vals curTime
    logInfo @String "Retrieved states"
    tell $ Just $ Last states
