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


module Contracts.MixerStateContract where

import           Data.Semigroup                           (Last (..))
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Value                             (geq)
import           Plutus.Contract                          (Promise, ContractError, Endpoint, endpoint, tell, Contract)
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<>), (<$>), unless, find, toList, fromInteger, check)

import           Crypto
import           MixerState
import           Scripts.MixerScript
import           Tokens.DepositToken                      (depositTokenTargetAddress, depositToken)
import           Utils.Contracts                          (txosTxTxOutAt)


---------------------------------------------------------------------
--------------------------- Off-Chain -------------------------------
---------------------------------------------------------------------

-- instance Ord (ChainIndexTx, ChainIndexTxOut) where
--     (compare) (tx1, txo1) (tx2, txo2) = t1 `compare` t2
--         where
--             t1 = ivTo $ _citxValidRange tx1
--             t2 = ivTo $ _citxValidRange tx2

getMixerState :: Value -> Contract w s ContractError MixerState
getMixerState v = do
    let mixer = makeMixerFromFees v
        val   = mValue mixer + mTotalFees mixer
    txTxos <- txosTxTxOutAt depositTokenTargetAddress
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

type MixerStateSchema = Endpoint "Get Mixer state" Value

getMixerStatePromise :: Promise (Maybe (Last MixerState)) MixerStateSchema ContractError ()
getMixerStatePromise = endpoint @"Get Mixer state" @Value $ \v -> do
    state <- getMixerState v
    tell $ Just $ Last state
