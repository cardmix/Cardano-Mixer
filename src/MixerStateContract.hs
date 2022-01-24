{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module MixerStateContract where

import           Data.Either                              (rights)
import           Data.Semigroup                           (Last (..))
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Value                             (geq)
import           Plutus.Contract                          (Promise, ContractError, Endpoint, endpoint, tell, Contract)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

import           MixerScript
import           MixerState
import           Utils.Contracts                          (txosTxTxOutAt)

---------------------------------------------------------------------
--------------------------- Off-Chain -------------------------------
---------------------------------------------------------------------

-- instance Ord (ChainIndexTx, ChainIndexTxOut) where
--     (compare) (tx1, txo1) (tx2, txo2) = t1 `compare` t2
--         where
--             t1 = ivTo $ _citxValidRange tx1
--             t2 = ivTo $ _citxValidRange tx2

getMixerState :: Value -> Contract (Maybe (Last MixerState)) s ContractError MixerState
getMixerState v = do
    let mixer = makeMixerFromFees v
    txTxos <- txosTxTxOutAt (mixerAddress mixer)
    let txTxos' = filter (\(_, o) -> _ciTxOutValue o `geq` (mValue mixer + mTotalFees mixer)) txTxos -- TODO: implement proper sort?
        leafs   = map (getMixerDatum . unsafeFromBuiltinData . getDatum) $ rights $ map (_ciTxOutDatum . snd) txTxos'
    let state = snd $ constructStateFromList (leafs, [])
    tell $ Just $ Last state
    return state

type MixerStateSchema = Endpoint "Get Mixer state" Value

getMixerStatePromise :: Promise (Maybe (Last MixerState)) MixerStateSchema ContractError MixerState
getMixerStatePromise = endpoint @"Get Mixer state" @Value getMixerState