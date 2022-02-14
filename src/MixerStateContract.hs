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


module MixerStateContract where

import           Data.Either                              (rights)
import           Data.Semigroup                           (Last (..))
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Plutus.Contract                          (Promise, ContractError, Endpoint, endpoint, tell, Contract)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

import           MixerScript
import           MixerState
import           Utils.Contracts                          (txosTxTxOutAt)
import Tokens.DepositToken (depositTokenTargetAddress)
import Crypto
import PlutusTx.Prelude (mapMaybe)

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
    let mData = mapMaybe ((fromBuiltinData :: BuiltinData -> Maybe ((Address, Value), (Fr, POSIXTime))) . getDatum) $ rights $ map (_ciTxOutDatum . snd) txTxos
        leafs = map (fst . snd) $ filter (\d -> fst (fst d) == mixerAddress mixer && snd (fst d) == val) mData
        state = snd $ constructStateFromList (leafs, [])
    return state

type MixerStateSchema = Endpoint "Get Mixer state" Value

getMixerStatePromise :: Promise (Maybe (Last MixerState)) MixerStateSchema ContractError ()
getMixerStatePromise = endpoint @"Get Mixer state" @Value $ \v -> do
    state <- getMixerState v
    tell $ Just $ Last state
