{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}


module Contracts.MixerKeysContract where

import           Data.Map                                 (member)
import           Data.Maybe                               (mapMaybe)
import           Data.Semigroup                           (Last (..))
import           Ledger                                   hiding (member, singleton, validatorHash, unspentOutputs)
import           Plutus.ChainIndex.Tx                     (ChainIndexTx(..))
import           Plutus.Contract                          (Promise, ContractError, Endpoint, endpoint, tell, Contract)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

import           Crypto
import           Scripts.MixerScript
import           Scripts.VestingScript                    (vestingScriptAddress, VestingParams (..))
import           Utils.Contracts                          (txosTxTxOutAt)

type MixerKeys = [Fr]

---------------------------------------------------------------------
--------------------------- Off-Chain -------------------------------
---------------------------------------------------------------------

-- instance Ord (ChainIndexTx, ChainIndexTxOut) where
--     (compare) (tx1, txo1) (tx2, txo2) = t1 `compare` t2
--         where
--             t1 = ivTo $ _citxValidRange tx1
--             t2 = ivTo $ _citxValidRange tx2

-- TODO: Fix possible exploits
getMixerKeys :: Value -> Contract w s ContractError MixerKeys
getMixerKeys v = do
    let mixer = makeMixerFromFees v
    txTxos <- txosTxTxOutAt vestingScriptAddress
    let f (tx, _) = 
            let ValidatorHash h = mixerValidatorHash mixer
            in ScriptHash h `member` _citxScripts tx
        txTxos' = filter f txTxos
        keys    = mapMaybe (getTxKeys . snd) txTxos'
    return keys

getTxKeys :: ChainIndexTxOut -> Maybe Fr
getTxKeys tx = do
        d <- either (const Nothing) Just $ _ciTxOutDatum tx
        p <- fromBuiltinData $ getDatum d :: Maybe VestingParams
        return $ vestingWHash p

type MixerKeysSchema = Endpoint "Get Mixer keys" Value

getMixerKeysPromise :: Promise (Maybe (Last MixerKeys)) MixerKeysSchema ContractError ()
getMixerKeysPromise = endpoint @"Get Mixer keys" @Value $ \v -> do
    mKeys <- getMixerKeys v
    tell $ Just $ Last mKeys

