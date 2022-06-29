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

import           Data.Map                                 (member, elems)
import           Data.Maybe                               (mapMaybe)
import           Data.Semigroup                           (Last (..))
import           Ledger                                   hiding (member, singleton, validatorHash, unspentOutputs)
import           Plutus.ChainIndex.Tx                     (ChainIndexTx(..))
import           Plutus.Contract                          (Promise, ContractError, Endpoint, endpoint, tell, Contract)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  ((<$>))

import           Contracts.ChainIndex                     (getUtxosAt)
import           Crypto
import           Scripts.MixerScript
import Scripts.VestingScript (VestingDatum, vestingValidatorHash, vestingValidatorAddress)
import Scripts.FailScript (failAddress)
-- import           Scripts.VestingScript                    (VestingParams (..), vestingScriptAddress, vestingScriptHash)

type MixerKeys = [Fr]

---------------------------------------------------------------------
--------------------------- Off-Chain -------------------------------
---------------------------------------------------------------------

-- instance Ord (ChainIndexTx, ChainIndexTxOut) where
--     (compare) (tx1, txo1) (tx2, txo2) = t1 `compare` t2
--         where
--             t1 = ivTo $ _citxValidRange tx1
--             t2 = ivTo $ _citxValidRange tx2

-- TODO: THIS IS NOT WORKING PROPERLY NOW. We should get the keys from the NFTs
getMixerKeys :: Value -> Contract w s ContractError MixerKeys
getMixerKeys v = do
    let mixer = Mixer v vestingValidatorHash vestingValidatorHash failAddress
    txoTxs <- elems <$> getUtxosAt vestingValidatorAddress
    let f (_, tx) = 
            let ValidatorHash h = mixerValidatorHash mixer
            in ScriptHash h `member` _citxScripts tx
        txoTxs' = filter f txoTxs
        keys    = mapMaybe (getTxKeys . fst) txoTxs'
    return keys

getTxKeys :: ChainIndexTxOut -> Maybe Fr
getTxKeys tx = do
        d <- either (const Nothing) Just $ _ciTxOutDatum tx
        p <- fromBuiltinData $ getDatum d :: Maybe VestingDatum
        -- return $ vestingWHash p
        return $ Zp 0

type MixerKeysSchema = Endpoint "get-mixer-keys" Value

getMixerKeysPromise :: Promise (Maybe (Last MixerKeys)) MixerKeysSchema ContractError ()
getMixerKeysPromise = endpoint @"get-mixer-keys" @Value $ \v -> do
    mKeys <- getMixerKeys v
    tell $ Just $ Last mKeys

