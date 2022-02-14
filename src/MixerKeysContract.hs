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


module MixerKeysContract where

import           Data.Maybe                               (mapMaybe)
import           Data.Semigroup                           (Last (..))
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Value                             (geq)
import           Plutus.Contract                          (Promise, ContractError, Endpoint, endpoint, tell, Contract)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

import           Contracts.Vesting                        (vestingScriptAddress, VestingParams (..))
import           Crypto
import           MixerScript
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

getMixerKeys :: Value -> Contract w s ContractError MixerKeys
getMixerKeys v = do
    let mixer = makeMixerFromFees v
    txTxos <- txosTxTxOutAt vestingScriptAddress
    let txTxos' = filter (\(_, o) -> _ciTxOutValue o `geq` mRelayerCollateral mixer) txTxos
        keys    = mapMaybe (getTxKeys . snd) txTxos'
    return keys

getTxKeys :: ChainIndexTxOut -> Maybe Fr
getTxKeys tx = do
        d <- either (const Nothing) Just $ _ciTxOutDatum tx
        p <- fromBuiltinData $ getDatum d :: Maybe VestingParams
        -- let VestingData _ _ subs _ = vestingData p
        -- if length subs >= 3 then Just $ subs !! 2 else Nothing
        return $ vestingWHash p

type MixerKeysSchema = Endpoint "Get Mixer keys" Value

getMixerKeysPromise :: Promise (Maybe (Last MixerKeys)) MixerKeysSchema ContractError ()
getMixerKeysPromise = endpoint @"Get Mixer keys" @Value $ \v -> do
    mKeys <- getMixerKeys v
    tell $ Just $ Last mKeys

