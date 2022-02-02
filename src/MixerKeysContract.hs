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
import           Data.Set                                 (toList)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Value                             (geq)
import           Plutus.ChainIndex.Tx                     (ChainIndexTx(..))
import           Plutus.Contract                          (Promise, ContractError, Endpoint, endpoint, tell, Contract)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

import           Contracts.Vesting                        (vestingScriptAddress)
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

getTxKeys :: Mixer -> ChainIndexTx -> [Fr]
getTxKeys mixer tx = mapMaybe f $ mapMaybe txInType $ toList $ _citxInputs tx
    where f a  = case a of
                ConsumeScriptAddress val red _ ->
                    if scriptAddress val /= mixerAddress mixer
                        then Nothing
                    else let MixerRedeemer _ _ subs _ = unsafeFromBuiltinData $ getRedeemer red
                         in if length subs >= 3 then Just $ subs !! 2 else Nothing
                _ -> Nothing

getMixerKeys :: Value -> Contract w s ContractError MixerKeys
getMixerKeys v = do
    let mixer = makeMixerFromFees v
    txTxos <- txosTxTxOutAt vestingScriptAddress
    let txTxos' = filter (\(_, o) -> _ciTxOutValue o `geq` (mValue mixer + mTotalFees mixer)) txTxos
        keys    = concatMap (getTxKeys mixer . fst) txTxos'
    return keys

type MixerKeysSchema = Endpoint "Get Mixer keys" Value

getMixerKeysPromise :: Promise (Maybe (Last MixerKeys)) MixerKeysSchema ContractError ()
getMixerKeysPromise = endpoint @"Get Mixer keys" @Value $ \v -> do
    mKeys <- getMixerKeys v
    tell $ Just $ Last mKeys

