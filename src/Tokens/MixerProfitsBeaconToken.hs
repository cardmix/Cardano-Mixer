{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Tokens.MixerProfitsBeaconToken where

import           Control.Monad.State              (State)
import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           PlutusTx.Prelude

import           Tokens.OneShotCurrency           (OneShotCurrencyParams, oneShotCurrencyPolicy, mkCurrency, oneShotCurrencyMintTx)
import           Types.TxConstructor              (TxConstructor)

--------------------------- On-Chain -----------------------------

{-# INLINABLE mixerProfitsBeaconTokenName #-}
mixerProfitsBeaconTokenName :: TokenName
mixerProfitsBeaconTokenName = TokenName ""

{-# INLINABLE mixProfitsBeaconParams #-}
mixProfitsBeaconParams :: TxOutRef -> OneShotCurrencyParams
mixProfitsBeaconParams ref = mkCurrency ref [(mixerProfitsBeaconTokenName, 1)]

curPolicy :: TxOutRef -> MintingPolicy
curPolicy = oneShotCurrencyPolicy . mixProfitsBeaconParams

-------------------------- Off-Chain -----------------------------

mixerProfitsBeaconCurrencySymbol :: TxOutRef -> CurrencySymbol
mixerProfitsBeaconCurrencySymbol = scriptCurrencySymbol . curPolicy

mixerProfitsBeaconAssetClass :: TxOutRef -> AssetClass
mixerProfitsBeaconAssetClass ref = AssetClass (mixerProfitsBeaconCurrencySymbol ref, mixerProfitsBeaconTokenName)

mixerProfitsBeaconToken :: TxOutRef -> Value
mixerProfitsBeaconToken = token . mixerProfitsBeaconAssetClass

mixerProfitsBeaconMintTx :: TxOutRef -> State (TxConstructor d a i o) (Maybe (TxOutRef, ChainIndexTxOut))
mixerProfitsBeaconMintTx = oneShotCurrencyMintTx . mixProfitsBeaconParams
