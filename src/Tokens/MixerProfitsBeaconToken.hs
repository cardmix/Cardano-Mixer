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

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))

import           Configuration.PABConfig          (mixProfitsBeaconTxOutRef)
import           Tokens.OneShotCurrency           (OneShotCurrencyParams, oneShotCurrencyPolicy, mkCurrency, oneShotCurrencyMintTx)
import           Types.TxConstructor              (TxConstructor)

--------------------------- On-Chain -----------------------------

{-# INLINABLE mixerProfitsBeaconTokenName #-}
mixerProfitsBeaconTokenName :: TokenName
mixerProfitsBeaconTokenName = TokenName ""

{-# INLINABLE mixProfitsBeaconParams #-}
mixProfitsBeaconParams :: OneShotCurrencyParams
mixProfitsBeaconParams = mkCurrency mixProfitsBeaconTxOutRef [(mixerProfitsBeaconTokenName, 1)]

curPolicy :: MintingPolicy
curPolicy = oneShotCurrencyPolicy mixProfitsBeaconParams

-------------------------- Off-Chain -----------------------------

mixerProfitsBeaconCurrencySymbol :: CurrencySymbol
mixerProfitsBeaconCurrencySymbol = scriptCurrencySymbol curPolicy

mixerProfitsBeaconAssetClass :: AssetClass
mixerProfitsBeaconAssetClass = AssetClass (mixerProfitsBeaconCurrencySymbol, mixerProfitsBeaconTokenName)

mixerProfitsBeaconToken :: Value
mixerProfitsBeaconToken = token mixerProfitsBeaconAssetClass

mixerProfitsBeaconMintTx :: TxConstructor a i o -> TxConstructor a i o
mixerProfitsBeaconMintTx = oneShotCurrencyMintTx mixProfitsBeaconParams
