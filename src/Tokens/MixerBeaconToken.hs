{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Tokens.MixerBeaconToken where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))

import           Configuration.PABConfig          (mixerBeaconTxOutRef)
import           Tokens.OneShotCurrency           (OneShotCurrencyParams, oneShotCurrencyPolicy, mkCurrency, oneShotCurrencyMintTx)
import           Types.TxConstructor              (TxConstructor)

--------------------------- On-Chain -----------------------------

{-# INLINABLE mixerBeaconTokenName #-}
mixerBeaconTokenName :: TokenName
mixerBeaconTokenName = TokenName ""

{-# INLINABLE mixerBeaconParams #-}
mixerBeaconParams :: OneShotCurrencyParams
mixerBeaconParams = mkCurrency mixerBeaconTxOutRef [(mixerBeaconTokenName, 1)]

curPolicy :: MintingPolicy
curPolicy = oneShotCurrencyPolicy mixerBeaconParams

-------------------------- Off-Chain -----------------------------

mixerBeaconCurrencySymbol :: CurrencySymbol
mixerBeaconCurrencySymbol = scriptCurrencySymbol curPolicy

mixerBeaconAssetClass :: AssetClass
mixerBeaconAssetClass = AssetClass (mixerBeaconCurrencySymbol, mixerBeaconTokenName)

mixerBeaconToken :: Value
mixerBeaconToken = token mixerBeaconAssetClass

mixerBeaconMintTx :: TxConstructor a i o -> TxConstructor a i o
mixerBeaconMintTx = oneShotCurrencyMintTx mixerBeaconParams
