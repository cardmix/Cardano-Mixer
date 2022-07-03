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
import           PlutusTx.Prelude

import           Mixer                            (MixerInstance (..))
import           Tokens.OneShotCurrency           (OneShotCurrencyParams, oneShotCurrencyPolicy, mkCurrency, oneShotCurrencyMintTx)
import           Types.TxConstructor              (TxConstructor)

--------------------------- On-Chain -----------------------------

{-# INLINABLE mixerBeaconTokenName #-}
mixerBeaconTokenName :: TokenName
mixerBeaconTokenName = TokenName ""

{-# INLINABLE mixerBeaconParams #-}
mixerBeaconParams :: TxOutRef -> OneShotCurrencyParams
mixerBeaconParams ref = mkCurrency ref [(mixerBeaconTokenName, 1)]

curPolicy :: TxOutRef -> MintingPolicy
curPolicy = oneShotCurrencyPolicy . mixerBeaconParams

-------------------------- Off-Chain -----------------------------

mixerBeaconCurrencySymbol :: TxOutRef -> CurrencySymbol
mixerBeaconCurrencySymbol = scriptCurrencySymbol . curPolicy

mixerBeaconAssetClass :: TxOutRef -> AssetClass
mixerBeaconAssetClass ref = AssetClass (mixerBeaconCurrencySymbol ref, mixerBeaconTokenName)

mixerBeaconToken :: TxOutRef -> Value
mixerBeaconToken = token . mixerBeaconAssetClass

mixerBeaconMintTx :: MixerInstance -> TxConstructor a i o -> TxConstructor a i o
mixerBeaconMintTx = oneShotCurrencyMintTx . mixerBeaconParams . miMixerBeaconTxOutRef
