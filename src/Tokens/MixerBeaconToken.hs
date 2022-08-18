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

import           Control.Monad.State              (State)
import           Data.Functor                     (($>))
import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           PlutusTx.Prelude

import           Scripts.Constraints              (utxoProducedScriptTx)
import           Tokens.OneShotCurrency           (OneShotCurrencyParams, oneShotCurrencyPolicy, mkCurrency, oneShotCurrencyMintTx)
import           Types.MixerInstance              (MixerInstance (..))
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

mixerBeaconMintTx :: MixerInstance -> State (TxConstructor d a i o) ()
mixerBeaconMintTx mi = oneShotCurrencyMintTx (mixerBeaconParams $ miMixerBeaconTxOutRef mi) $> ()

mixerBeaconSendTx :: MixerInstance -> State (TxConstructor d a i o) ()
mixerBeaconSendTx mi = utxoProducedScriptTx val Nothing v ()
  where val = miMixerValidatorHash mi
        v   = mixerBeaconToken $ miMixerBeaconTxOutRef mi