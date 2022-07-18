{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Tokens.GovernanceBeaconToken where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)

import           Tokens.OneShotCurrency           (OneShotCurrencyParams, oneShotCurrencyPolicy, mkCurrency)

-- import           Configuration.PABConfig          (governanceBeaconTokenTxOutRef)

--------------------------- On-Chain -----------------------------

{-# INLINABLE governanceBeaconTokenName #-}
governanceBeaconTokenName :: TokenName
governanceBeaconTokenName = TokenName "Cardano Mixer Admin Token"

{-# INLINABLE governanceBeaconParams #-}
governanceBeaconParams :: TxOutRef -> OneShotCurrencyParams
governanceBeaconParams ref = mkCurrency ref [(governanceBeaconTokenName, 1)]

curPolicy :: TxOutRef -> MintingPolicy
curPolicy = oneShotCurrencyPolicy . governanceBeaconParams

-------------------------- Off-Chain -----------------------------

governanceBeaconCurrencySymbol :: TxOutRef -> CurrencySymbol
governanceBeaconCurrencySymbol = scriptCurrencySymbol . curPolicy

governanceBeaconAssetClass :: TxOutRef -> AssetClass
governanceBeaconAssetClass ref = AssetClass (governanceBeaconCurrencySymbol ref, governanceBeaconTokenName)

governanceBeaconToken :: TxOutRef -> Value
governanceBeaconToken = token . governanceBeaconAssetClass
