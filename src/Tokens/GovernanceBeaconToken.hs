{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Tokens.GovernanceBeaconToken (
    governanceBeaconToken,
    governanceBeaconCurrencySymbol,
    governanceBeaconTokenName,
    governanceBeaconAssetClass,
    governanceBeaconTokenRequired,
    governanceBeaconTokenTx
) where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints               (mustSpendAtLeast)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..), CurrencySymbol (..), geq)
import           Plutus.Contract.StateMachine
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)

import           Scripts.Constraints
import           Configuration.PABConfig          (governanceBeaconTokenPolicyId)

------------------------------------ Admin Token ---------------------------------------------------

{-# INLINABLE governanceBeaconTokenName #-}
governanceBeaconTokenName :: TokenName
governanceBeaconTokenName = TokenName "Cardano Mixer Admin Token"

{-# INLINABLE governanceBeaconCurrencySymbol #-}
governanceBeaconCurrencySymbol :: CurrencySymbol
governanceBeaconCurrencySymbol = CurrencySymbol $ foldr consByteString emptyByteString governanceBeaconTokenPolicyId

{-# INLINABLE governanceBeaconAssetClass #-}
governanceBeaconAssetClass :: AssetClass
governanceBeaconAssetClass = AssetClass (governanceBeaconCurrencySymbol, governanceBeaconTokenName)

{-# INLINABLE governanceBeaconToken #-}
governanceBeaconToken :: Value
governanceBeaconToken = token governanceBeaconAssetClass

--------------------------- On-Chain -----------------------------

{-# INLINABLE governanceBeaconTokenRequired #-}
governanceBeaconTokenRequired :: TxInfo -> Bool
governanceBeaconTokenRequired = utxoSpent (\o -> txOutValue o `geq` governanceBeaconToken)

-------------------------- Off-Chain -----------------------------

-- TxConstraints that Admin Token is consumed by transaction
governanceBeaconTokenTx :: TxConstraints i o
governanceBeaconTokenTx = mustSpendAtLeast governanceBeaconToken