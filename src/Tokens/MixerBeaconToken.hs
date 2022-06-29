{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Tokens.MixerBeaconToken where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints               (TxConstraints)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           PlutusTx                         (compile, applyCode, liftCode)
import           PlutusTx.Prelude                 
import           Prelude                          (undefined)

--------------------------- On-Chain -----------------------------

type MixerBeaconParams = ()

type MixerBeaconRedeemer = ()

{-# INLINABLE mixerBeaconTokenName #-}
mixerBeaconTokenName :: MixerBeaconRedeemer -> TokenName
mixerBeaconTokenName _ = TokenName ""

checkPolicy :: MixerBeaconParams -> MixerBeaconRedeemer -> ScriptContext -> Bool
checkPolicy _ _ _ = True

curPolicy :: MixerBeaconParams -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

-------------------------- Off-Chain -----------------------------

mixerBeaconCurrencySymbol :: MixerBeaconParams -> CurrencySymbol
mixerBeaconCurrencySymbol = scriptCurrencySymbol . curPolicy

mixerBeaconAssetClass :: MixerBeaconParams -> MixerBeaconRedeemer -> AssetClass
mixerBeaconAssetClass par red = AssetClass (mixerBeaconCurrencySymbol par, mixerBeaconTokenName red)

mixerBeaconToken :: MixerBeaconParams -> MixerBeaconRedeemer -> Value
mixerBeaconToken par = token . mixerBeaconAssetClass par

mixerBeaconTokenTx :: MixerBeaconParams -> MixerBeaconRedeemer -> TxConstraints i o
mixerBeaconTokenTx _ _ = undefined
