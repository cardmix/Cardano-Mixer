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

module Tokens.MixerProfitsBeaconToken where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints               (TxConstraints)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           PlutusTx                         (compile, applyCode, liftCode)
import           PlutusTx.Prelude                 
import           Prelude                          (undefined)

--------------------------- On-Chain -----------------------------

type MixerProfitsBeaconParams = ()

type MixerProfitsBeaconRedeemer = ()

{-# INLINABLE mixerProfitsBeaconTokenName #-}
mixerProfitsBeaconTokenName :: MixerProfitsBeaconRedeemer -> TokenName
mixerProfitsBeaconTokenName _ = TokenName ""

checkPolicy :: MixerProfitsBeaconParams -> MixerProfitsBeaconRedeemer -> ScriptContext -> Bool
checkPolicy _ _ _ = True

curPolicy :: MixerProfitsBeaconParams -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

-------------------------- Off-Chain -----------------------------

mixerProfitsBeaconCurrencySymbol :: MixerProfitsBeaconParams -> CurrencySymbol
mixerProfitsBeaconCurrencySymbol = scriptCurrencySymbol . curPolicy

mixerProfitsBeaconAssetClass :: MixerProfitsBeaconParams -> MixerProfitsBeaconRedeemer -> AssetClass
mixerProfitsBeaconAssetClass par red = AssetClass (mixerProfitsBeaconCurrencySymbol par, mixerProfitsBeaconTokenName red)

mixerProfitsBeaconToken :: MixerProfitsBeaconParams -> MixerProfitsBeaconRedeemer -> Value
mixerProfitsBeaconToken par = token . mixerProfitsBeaconAssetClass par

mixerProfitsBeaconTokenTx :: MixerProfitsBeaconParams -> MixerProfitsBeaconRedeemer -> TxConstraints i o
mixerProfitsBeaconTokenTx _ _ = undefined
