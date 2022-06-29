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

module Tokens.DepositBeaconToken where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints               (TxConstraints)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           PlutusTx                         (compile, applyCode, liftCode)
import           PlutusTx.Prelude                 
import           Prelude                          (undefined)

--------------------------- On-Chain -----------------------------

type DepositBeaconParams = ()

type DepositBeaconRedeemer = ()

{-# INLINABLE depositBeaconTokenName #-}
depositBeaconTokenName :: DepositBeaconRedeemer -> TokenName
depositBeaconTokenName _ = TokenName ""

checkPolicy :: DepositBeaconParams -> DepositBeaconRedeemer -> ScriptContext -> Bool
checkPolicy _ _ _ = True

curPolicy :: DepositBeaconParams -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

-------------------------- Off-Chain -----------------------------

depositBeaconCurrencySymbol :: DepositBeaconParams -> CurrencySymbol
depositBeaconCurrencySymbol = scriptCurrencySymbol . curPolicy

depositBeaconAssetClass :: DepositBeaconParams -> DepositBeaconRedeemer -> AssetClass
depositBeaconAssetClass par red = AssetClass (depositBeaconCurrencySymbol par, depositBeaconTokenName red)

depositBeaconToken :: DepositBeaconParams -> DepositBeaconRedeemer -> Value
depositBeaconToken par = token . depositBeaconAssetClass par

depositBeaconTokenTx :: DepositBeaconParams -> DepositBeaconRedeemer -> TxConstraints i o
depositBeaconTokenTx _ _ = undefined
