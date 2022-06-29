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

module Tokens.WithdrawBeaconToken where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints               (TxConstraints)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           PlutusTx                         (compile, applyCode, liftCode)
import           PlutusTx.Prelude                 
import           Prelude                          (undefined)

--------------------------- On-Chain -----------------------------

type WithdrawBeaconParams = ()

type WithdrawBeaconRedeemer = ()

{-# INLINABLE withdrawBeaconTokenName #-}
withdrawBeaconTokenName :: WithdrawBeaconRedeemer -> TokenName
withdrawBeaconTokenName _ = TokenName ""

checkPolicy :: WithdrawBeaconParams -> WithdrawBeaconRedeemer -> ScriptContext -> Bool
checkPolicy _ _ _ = True

curPolicy :: WithdrawBeaconParams -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

-------------------------- Off-Chain -----------------------------

withdrawBeaconCurrencySymbol :: WithdrawBeaconParams -> CurrencySymbol
withdrawBeaconCurrencySymbol = scriptCurrencySymbol . curPolicy

withdrawBeaconAssetClass :: WithdrawBeaconParams -> WithdrawBeaconRedeemer -> AssetClass
withdrawBeaconAssetClass par red = AssetClass (withdrawBeaconCurrencySymbol par, withdrawBeaconTokenName red)

withdrawBeaconToken :: WithdrawBeaconParams -> WithdrawBeaconRedeemer -> Value
withdrawBeaconToken par = token . withdrawBeaconAssetClass par

withdrawBeaconTokenTx :: WithdrawBeaconParams -> WithdrawBeaconRedeemer -> TxConstraints i o
withdrawBeaconTokenTx _ _ = undefined
