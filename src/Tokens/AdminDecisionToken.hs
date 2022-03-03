{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE NumericUnderscores    #-}


module Tokens.AdminDecisionToken where

import           Ledger                           (CurrencySymbol, scriptCurrencySymbol, ScriptContext)
import           Ledger.Scripts                   (MintingPolicy, mkMintingPolicyScript)
import           Ledger.Tokens                    (token)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Value                     (TokenName, Value, AssetClass(..))
import           PlutusTx                         (compile)
import           PlutusTx.Prelude                 hiding (Monoid (..), Semigroup (..), Eq)

import           Tokens.AdminToken                (adminTokenRequired)


-------------------------------- On-chain ---------------------------------

checkPolicy :: () -> ScriptContext -> Bool
checkPolicy _ = adminTokenRequired

curPolicy :: MintingPolicy
curPolicy = mkMintingPolicyScript $$(compile [|| wrapMintingPolicy checkPolicy ||])

adminDecisionSymbol :: CurrencySymbol
adminDecisionSymbol = scriptCurrencySymbol curPolicy

adminDecisionAssetClass :: TokenName -> AssetClass
adminDecisionAssetClass tn = AssetClass (adminDecisionSymbol, tn)

adminDecisionToken :: TokenName -> Value
adminDecisionToken = token . adminDecisionAssetClass