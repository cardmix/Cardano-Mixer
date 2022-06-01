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

module Tokens.GovernanceDecisionToken where

import qualified Data.Map
import           Data.Maybe                       (fromJust)
import           Ledger                           (CurrencySymbol, ScriptContext (..), TxId (..), scriptCurrencySymbol, TxInfo(..), TxOut (..))
import           Ledger.Constraints               (TxConstraints, ScriptLookups)
import           Ledger.Scripts                   (MintingPolicy, mkMintingPolicyScript)
import           Ledger.Tokens                    (token)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Value                     (TokenName (..), Value, AssetClass(..), geq)
import           PlutusTx                         (compile)
import           PlutusTx.Prelude                 hiding (Monoid (..), Semigroup (..), Eq)
import           Prelude                          (mempty)

import           Scripts.Constraints              (utxoSpent, utxoSpentPublicKeyTx)
import           Tokens.GovernanceBeaconToken     (governanceBeaconTokenRequired)
import           Types.TxConstructor              (TxConstructor(..))

-------------------------------- On-chain ---------------------------------

{-# INLINABLE governanceDecisionTokenName #-}
governanceDecisionTokenName :: TxId -> TokenName
governanceDecisionTokenName tx = TokenName $ getTxId tx

checkPolicy :: () -> ScriptContext -> Bool
checkPolicy _ ctx =
    let info = scriptContextTxInfo ctx
    in governanceBeaconTokenRequired info

curPolicy :: MintingPolicy
curPolicy = mkMintingPolicyScript $$(compile [|| wrapMintingPolicy checkPolicy ||])

governanceDecisionCurrencySymbol :: CurrencySymbol
governanceDecisionCurrencySymbol = scriptCurrencySymbol curPolicy

governanceDecisionAssetClass :: TxId -> AssetClass
governanceDecisionAssetClass tx = AssetClass (governanceDecisionCurrencySymbol, governanceDecisionTokenName tx)

governanceDecisionToken :: TxId -> Value
governanceDecisionToken = token . governanceDecisionAssetClass

{-# INLINABLE governanceDecisionTokenRequired #-}
governanceDecisionTokenRequired :: TxInfo -> Bool
governanceDecisionTokenRequired info = utxoSpent (\o -> txOutValue o `geq` governanceDecisionToken tx) info
    where tx = txInfoId info

-------------------------- Off-Chain -----------------------------

-- TxConstraints that Governance Decision Token is spent by the transaction
governanceDecisionTokenTx :: TxId -> (ScriptLookups a, TxConstraints i o)
governanceDecisionTokenTx tx = fromJust $ txConstructorResult constr
    where constr = utxoSpentPublicKeyTx (\o -> txOutValue o `geq` governanceDecisionToken tx) $ TxConstructor Data.Map.empty $ Just (mempty, mempty)