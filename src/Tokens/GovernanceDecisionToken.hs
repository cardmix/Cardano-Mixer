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

import           Control.Monad.State              (State)
import           Ledger                           (CurrencySymbol, ScriptContext (..), TxId (..), TxInfo(..),  TxOut (..),
                                                    ChainIndexTxOut (..), TxOutRef, scriptCurrencySymbol)
import           Ledger.Scripts                   (MintingPolicy, mkMintingPolicyScript)
import           Ledger.Tokens                    (token)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Value                     (TokenName (..), Value, AssetClass(..), geq)
import           PlutusTx                         (compile)
import           PlutusTx.Prelude                 hiding (Monoid (..), Semigroup (..), Eq)

import           Scripts.Constraints              (utxoSpent, utxoSpentPublicKeyTx)
import           Types.TxConstructor              (TxConstructor(..))

-------------------------------- On-chain ---------------------------------

{-# INLINABLE governanceDecisionTokenName #-}
governanceDecisionTokenName :: TxId -> TokenName
governanceDecisionTokenName tx = TokenName $ getTxId tx

checkPolicy :: () -> ScriptContext -> Bool
checkPolicy _ _ = True
    -- let info = scriptContextTxInfo ctx
    -- in governanceBeaconTokenRequired info

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
governanceDecisionTokenRequired info = utxoSpent info (\o -> txOutValue o `geq` governanceDecisionToken tx)
    where tx = txInfoId info

-------------------------- Off-Chain -----------------------------

-- TxConstraints that Governance Decision Token is spent in the transaction
governanceDecisionTokenTx :: TxId -> State (TxConstructor d a i o) (Maybe (TxOutRef, ChainIndexTxOut))
governanceDecisionTokenTx tx = utxoSpentPublicKeyTx (\_ o -> _ciTxOutValue o `geq` governanceDecisionToken tx)
    