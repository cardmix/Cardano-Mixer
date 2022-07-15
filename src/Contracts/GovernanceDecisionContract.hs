{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Contracts.GovernanceDecisionContract where

import           Control.Lens.Review              (review)
import           Control.Monad                    (void)
import           Data.Aeson                       (FromJSON, ToJSON)
import qualified Data.Map
import           GHC.Generics                     (Generic)
import           Ledger                           (TxId, getCardanoTxId)
import           Ledger.Address                   (Address (..), PaymentPubKeyHash (PaymentPubKeyHash))
import           Ledger.Constraints               (mintingPolicy, ScriptLookups, unspentOutputs)
import           Ledger.Constraints.TxConstraints (TxConstraints, mustPayToPubKey, mustPayToOtherScript, mustMintValue)
import           Ledger.Scripts                   (Datum(..))
import           Ledger.Typed.Scripts             (Any)
import           Ledger.Value                     (assetClassValue)
import           Plutus.Contract
import           Plutus.V1.Ledger.Api             (Credential(..), toBuiltinData)
import           PlutusTx.Prelude                 hiding (Monoid (..), Semigroup (..), Eq)
import           Prelude                          (Semigroup (..), Show, Eq)

import           Contracts.CurrencyContract       (AsCurrencyError(..), CurrencyError)
import           Tokens.GovernanceDecisionToken

-- TODO: The code in this module is outdated.

-------------------------------- Off-chain --------------------------------

-- data GovernanceDecision = GovernanceDecision
--     {
--         decision    :: TxId,
--         addressBook :: [Address]
--     }
--     deriving stock (Eq, Generic, Show)
--     deriving anyclass (FromJSON, ToJSON)

-- decide :: forall w s e a i o . AsCurrencyError e => TxId -> [Address] -> Contract w s e (ScriptLookups a, TxConstraints i o)
-- decide tx addrBook = mapError (review _CurrencyError) $ do
--     utxos <- Data.Map.map fst <$> getUtxosWithCurrency (governanceDecisionAssetClass tx)
--     let k        = length addrBook
--         val      = assetClassValue (governanceDecisionAssetClass tx) k
--         (lookups, cons) = governanceDecisionTokenTx tx
--         lookups'  = lookups <> unspentOutputs utxos <> mintingPolicy curPolicy
--         cons'     = cons <> mustMintValue val <> mconcat (map go addrBook)
--     return (lookups', cons')
--   where
--       go :: Address -> TxConstraints i o
--       go addr = case addr of
--                     Address (PubKeyCredential pkh) _ -> mustPayToPubKey (PaymentPubKeyHash pkh) $ governanceDecisionToken tx
--                     Address (ScriptCredential vh)  _ -> mustPayToOtherScript vh (Datum $ toBuiltinData ()) $ governanceDecisionToken tx

-- type GovernanceDecisionSchema = Endpoint "mint-governance-decision" GovernanceDecision

-- makeDecision :: Promise () GovernanceDecisionSchema CurrencyError ()
-- makeDecision = endpoint @"mint-governance-decision" $ \(GovernanceDecision d b) -> do
--     (lookups, cons) <- decide d b
--     tx <- submitTxConstraintsWith @Any lookups cons
--     void $ awaitTxConfirmed (getCardanoTxId tx)