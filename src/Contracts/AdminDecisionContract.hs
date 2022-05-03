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

module Contracts.AdminDecisionContract where

import           Control.Lens.Review              (review)
import           Control.Monad                    (void)
import           Data.Aeson                       (FromJSON, ToJSON)
import           GHC.Generics                     (Generic)
import           Ledger                           (getCardanoTxId)
import           Ledger.Address                   (Address (..), PaymentPubKeyHash (PaymentPubKeyHash))
import           Ledger.Constraints               (mintingPolicy, ScriptLookups)
import           Ledger.Constraints.TxConstraints (TxConstraints, mustPayToPubKey, mustPayToOtherScript, mustMintValue)
import           Ledger.Scripts                   (Datum(..))
import           Ledger.Typed.Scripts             (Any)
import           Ledger.Value                     (TokenName, assetClassValue)
import           Plutus.Contract
import           Plutus.V1.Ledger.Api             (Credential(..), toBuiltinData)
import           PlutusTx.Prelude                 hiding (Monoid (..), Semigroup (..), Eq)
import           Prelude                          (Semigroup (..), Show, Eq)

import           Contracts.CurrencyContract       (AsCurrencyError(..), CurrencyError)
import           Tokens.AdminDecisionToken
import           Tokens.AdminToken                (adminTokenTx)

-------------------------------- Off-chain --------------------------------

data AdminDecision = AdminDecision
    {
        decision    :: TokenName,
        addressBook :: [Address]
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

decide :: forall w s e a i o . AsCurrencyError e => TokenName -> [Address] -> Contract w s e (ScriptLookups a, TxConstraints i o)
decide tn addrBook = mapError (review _CurrencyError) $ do
    (lookups', cons') <- adminTokenTx
    let k        = length addrBook
        val      = assetClassValue (adminDecisionAssetClass tn) k
        lookups  = lookups' <> mintingPolicy curPolicy
        cons     = cons' <> mustMintValue val <> mconcat (map go addrBook)
    return (lookups, cons)
  where
      go :: Address -> TxConstraints i o
      go addr = case addr of
                    Address (PubKeyCredential pkh) _ -> mustPayToPubKey (PaymentPubKeyHash pkh) $ adminDecisionToken tn
                    Address (ScriptCredential vh)  _ -> mustPayToOtherScript vh (Datum $ toBuiltinData ()) $ adminDecisionToken tn

type AdminDecisionSchema = Endpoint "Make admin decision" AdminDecision

makeDecision :: Promise () AdminDecisionSchema CurrencyError ()
makeDecision = endpoint @"Make admin decision" $ \(AdminDecision d b) -> do
    (lookups, cons) <- decide d b
    tx <- submitTxConstraintsWith @Any lookups cons
    void $ awaitTxConfirmed (getCardanoTxId tx)