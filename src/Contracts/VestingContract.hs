{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module Contracts.VestingContract where

import           Cardano.Api              (FromJSON, ToJSON)
import           Control.Lens             (makeClassyPrisms, review)
import           Control.Monad            (void)
import           Data.Map                 (minViewWithKey)
import qualified Data.Map
import           Data.Maybe               (fromJust)
import           GHC.Generics             (Generic)
import           Ledger                   (Redeemer (..), StakePubKeyHash (..))
import           Ledger.Constraints       (ScriptLookups, TxConstraints, mustBeSignedBy, mustValidateIn, unspentOutputs,
                                            otherScript, typedValidatorLookups, mustSpendScriptOutput, mustPayToPubKeyAddress, mustPayToOtherScript)
import qualified Ledger.Interval          as Interval
import           Ledger.Scripts           (Datum(..))
import           Ledger.Tx                (ChainIndexTxOut(..))
import           Ledger.Value             (Value)
import           Prelude                  (Semigroup (..), Eq, Show)
import           Plutus.Contract
import           Plutus.V1.Ledger.Ada     (lovelaceValueOf)
import           PlutusTx
import           PlutusTx.Prelude         hiding ((<>), Eq, Semigroup, fold, mempty)

import           Configuration.PABConfig  (pabWalletPKH, pabWalletSKH)
import           Scripts.VestingScript


data VestingError =
    VContractError ContractError
    | InsufficientFundsError Value Value Value
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''VestingError

instance AsContractError VestingError where
    _ContractError = _VContractError

timelockTx :: (AsVestingError e) => VestingDatum -> Value -> Contract w s e (ScriptLookups a, TxConstraints i o)
timelockTx p v = mapError (review _VestingError) $ do
    utxos <- utxosAt vestingValidatorAddress
    let lookups = otherScript vestingValidator <> unspentOutputs utxos
        cons    = mustPayToOtherScript vestingValidatorHash (Datum $ toBuiltinData p) v
    return (lookups, cons)

retrieveFunds :: (AsVestingError e) => Contract w s e ()
retrieveFunds = mapError (review _VestingError) $ do
    pkh   <- ownPaymentPubKeyHash
    utxos <- utxosAt vestingValidatorAddress
    ct    <- currentTime
    let utxos' = Data.Map.filter (\txout -> f txout (ct-100000) pkh) utxos
    if Data.Map.null utxos'
        then return ()
        else do
            let ((utxo1, _), utxos'') = fromJust $ minViewWithKey utxos'
                lookups = unspentOutputs utxos'' <> typedValidatorLookups vestingTypedValidator <> otherScript vestingValidator
                cons    = mustSpendScriptOutput utxo1 (Redeemer $ toBuiltinData ()) <> mustValidateIn (Interval.from (ct-100000)) <>
                    mustBeSignedBy pkh <> mustPayToPubKeyAddress pabWalletPKH (StakePubKeyHash pabWalletSKH) (lovelaceValueOf 1000_000_000)
                    -- TODO: add normal collateral check
            void $ submitTxConstraintsWith lookups cons
  where f o t h = case _ciTxOutDatum o of
          Left  _ -> False
          Right r -> let m = fromBuiltinData $ getDatum r :: Maybe VestingDatum
                     in maybe False (\(d, oh) -> d <= t && oh == h) m

retrieveFundsLoop :: (AsVestingError e, AsContractError e) => Contract w s e ()
retrieveFundsLoop = do
    retrieveFunds
    _ <- waitNSlots 30
    retrieveFundsLoop

type VestingSchema =
        Endpoint "vest-funds" (VestingDatum, Value)
        .\/ Endpoint "retrieve-funds" ()

vestingContract :: Contract () VestingSchema VestingError ()
vestingContract = selectList [vest, retrieve]
  where
    vest = endpoint @"vest-funds" $ \(p, v) -> do
        (lookups, cons) <- timelockTx p v
        void $ submitTxConstraintsWith (lookups <> typedValidatorLookups vestingTypedValidator) cons
    retrieve = endpoint @"retrieve-funds" $ \() -> retrieveFunds

    