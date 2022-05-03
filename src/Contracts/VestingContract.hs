{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-orphans               #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Contracts.VestingContract where

import           Control.Lens             (review)
import           Control.Monad            (void)
import qualified Data.Map
import           Ledger                   (Redeemer (..), StakePubKeyHash (..))
import           Ledger.Constraints       (mustBeSignedBy, mustValidateIn, unspentOutputs,
                                            otherScript, typedValidatorLookups, mustSpendScriptOutput, mustPayToPubKeyAddress)
import qualified Ledger.Interval          as Interval
import           Ledger.Scripts           (Datum(..))
import           Ledger.Tx                (ChainIndexTxOut(..))
import           Ledger.Value             (Value)
import           Prelude                  (Semigroup (..))
import           Plutus.Contract
import           Plutus.V1.Ledger.Ada     (lovelaceValueOf)
import           PlutusTx
import           PlutusTx.Prelude         hiding ((<>), Eq, Semigroup, fold, mempty)

import           Configuration.PABConfig  (pabWalletPKH, pabWalletSKH)
import           Scripts.VestingScript
import           Utils.Contracts          (selectUTXO)


retrieveFunds :: (AsVestingError e) => Contract w s e ()
retrieveFunds = mapError (review _VestingError) $ do
    pkh   <- ownPaymentPubKeyHash
    utxos <- utxosAt vestingScriptAddress
    ct    <- currentTime
    let (utxo1, utxos') = selectUTXO $ Data.Map.filter (\txout -> f txout (ct-100000) pkh) utxos
    if Data.Map.null utxos'
        then return ()
        else do
            let lookups = unspentOutputs utxos' <> typedValidatorLookups typedValidator <> otherScript vestingScript
                cons    = mustSpendScriptOutput utxo1 (Redeemer $ toBuiltinData ()) <> mustValidateIn (Interval.from (ct-100000)) <>
                    mustBeSignedBy pkh <> mustPayToPubKeyAddress pabWalletPKH (StakePubKeyHash pabWalletSKH) (lovelaceValueOf 1000_000_000)
                    -- TODO: add normal collateral check
            void $ submitTxConstraintsWith lookups cons
  where f o t h = case _ciTxOutDatum o of
          Left  _ -> False
          Right r -> let m = fromBuiltinData $ getDatum r :: Maybe VestingParams
                     in maybe False (\p -> vestingDate p <= t && vestingOwner p == h) m

retrieveFundsLoop :: (AsVestingError e, AsContractError e) => Contract w s e ()
retrieveFundsLoop = do
    retrieveFunds
    _ <- waitNSlots 30
    retrieveFundsLoop

type VestingSchema =
        Endpoint "vest-funds" (VestingParams, Value)
        .\/ Endpoint "retrieve-funds" ()

vestingContract :: Contract () VestingSchema VestingError ()
vestingContract = selectList [vest, retrieve]
  where
    vest = endpoint @"vest-funds" $ \(p, v) -> do
        (lookups, cons) <- timelockTx p v
        void $ submitTxConstraintsWith (lookups <> typedValidatorLookups typedValidator) cons
    retrieve = endpoint @"retrieve-funds" $ \() -> retrieveFunds