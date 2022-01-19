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
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module MixerContract (
    DepositParams(..),
    WithdrawParams(..),
    MixerSchema,
    mixerProgram
) where

import           Data.Aeson                               (FromJSON, ToJSON)
import qualified Data.Map
import           Data.Map                                 (keys, findMin, fromList)
import           Data.Semigroup                           (Last)
import           GHC.Generics                             (Generic)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Constraints.OffChain              (unspentOutputs, typedValidatorLookups, otherScript)
import           Ledger.Constraints.TxConstraints
import           Ledger.Value                             (geq)
import           Plutus.Contract                          (Promise, ContractError, Endpoint, type (.\/), Contract,
                                                            endpoint, selectList, utxosAt, logInfo, mkTxConstraints, submitTxConfirmed, currentTime, ownPaymentPubKeyHash, txOutFromRef)
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (Show (..), String, (<>), (<$>))
import           Schema                                   (ToSchema)


import           Contracts.Vesting                        (VestingParams(..), vestingScriptHash)
import           Crypto
import           MixerProofs                              (verifyWithdraw)
import           MixerScript
import           MixerState                               (MixerState)



-- Parameters for the "deposit" endpoint
data DepositParams = DepositParams
    {
        dpValue          :: !Value,
        dpLeaf           :: !Fr
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

-- "deposit" endpoint implementation
deposit :: Promise (Maybe (Last MixerState)) MixerSchema ContractError ()
deposit = endpoint @"deposit" @DepositParams $ \(DepositParams v leaf) -> do
    curTime <- currentTime
    let mixer    = Mixer v v
        -- We create transaction that is valid during the next 10 minutes.
        -- This is needed to order deposits: the upper bound of the valRange is the time of deposit.
        valRange = interval curTime (curTime +  600000)
        lookups  = typedValidatorLookups $ mixerInst mixer
        cons     = mustPayToTheScript (MixerDatum leaf) v <> mustValidateIn valRange
    utx <- mkTxConstraints lookups cons
    submitTxConfirmed utx

-- Parameters for the "withdraw" endpoint
data WithdrawParams = WithdrawParams
    {
        wpValue         :: !Value,
        wpDepositNum    :: !(Integer, Integer),
        wpPKH           :: !PaymentPubKeyHash,
        wpPublicInputs  :: ![Fr],
        wpProof         :: !Proof
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- "withdraw" endpoint implementation
withdraw :: Promise (Maybe (Last MixerState)) MixerSchema ContractError ()
withdraw = endpoint @"withdraw" @WithdrawParams $ \(WithdrawParams v (nTree, nDeposit) pkhW subs proof) -> do
    let mixer = Mixer v v
    pkhR  <- ownPaymentPubKeyHash
    utxos <- utxosAt (mixerAddress mixer)
    ct    <- currentTime
    let utxo           = Data.Map.filter (\o -> _ciTxOutValue o `geq` mValue mixer) utxos
        utxo'          = fromList [findMin utxo]
        txo            = head $ keys  utxo
    dh <- (fromMaybe (error ()) <$> txOutDatumHash . toTxOut) . fromMaybe (error ()) <$> txOutFromRef txo

    -- TODO: Implement all necessary checks
    -- state <- getMixerState v
    -- let Just (MerkleTree _ leafs) = getMerkleTree state (nTree, nDeposit)
    let pubParams = [one, zero, zero, zero, zero, zero] ++ subs
    let lookups   = unspentOutputs utxo' <> typedValidatorLookups (mixerInst mixer) <> otherScript (mixerValidator mixer)
        cons      = mustPayToPubKey pkhW v <> mustValidateIn (to $ ct + POSIXTime 100000) <>
            mustPayToOtherScript vestingScriptHash (Datum $ toBuiltinData $ VestingParams
                (ct + POSIXTime 3700000) pkhR dh) (mRelayerCollateral mixer) <>
            mustSpendScriptOutput txo (Redeemer $ toBuiltinData $ MixerRedeemer pkhR (nTree, nDeposit) subs proof)
    if verifyWithdraw pubParams proof
        then do
            utx <- mkTxConstraints lookups cons
            submitTxConfirmed utx
        else do
            logInfo @String "Supplied proof is not correct"

type MixerSchema = Endpoint "deposit" DepositParams .\/ Endpoint "withdraw" WithdrawParams

mixerProgram :: Contract (Maybe (Last MixerState)) MixerSchema ContractError MixerDatum
mixerProgram = do
    selectList [deposit, withdraw] >> mixerProgram

