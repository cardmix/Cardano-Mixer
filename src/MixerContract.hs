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

import qualified Data.Map
import           Data.Map                                 (keys, findMin, fromList)
import           Data.Semigroup                           (Last)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Constraints.OffChain              (unspentOutputs, typedValidatorLookups, otherScript)
import           Ledger.Constraints.TxConstraints
import           Ledger.Value                             (geq)
import           Plutus.Contract                          (Promise, ContractError, Endpoint, type (.\/), Contract,
                                                            endpoint, selectList, utxosAt, logInfo, mkTxConstraints, submitTxConfirmed, currentTime, ownPaymentPubKeyHash, txOutFromRef)
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (String, (<>), (<$>))

import           Contracts.Vesting                        (VestingParams(..), vestingScriptHash)
import           RelayRequest
import           MixerContractTypes
import           MixerScript
import           MixerState                               (MixerState)
import           MixerStateContract                       (getMixerState)

-- "deposit" endpoint implementation
deposit :: Promise (Maybe (Last MixerState)) MixerSchema ContractError ()
deposit = endpoint @"deposit" @DepositParams $ \(DepositParams v leaf) -> do
    curTime <- currentTime
    let mixer    = makeMixerFromFees v
        -- We create transaction that is valid during the next 10 minutes.
        -- This is needed to order deposits: the upper bound of the valRange is the time of deposit.
        valRange = interval curTime (curTime +  600000)
        lookups  = typedValidatorLookups $ mixerInst mixer
        cons     = mustPayToTheScript (MixerDatum leaf) (mValue mixer + mTotalFees mixer) <> mustValidateIn valRange
    utx <- mkTxConstraints lookups cons
    submitTxConfirmed utx


timeToValidateWithdrawal :: POSIXTime
timeToValidateWithdrawal = POSIXTime 100000

-- "withdraw" endpoint implementation
withdraw :: Promise (Maybe (Last MixerState)) MixerSchema ContractError ()
withdraw = endpoint @"withdraw" @WithdrawParams $ \params@(WithdrawParams v (nTree, nDeposit) pkhW subs proof) -> do
    let mixer = makeMixerFromFees v
    pkhR  <- ownPaymentPubKeyHash
    utxos <- utxosAt (mixerAddress mixer)
    ct    <- currentTime
    let utxo           = Data.Map.filter (\o -> _ciTxOutValue o `geq` (mValue mixer + mTotalFees mixer)) utxos
        utxo'          = fromList [findMin utxo]
        txo            = head $ keys  utxo
    dh <- (fromMaybe (error ()) <$> txOutDatumHash . toTxOut) . fromMaybe (error ()) <$> txOutFromRef txo

    -- TODO: Implement all necessary checks
    state <- getMixerState v
    case checkRelayRequest state params of
        RelayRequestAccepted -> do
                let lookups   = unspentOutputs utxo' <> typedValidatorLookups (mixerInst mixer) <> otherScript (mixerValidator mixer)
                    cons      = mustPayToPubKey pkhW (mValue mixer) <> mustValidateIn (to $ ct + timeToValidateWithdrawal) <>
                        mustPayToOtherScript vestingScriptHash (Datum $ toBuiltinData $ VestingParams
                            (ct + hourPOSIX + timeToValidateWithdrawal) pkhR dh) (mRelayerCollateral mixer) <>
                        mustSpendScriptOutput txo (Redeemer $ toBuiltinData $ MixerRedeemer pkhR (nTree, nDeposit) subs proof)
                utx <- mkTxConstraints lookups cons
                submitTxConfirmed utx
        WrongRootValue         -> logInfo @String "Wrong root value!"
        WrongWithdrawalAddress -> logInfo @String "Wrong withdrawal address!"
        WrongProof             -> logInfo @String "Wrong proof!"

type MixerSchema = Endpoint "deposit" DepositParams .\/ Endpoint "withdraw" WithdrawParams

mixerProgram :: Contract (Maybe (Last MixerState)) MixerSchema ContractError MixerDatum
mixerProgram = do
    selectList [deposit, withdraw] >> mixerProgram

