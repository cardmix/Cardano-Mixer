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
                                                            endpoint, selectList, utxosAt, logInfo, mkTxConstraints, submitTxConfirmed, currentTime)
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (Show (..), String, (<>), last)
import           Schema                                   (ToSchema)


import           Crypto
import           MixerProofs                              (verifyWithdraw)
import           MixerScript
import           MixerState                               (MixerState, getMixerState)



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
        wpPKH           :: !PaymentPubKeyHash,
        wpKey           :: !Fr,
        wpKeyA          :: !Fr,
        wpOldHash       :: !Fr,
        wpNewHash       :: !Fr,
        wpProof         :: !Proof
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- "withdraw" endpoint implementation
withdraw :: Promise (Maybe (Last MixerState)) MixerSchema ContractError ()
withdraw = endpoint @"withdraw" @WithdrawParams $ \(WithdrawParams v pkh key keyA oh nh proof) -> do
    let mixer = Mixer v v
    utxos <- utxosAt (mixerAddress mixer)
    let utxo           = Data.Map.filter (\o -> _ciTxOutValue o `geq` mValue mixer) utxos
        utxo'          = fromList [findMin utxo]
        txo            = head $ keys  utxo
    
    state <- getMixerState v
    let coPath = getMerkleCoPath (head state) 1
    let root = last coPath
        pubParams = [one, zero, zero, zero, zero, zero, root, dataToZp pkh, key, keyA, toZp 1 :: Fr, oh, nh]
    let lookups   = unspentOutputs utxo' <> typedValidatorLookups (mixerInst mixer) <> otherScript (mixerValidator mixer)
        cons      = mustPayToPubKey pkh v <> mustSpendScriptOutput txo (Redeemer $ toBuiltinData $ MixerRedeemer pkh key proof)
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

