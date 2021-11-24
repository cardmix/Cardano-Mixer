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
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Mixer (
    Mixer,
    MixerSchema,
    DepositParams(..),
    WithdrawParams(..),
    CollectParams(..),
    ClaimParams(..),
    verifierTokenAssetClass,
    mixerProgram
) where

import           Data.Aeson                       (FromJSON, ToJSON)
import           GHC.Generics                     (Generic)
import           Ledger                           (PubKeyHash)
import           Ledger.Typed.Tx                  (TypedScriptTxOut(tyTxOutData))
import           Ledger.Value                     (Value)
import           Plutus.Contract                  (Contract, Promise, type (.\/), Endpoint, endpoint, selectList, throwError, logInfo)
import           Plutus.Contract.StateMachine     (SMContractError(..), TransitionResult(..), OnChainState(..), getOnChainState, runStepWith)
import           PlutusTx.Prelude                 hiding (mempty, (<>))
import           Prelude                          (Show (..), Monoid (mempty), (<>))
import           Schema                           (ToSchema)

import           AdminKey
import           CheckKey                         (checkKeyTx)
import           Crypto
import           MixerFactory
import           MixerProofs
import           MixerStateMachine
import           Utility                          (last)

---------------------------------------------------------------------
--------------------------- Off-Chain -------------------------------
---------------------------------------------------------------------

-- Parameters for the "deposit" endpoint
data DepositParams = DepositParams
    {
        dpValue          :: !Value,
        dpLeaf           :: !Fr
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

-- "deposit" endpoint implementation
deposit :: Promise () MixerSchema SMContractError MixerState
deposit = endpoint @"deposit" @DepositParams $ \(DepositParams v leaf) -> do
    updateMixerState v (Deposit leaf)

-- Parameters for the "withdraw" endpoint
data WithdrawParams = WithdrawParams
    {
        wpValue         :: !Value,
        wpPKH           :: !PubKeyHash,
        wpKey           :: !Fr,
        wpLeaf'         :: !Fr,
        wpProof         :: !Proof
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- "withdraw" endpoint implementation
withdraw :: Promise () MixerSchema SMContractError MixerState
withdraw = endpoint @"withdraw" @WithdrawParams $ \(WithdrawParams v pkh key leaf proof) -> do
    s <- getMixerState v
    let pubParams = [one, zero, zero, zero, zero, last $ coPath s, dataToZp pkh, key, leaf, toZp $ depositCounter s]
    if verifyWithdraw pubParams proof
        then updateMixerState v (Withdraw pkh key leaf proof)
        else do
            logInfo $ ChooserError "Supplied proof is not correct"
            return s


-- Parameters for the "collect rewards" endpoint
data CollectParams = CollectParams
    {
        ctpValue          :: !Value,
        ctpPKH            :: !PubKeyHash,
        ctpKey            :: !Fr,
        ctpProof          :: !Proof,
        ctpOldHash        :: !Fr,
        ctpNewHash        :: !Fr
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- "collect rewards" endpoint implementation
collect :: Promise () MixerSchema SMContractError MixerState
collect = endpoint @"collect" @CollectParams $ \(CollectParams v pkh _ proof oh nh) -> do
    updateMixerState v (Withdraw pkh oh nh proof)

-- Parameters for the "claim rewards" endpoint
data ClaimParams = ClaimParams
    {
        clpValue          :: !Value,
        clpPKH            :: !PubKeyHash,
        clpClaimValue     :: !Value,
        clpOldHash        :: !Fr,
        clpNewHash        :: !Fr
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

-- "claim rewards" endpoint implementation
claim :: Promise () MixerSchema SMContractError MixerState
claim = endpoint @"claim" @ClaimParams $ \(ClaimParams v pkh _ oh nh) -> do
    updateMixerState v (Withdraw pkh oh nh (Proof O O O))

type MixerSchema = Endpoint "deposit" DepositParams .\/ Endpoint "withdraw" WithdrawParams
        .\/ Endpoint "collect" CollectParams .\/ Endpoint "claim" ClaimParams

mixerProgram :: Contract () MixerSchema SMContractError MixerState
mixerProgram = do
    selectList [deposit, withdraw, collect, claim] >> mixerProgram

------------------------------ Contract auxiliary --------------------------------

updateMixerState :: Value -> MixerInput -> Contract w MixerSchema SMContractError MixerState
updateMixerState v input = do
    mixer <- getMixerByValue v
    (lookups, cons) <- case input of
            Withdraw _ h _ _ -> do
                (lookups1, cons1) <- checkKeyTx (mixerTokenValue mixer) (withdrawToken mixer) h
                (lookups2, cons2) <- adminKeyTx -- TEMPORARY: one server implementation
                pure (lookups1 <> lookups2, cons1 <> cons2)
            _                -> pure (mempty, mempty)
    tResult <- runStepWith lookups cons (mixerClient mixer) input
    case tResult of
        TransitionFailure _  -> getMixerState v        -- returning the old state
        TransitionSuccess ns -> return ns              -- returning the new state

getMixerState :: Value -> Contract w MixerSchema SMContractError MixerState
getMixerState v = do
    mixer <- getMixerByValue v
    ocs <- getOnChainState (mixerClient mixer)
    case ocs of
        Nothing -> throwError $ ChooserError "Mixer not found"
        Just (OnChainState o _ _, _) -> return $ tyTxOutData o
    