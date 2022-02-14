{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module MixerContract (
    DepositParams(..),
    WithdrawParams(..),
    MixerSchema,
    mixerProgram
) where

import qualified Data.Map
import           Data.Semigroup                           (Last (..))
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Constraints.OffChain              (unspentOutputs, typedValidatorLookups, otherScript)
import           Ledger.Constraints.TxConstraints
import           Ledger.Value                             (geq)
import           Plutus.Contract                          (Promise, ContractError, Endpoint, type (.\/), Contract,
                                                            endpoint, selectList, utxosAt, logInfo, mkTxConstraints, submitTxConfirmed, currentTime, ownPaymentPubKeyHash, tell)
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (String, (<>))

import           Contracts.Vesting                        (VestingParams(..), vestingScriptHash)
import           RelayRequest
import           MixerContractTypes
import           MixerKeysContract                        (getMixerKeys)
import           MixerScript
import           MixerStateContract                       (getMixerState)
import           Tokens.DepositToken                      (depositTokenMintTx)
import           Utils.Conversions                        (unbalancedTxToCBOR)
import           Utils.Contracts (selectUTXO)



-- "deposit" endpoint implementation
deposit :: Promise (Maybe (Last String)) MixerSchema ContractError ()
deposit = endpoint @"deposit" @DepositParams $ \(DepositParams v leaf) -> do
    let mixer = makeMixerFromFees v
        val   = mValue mixer + mTotalFees mixer
    ct <- currentTime
    (lookups', cons') <- depositTokenMintTx (mixerAddress mixer, val) (leaf, ct)
    let lookups  = typedValidatorLookups (mixerInst mixer) <> lookups'
        cons     = mustPayToTheScript () val <> cons'
    utx <- mkTxConstraints lookups cons
    tell $ Just $ Last $ unbalancedTxToCBOR utx
    submitTxConfirmed utx

timeToValidateWithdrawal :: POSIXTime
timeToValidateWithdrawal = POSIXTime 100000

-- "withdraw" endpoint implementation
withdraw :: Promise (Maybe (Last String)) MixerSchema ContractError ()
withdraw = endpoint @"withdraw" @WithdrawParams $ \params@(WithdrawParams v (_, _) pkhW subs _) -> do
    let mixer = makeMixerFromFees v
    pkhR   <- ownPaymentPubKeyHash
    utxos  <- utxosAt (mixerAddress mixer)
    ct     <- currentTime
    let (utxo1, utxos'') = selectUTXO $ Data.Map.filter (\o -> _ciTxOutValue o `geq` (mValue mixer + mTotalFees mixer)) utxos

    state <- getMixerState v
    mKeys <- getMixerKeys v
    case checkRelayRequest state mKeys params of
        RelayRequestAccepted -> do
                let lookups   = unspentOutputs utxos'' <> typedValidatorLookups (mixerInst mixer) <> otherScript (mixerValidator mixer)
                    cons      = mustPayToPubKey pkhW (mValue mixer) <> mustValidateIn (to $ ct + timeToValidateWithdrawal) <>
                        mustPayToOtherScript vestingScriptHash (Datum $ toBuiltinData $ VestingParams
                            (ct + hourPOSIX + 100000 + timeToValidateWithdrawal) pkhR pkhW utxo1 (sha2_256 emptyByteString) (subs !! 2)) (mRelayerCollateral mixer) <>
                        mustSpendScriptOutput utxo1 (Redeemer $ toBuiltinData ())
                utx <- mkTxConstraints lookups cons
                submitTxConfirmed utx
        WrongRootValue         -> logInfo @String "Wrong root value!"
        WrongWithdrawalAddress -> logInfo @String "Wrong withdrawal address!"
        WrongProof             -> logInfo @String "Wrong proof!"
        DuplicateKey           -> logInfo @String "The key was already used!"

type MixerSchema = Endpoint "deposit" DepositParams .\/ Endpoint "withdraw" WithdrawParams

mixerProgram :: Contract (Maybe (Last String)) MixerSchema ContractError ()
mixerProgram =
    selectList [deposit, withdraw] >> mixerProgram

