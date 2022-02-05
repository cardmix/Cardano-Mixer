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
{-# LANGUAGE NumericUnderscores #-}


module MixerContract (
    DepositParams(..),
    WithdrawParams(..),
    MixerSchema,
    mixerProgram
) where

import qualified Data.Map
import           Data.Map                                 (keys, difference)
import           Data.Semigroup                           (Last (..))
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Constraints.OffChain              (unspentOutputs, typedValidatorLookups, otherScript)
import           Ledger.Constraints.TxConstraints
import           Ledger.Value                             (geq)
import           Plutus.Contract                          (Promise, ContractError, Endpoint, type (.\/), Contract,
                                                            endpoint, selectList, utxosAt, logInfo, mkTxConstraints, submitTxConfirmed, currentTime, ownPaymentPubKeyHash, txOutFromRef, tell)
import           Plutus.V1.Ledger.Ada                     (lovelaceValueOf)
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (String, (<>), (<$>))

import           Contracts.Vesting                        (VestingParams(..), vestingScriptHash, VestingData (VestingData))
import           RelayRequest
import           MixerContractTypes
import           MixerKeysContract                        (getMixerKeys)
import           MixerScript
import           MixerStateContract                       (getMixerState)
import           Tokens.RelayTicketToken                  (relayTicketToken)
import           Utils.Conversions                        (unbalancedTxToCBOR)


-- "deposit" endpoint implementation
deposit :: Promise (Maybe (Last String)) MixerSchema ContractError ()
deposit = endpoint @"deposit" @DepositParams $ \(DepositParams v leaf) -> do
    curTime <- currentTime
    let mixer    = makeMixerFromFees v
        -- We create transaction that is valid during the next 10 minutes.
        -- This is needed to order deposits: the upper bound of the valRange is the time of deposit.
        valRange = interval curTime (curTime +  600000)
        lookups  = typedValidatorLookups $ mixerInst mixer
        cons     = mustPayToTheScript (MixerDatum leaf) (mValue mixer + mTotalFees mixer) <> mustValidateIn valRange
    utx <- mkTxConstraints lookups cons
    tell $ Just $ Last $ unbalancedTxToCBOR utx
    submitTxConfirmed utx


timeToValidateWithdrawal :: POSIXTime
timeToValidateWithdrawal = POSIXTime 100000

-- "withdraw" endpoint implementation
withdraw :: Promise (Maybe (Last String)) MixerSchema ContractError ()
withdraw = endpoint @"withdraw" @WithdrawParams $ \params@(WithdrawParams v (nTree, nDeposit) pkhW subs proof) -> do
    let mixer = makeMixerFromFees v
    pkhR   <- ownPaymentPubKeyHash
    utxos  <- utxosAt (mixerAddress mixer)
    utxos0 <- getRelayTicketUTXOs mixer
    ct     <- currentTime
    let utxos'         = difference utxos utxos0
        utxos''        = Data.Map.filter (\o -> _ciTxOutValue o `geq` (mValue mixer + mTotalFees mixer)) utxos'
        utxo1          = head $ keys utxos''
        utxo2          = head $ keys utxos0
    dh <- (fromMaybe (error ()) <$> txOutDatumHash . toTxOut) . fromMaybe (error ()) <$> txOutFromRef utxo1

    state <- getMixerState v
    mKeys <- getMixerKeys v
    case checkRelayRequest state mKeys params of
        RelayRequestAccepted -> do
                let lookups   = unspentOutputs utxos <> typedValidatorLookups (mixerInst mixer) <> otherScript (mixerValidator mixer)
                    cons      = mustPayToPubKey pkhW (mValue mixer) <> mustValidateIn (to $ ct + timeToValidateWithdrawal) <>
                        mustPayToOtherScript vestingScriptHash (Datum $ toBuiltinData $ VestingParams
                            (ct + hourPOSIX + timeToValidateWithdrawal) pkhR dh (VestingData pkhW (nTree, nDeposit) subs proof)) (mRelayerCollateral mixer) <>
                        mustSpendScriptOutput utxo1 (Redeemer $ toBuiltinData Withdraw) <>
                        mustSpendScriptOutput utxo2 (Redeemer $ toBuiltinData PayRelayTicket) <>
                        mustPayToTheScript (MixerDatum zero) (relayTicketToken + lovelaceValueOf 2_000_000)
                utx <- mkTxConstraints lookups cons
                submitTxConfirmed utx
        WrongRootValue         -> logInfo @String "Wrong root value!"
        WrongWithdrawalAddress -> logInfo @String "Wrong withdrawal address!"
        WrongProof             -> logInfo @String "Wrong proof!"
        DuplicateKey           -> logInfo @String "The key was already used!"

type MixerSchema = Endpoint "deposit" DepositParams .\/ Endpoint "withdraw" WithdrawParams

mixerProgram :: Contract (Maybe (Last String)) MixerSchema ContractError MixerDatum
mixerProgram =
    selectList [deposit, withdraw] >> mixerProgram

