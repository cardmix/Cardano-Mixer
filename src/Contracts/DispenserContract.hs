{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}

{-# LANGUAGE OverloadedStrings          #-}

{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Contracts.DispenserContract (
    DepositParams(..),
    WithdrawParams(..),
    MixerSchema,
    -- mixerProgram
) where

import           Control.Monad                            (void)
import qualified Data.Map
import           Data.Semigroup                           (Last (..))
import           Data.Text                                (pack, unpack)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Constraints.OffChain              (unspentOutputs, typedValidatorLookups, otherScript, UnbalancedTx (unBalancedTxTx))
import           Ledger.Constraints.TxConstraints
import           Ledger.Value                             (geq)
import           Plutus.Contract                          (Promise, Endpoint, type (.\/), Contract,
                                                            endpoint, selectList, utxosAt, logInfo, mkTxConstraints,
                                                            submitTxConfirmed, currentTime, ownPaymentPubKeyHash,
                                                            tell, handleError, throwError, submitBalancedTx)
import           Plutus.Contract.Types                    (ContractError(..))
import           Plutus.V1.Ledger.Ada                     (lovelaceValueOf, toValue)
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (String, (<>), show)

import           Contracts.MixerKeysContract              (getMixerKeys)
import           Contracts.MixerStateContract             (getMixerState)
import           RelayRequest
import           Scripts.MixerScript
import           Scripts.VestingScript                    (VestingParams(..), vestingScriptHash)
import           Tokens.DepositToken                      (depositTokenMintTx)
import           Types.MixerContractTypes
import           Utils.Conversions                        (unbalancedTxToCBOR)
import           Utils.Contracts                          (selectUTXO, balanceTxWithExternalWallet)
import Configuration.PABConfig (pabWalletPKH)

dispenserAddress :: Address
dispenserAddress = pubKeyHashAddress pabWalletPKH Nothing

-- "withdraw" endpoint implementation
sendTokens :: Promise (Maybe (Last String)) MixerSchema ContractError ()
sendTokens = endpoint @"Send tokens" @() $ \params -> do
    utxos  <- utxosAt dispenserAddress
    -- let lookups = mempty
    --     cons    = mustPayToPubKey pkh Value
    return ()

    -- ct     <- currentTime
    -- let (utxo1, utxos'') = selectUTXO $ Data.Map.filter (\o -> _ciTxOutValue o `geq` (mValue mixer + mTotalFees mixer)) utxos

    -- state <- getMixerState v
    -- mKeys <- getMixerKeys v
    -- case checkRelayRequest state mKeys params of
    --     RelayRequestAccepted -> do
    --             let lookups   = unspentOutputs utxos'' <> typedValidatorLookups (mixerInst mixer) <> otherScript (mixerValidator mixer)
    --                 cons      = mustPayToPubKey pkhW (mValue mixer) <> mustValidateIn (to $ ct + timeToValidateWithdrawal) <>
    --                     mustPayToOtherScript vestingScriptHash (Datum $ toBuiltinData $ VestingParams
    --                         (ct + hourPOSIX + 100000 + timeToValidateWithdrawal) pkhR pkhW utxo1 (sha2_256 emptyByteString) (subs !! 2)) (mRelayerCollateral mixer) <>
    --                     mustSpendScriptOutput utxo1 (Redeemer $ toBuiltinData ())
    --             utx <- mkTxConstraints lookups cons
    --             submitTxConfirmed utx
    --             tell $ Just $ Last "RelayRequestAccepted"
    --     e                    -> throwError $ OtherContractError $ pack $ show e

type MixerSchema = Endpoint "deposit" DepositParams .\/ Endpoint "withdraw" WithdrawParams

-- mixerProgram :: Contract (Maybe (Last String)) MixerSchema ContractError ()
-- mixerProgram = selectList [deposit, withdraw]

