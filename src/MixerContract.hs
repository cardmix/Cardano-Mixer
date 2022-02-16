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
import           Data.Text                                (pack)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Constraints.OffChain              (unspentOutputs, typedValidatorLookups, otherScript)
import           Ledger.Constraints.TxConstraints
import           Ledger.Value                             (geq)
import           Plutus.Contract                          (Promise, Endpoint, type (.\/), Contract,
                                                            endpoint, selectList, utxosAt, logInfo, mkTxConstraints,
                                                             submitTxConfirmed, currentTime, ownPaymentPubKeyHash,
                                                              tell, handleError, throwError)
import           Plutus.Contract.Types                    (ContractError(..))
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (String, (<>), show)

import           Contracts.Vesting                        (VestingParams(..), vestingScriptHash)
import           RelayRequest
import           MixerContractTypes
import           MixerKeysContract                        (getMixerKeys)
import           MixerScript
import           MixerStateContract                       (getMixerState)
import           Tokens.DepositToken                      (depositTokenMintTx)
import           Utils.Conversions                        (unbalancedTxToCBOR)
import           Utils.Contracts                          (selectUTXO)

-- General MixerContract deposit error
errorDeposit :: ContractError -> Contract (Maybe (Last String)) MixerSchema ContractError ()
errorDeposit = const $ do
    logInfo @String "Something went wrong during deposit!"
    tell $ Just $ Last ""

-- "deposit" endpoint implementation
deposit :: Promise (Maybe (Last String)) MixerSchema ContractError ()
deposit = endpoint @"deposit" @DepositParams $ \(DepositParams v leaf) -> handleError errorDeposit $ do
    let mixer = makeMixerFromFees v
        val   = mValue mixer + mTotalFees mixer
    ct <- currentTime
    let (lookups', cons') = depositTokenMintTx (mixerAddress mixer, val) (leaf, ct)
        lookups           = typedValidatorLookups (mixerInst mixer) <> lookups'
        cons              = mustPayToTheScript () val <> cons'
    utx <- mkTxConstraints lookups cons
    tell $ Just $ Last $ unbalancedTxToCBOR utx
    submitTxConfirmed utx

timeToValidateWithdrawal :: POSIXTime
timeToValidateWithdrawal = POSIXTime 100000

-- General MixerContract withdraw error
errorWithdraw :: ContractError -> Contract (Maybe (Last String)) MixerSchema ContractError ()
errorWithdraw e = do
    logInfo $ show e
    tell $ Just $ Last $ show e

-- "withdraw" endpoint implementation
withdraw :: Promise (Maybe (Last String)) MixerSchema ContractError ()
withdraw = endpoint @"withdraw" @WithdrawParams $ \params@(WithdrawParams v (_, _) pkhW subs _) -> handleError errorWithdraw $ do
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
                tell $ Just $ Last "RelayRequestAccepted"
        WrongRootValue         -> throwError $ OtherContractError $ pack $ show WrongRootValue
        WrongWithdrawalAddress -> throwError $ OtherContractError $ pack $ show WrongWithdrawalAddress
        WrongProof             -> throwError $ OtherContractError $ pack $ show WrongProof
        DuplicateKey           -> throwError $ OtherContractError $ pack $ show DuplicateKey

type MixerSchema = Endpoint "deposit" DepositParams .\/ Endpoint "withdraw" WithdrawParams

mixerProgram :: Contract (Maybe (Last String)) MixerSchema ContractError ()
mixerProgram = selectList [deposit, withdraw]

