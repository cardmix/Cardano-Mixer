{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module Contracts.MixerContract (
    DepositParams(..),
    WithdrawParams(..),
    MixerSchema,
    mixerProgram
) where

import           Control.Monad                            (void)
import qualified Data.Map
import           Data.Semigroup                           (Last (..))
import           Data.Text                                (pack, unpack)
import           Ledger                                   hiding (txFee, singleton, validatorHash, unspentOutputs)
import           Ledger.CardanoWallet                     (paymentPrivateKey, knownMockWallets)
import           Ledger.Constraints.OffChain              (unspentOutputs, typedValidatorLookups, otherScript, UnbalancedTx (unBalancedTxTx))
import           Ledger.Constraints.TxConstraints
import           Ledger.Value                             (geq)
import           Plutus.Contract                          (Promise, Endpoint, type (.\/), Contract,
                                                            endpoint, selectList, utxosAt, logInfo, mkTxConstraints,
                                                            submitTxConfirmed, currentTime, ownPaymentPubKeyHash,
                                                            tell, handleError, throwError, submitBalancedTx, balanceTx)
import           Plutus.Contract.Types                    (ContractError(..))
import           Plutus.V1.Ledger.Ada                     (lovelaceValueOf, toValue)
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (String, (<>), show)


import           Configuration.PABConfig                  (inSimulation)
import           Contracts.MixerKeysContract              (getMixerKeys)
import           Contracts.MixerStateContract             (getMixerState)
import           RelayRequest
import           Scripts.MixerScript
import           Scripts.VestingScript                    (VestingParams(..), vestingScriptHash)
import           Tokens.DepositToken                      (depositTokenMintTx)
import           Types.MixerContractTypes
import           Utils.Contracts                          (selectUTXO, balanceTxWithExternalWallet)


-- General MixerContract error
errorMixerContract :: ContractError -> Contract (Maybe (Last CardanoTx)) MixerSchema ContractError ()
errorMixerContract e = do
    let msg = case e of
          OtherContractError txt -> unpack txt
          _                      -> show e
    logInfo @String msg
    -- tell $ Just $ Last msg

-- "deposit" endpoint implementation
deposit :: Promise (Maybe (Last CardanoTx)) MixerSchema ContractError ()
deposit = endpoint @"deposit" @DepositParams $ \(DepositParams pkh v leaf) -> handleError errorMixerContract $ do
    let mixer = makeMixerFromFees v
        -- value sent to the mixer script
        val   = mValue mixer + mTotalFees mixer
    ct <- currentTime
    let (lookups', cons') = depositTokenMintTx (mixerAddress mixer, val) (leaf, ct)
        -- total ADA value of all outputs
        val'              = val + toValue minAdaTxOut
        lookups           = typedValidatorLookups (mixerInst mixer) <> lookups'
        -- must send value to the mixer script and mint deposit token
        cons              = mustPayToTheScript () val <> cons'
    logInfo ct
    logInfo cons
    -- unbalanced transaction
    utx  <- mkTxConstraints lookups cons
    -- adding user wallet inputs and outputs
    utx' <- balanceTxWithExternalWallet utx (pkh, val') (map (lovelaceValueOf . (\i -> 1_000_000 + 10_000 * i)) [0..100])
    let tx    = unBalancedTxTx utx
        utx'' = utx' { unBalancedTxTx = if inSimulation then addSignature' (unPaymentPrivateKey $ paymentPrivateKey $ knownMockWallets !! 2) tx else tx }
    -- final balancing with PAB wallet
    ctx <- balanceTx utx''
    tell $ Just $ Last ctx

-- "deposit" endpoint implementation
depositSubmit :: Promise (Maybe (Last CardanoTx)) MixerSchema ContractError ()
depositSubmit = endpoint @"depositSubmit" @CardanoTx $ \ctx -> handleError errorMixerContract $ do
    void $ submitBalancedTx ctx

timeToValidateWithdrawal :: POSIXTime
timeToValidateWithdrawal = POSIXTime 100_000

-- "withdraw" endpoint implementation
withdraw :: Promise (Maybe (Last CardanoTx)) MixerSchema ContractError ()
withdraw = endpoint @"withdraw" @WithdrawParams $ \params@(WithdrawParams v (_, _) pkhW subs _) -> handleError errorMixerContract $ do
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
                -- tell $ Just $ Last "RelayRequestAccepted"
        e                    -> throwError $ OtherContractError $ pack $ show e

type MixerSchema = Endpoint "deposit" DepositParams  .\/ Endpoint "depositSubmit" CardanoTx .\/ Endpoint "withdraw" WithdrawParams

mixerProgram :: Contract (Maybe (Last CardanoTx)) MixerSchema ContractError ()
mixerProgram = selectList [deposit, withdraw, depositSubmit]

