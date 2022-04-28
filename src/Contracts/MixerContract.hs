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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Contracts.MixerContract (
    DepositParams(..),
    WithdrawParams(..),
    MixerSchema,
    mixerProgram
) where

import           Cardano.Api                              (EraInMode (..), AsType (..), serialiseToCBOR, deserialiseFromCBOR)
import           Control.Monad                            (void)
import           Data.Aeson                               (FromJSON(..), ToJSON(..), Value (Object), (.:), encode)
import           Data.Aeson.Extras                        (encodeByteString, tryDecode)
import           Data.ByteString.Lazy                     (toStrict)
import           Data.Either                              (fromRight)
import qualified Data.Map
import           Data.Semigroup                           (Last (..))
import qualified Data.Set
import           Data.Text                                (pack, unpack, Text)
import           Data.Text.Encoding                       (decodeUtf8)
import           GHC.Generics                             (Generic)
import           Ledger                                   hiding (txFee, singleton, validatorHash, unspentOutputs)
import           Ledger.Constraints.OffChain              (unspentOutputs, typedValidatorLookups, otherScript, UnbalancedTx (unBalancedTxTx))
import           Ledger.Constraints.TxConstraints
import           Ledger.Value                             (geq)
import           Plutus.ChainIndex.Tx                     (ChainIndexTx (..), ChainIndexTxOutputs(..), fromOnChainTx)
import           Plutus.Contract                          (Promise, Endpoint, type (.\/), Contract,
                                                            endpoint, selectList, utxosAt, logInfo, mkTxConstraints,
                                                            submitTxConfirmed, currentTime, ownPaymentPubKeyHash,
                                                            tell, handleError, throwError, submitBalancedTx, balanceTx)
import           Plutus.Contract.CardanoAPI               (fromCardanoTx)
import           Plutus.Contract.Types                    (ContractError(..))
import           Plutus.V1.Ledger.Ada                     (lovelaceValueOf, toValue)
import           Plutus.V1.Ledger.Api                     (Credential(..), StakingCredential (..))
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (String, (<>), show, Show, (<$>))


import           Configuration.PABConfig                  (PABConfig (..), pabConfig, pabWalletPKH, pabWalletSKH)
import           Contracts.MixerKeysContract              (getMixerKeys)
import           Contracts.MixerStateContract             (MixerStateCache (..), getMixerState)
import           RelayRequest
import           Scripts.MixerScript
import           Scripts.VestingScript                    (VestingParams(..), vestingScriptHash)
import           Tokens.DepositToken                      (depositTokenMintTx)
import           Types.MixerContractTypes
import           Utils.Address                            (textToAddress, textToKeys)
import           Utils.Contracts                          (selectUTXO, balanceTxWithExternalWallet, txOutsFromRefs)


-- General MixerContract error
errorMixerContract :: ContractError -> Contract (Maybe (Last Text)) MixerSchema ContractError ()
errorMixerContract e = do
    let msg = case e of
          OtherContractError txt -> unpack txt
          _                      -> show e
    logInfo msg
    tell $ Just $ Last $ pack msg

-- "deposit" endpoint implementation
deposit :: Promise (Maybe (Last Text)) MixerSchema ContractError ()
deposit = endpoint @"deposit" @DepositParams $ \dp@(DepositParams txt v leaf) -> handleError errorMixerContract $ do
    logInfo @String "deposit"
    logInfo dp
    let mixer = makeMixerFromFees v
        -- value sent to the mixer script
        val   = mValue mixer + mTotalFees mixer + mixerAdaUTXO mixer
    ct <- currentTime
    let (lookups', cons') = depositTokenMintTx (mixerAddress mixer, v) (leaf, ct)
        -- total ADA value of all outputs
        val'              = val + toValue minAdaTxOut
        lookups           = typedValidatorLookups (mixerInst mixer) <> lookups'
        -- must send value to the mixer script and mint deposit token
        cons              = mustPayToTheScript () val <> cons'
    -- unbalanced transaction
    utx  <- mkTxConstraints lookups cons
    -- adding user wallet inputs and outputs
    let addr = fromMaybe (pubKeyHashAddress pabWalletPKH (Just $ StakePubKeyHash pabWalletSKH)) $ textToAddress txt
    logInfo @String "Prebalancing..."
    utx' <- balanceTxWithExternalWallet utx (addr, val') (map (lovelaceValueOf . (\i -> 1_100_000 + 20_000 * i)) [0..100])
    -- logInfo utx'
    -- final balancing with PAB wallet
    ctx <- case pabConfig of
            Simulator -> pure $ Right $ unBalancedTxTx utx'
            Testnet   -> balanceTx utx'
    txUnsigned <- case ctx of
        Left (SomeTx tx _) -> pure $ Data.Text.Encoding.decodeUtf8 $ toStrict $ encode $ TxUnsignedCW "1234567890" $ encodeByteString $ serialiseToCBOR tx
        Right tx           -> pure $ Data.Text.Encoding.decodeUtf8 $ toStrict $ encode tx
    tell $ Just $ Last txUnsigned

-- "deposit" endpoint implementation
depositSubmit :: Promise (Maybe (Last Text)) MixerSchema ContractError ()
depositSubmit = endpoint @"deposit-submit" @Text $ \txSigned -> handleError errorMixerContract $ do
    logInfo @String "deposit-submit"
    logInfo txSigned
    -- converting CBOR text into CardanoTx
    let txSignedByteString = fromRight (error ()) $ tryDecode txSigned
        ctx                = Left $ SomeTx (fromRight (error ()) $ deserialiseFromCBOR AsAlonzoTx txSignedByteString) AlonzoEraInCardanoMode :: CardanoTx
    -- computing PAB wallet reward
    let btx    = case ctx of
                Left (SomeTx eraInMode tx) -> fromRight (error ()) $ fromCardanoTx tx eraInMode
                Right tx                   -> fromOnChainTx $ Valid tx
        inRefs = map txInRef $ Data.Set.toList $ _citxInputs btx
        outs   = case _citxOutputs btx of
                    ValidTx a -> a
                    InvalidTx -> error ()
    logInfo btx
    ins <- txOutsFromRefs inRefs
    let inVal  = sum $ map txOutValue $ filter checkTxOutPKH ins
        outVal = sum $ map txOutValue $ filter checkTxOutPKH outs
    if outVal `geq` inVal
        then void $ submitBalancedTx ctx
    else throwError $ OtherContractError $ pack $ show $ outVal-inVal --"PAB fee for the deposit transaction is negative!"
  where
      checkTxOutPKH o = case txOutAddress o of
          Address (PubKeyCredential _) (Just (StakingHash (PubKeyCredential skh))) -> skh == pabWalletSKH
          _ -> False

timeToValidateWithdrawal :: POSIXTime
timeToValidateWithdrawal = POSIXTime 100_000

-- "withdraw" endpoint implementation
withdraw :: Promise (Maybe (Last Text)) MixerSchema ContractError ()
withdraw = endpoint @"withdraw" @WithdrawParams $ \params@(WithdrawParams v (_, _) txt subs _) -> handleError errorMixerContract $ do
    logInfo @String "withdraw"
    logInfo params
    let (pkhW, skhW) = fromMaybe (pabWalletPKH, StakePubKeyHash pabWalletSKH) $ textToKeys txt
        mixer = makeMixerFromFees v
    pkhR   <- ownPaymentPubKeyHash
    utxos  <- utxosAt (mixerAddress mixer)
    ct     <- currentTime
    -- TODO: fix empty list error
    let (utxo1, utxos'') = selectUTXO $ Data.Map.filter (\o -> _ciTxOutValue o `geq` (mValue mixer + mTotalFees mixer + mixerAdaUTXO mixer)) utxos

    logInfo @String "Querying MixerState..."
    (state, _) <- getMixerState (MixerStateCache [] 0) v
    mKeys <- getMixerKeys v
    case checkRelayRequest state mKeys params of
        RelayRequestAccepted -> do
                let lookups   = unspentOutputs utxos'' <> typedValidatorLookups (mixerInst mixer) <> otherScript (mixerValidator mixer)
                    cons      = mustPayToPubKeyAddress pkhW skhW (mValue mixer + mixerAdaUTXO mixer) <>
                        mustValidateIn (to $ ct + timeToValidateWithdrawal) <>
                        mustPayToOtherScript vestingScriptHash (Datum $ toBuiltinData $ VestingParams
                            (ct + hourPOSIX + 100000 + timeToValidateWithdrawal) pkhR utxo1 (subs !! 2)) (mRelayerCollateral mixer) <>
                        mustSpendScriptOutput utxo1 (Redeemer $ toBuiltinData ()) <> mustBeSignedBy pabWalletPKH -- TODO: remove the last constraint after the test
                utx <- mkTxConstraints lookups cons
                submitTxConfirmed utx
                tell $ Just $ Last "RelayRequestAccepted"
        e                    -> throwError $ OtherContractError $ pack $ show e

type MixerSchema = Endpoint "deposit" DepositParams  .\/ Endpoint "deposit-submit" Text .\/ Endpoint "withdraw" WithdrawParams

mixerProgram :: Contract (Maybe (Last Text)) MixerSchema ContractError ()
mixerProgram = selectList [deposit, withdraw, depositSubmit]

-----------------------------------------------------------------------------------

data TxUnsignedCW = TxUnsignedCW { passphrase :: String, transaction :: Text }
    deriving (Generic, FromJSON, ToJSON)

newtype TxSignedCW = TxSignedCW { unTxSignedCW :: Text }
    deriving Show

instance FromJSON TxSignedCW where
    parseJSON (Data.Aeson.Object v) = TxSignedCW <$> v .: "transaction"
    parseJSON _ = error ()
