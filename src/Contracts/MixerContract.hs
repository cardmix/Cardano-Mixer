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
import           Data.Default                             (def)
import           Data.Either                              (fromRight)
import qualified Data.Map
import           Data.Semigroup                           (Last (..))
import qualified Data.Set
import           Data.Text                                (pack, unpack, Text)
import           Data.Text.Encoding                       (decodeUtf8)
import           GHC.Generics                             (Generic)
import           Ledger                                   hiding (txFee, singleton, validatorHash, unspentOutputs)
import           Ledger.Constraints.OffChain              (unspentOutputs, typedValidatorLookups, otherScript)
import           Ledger.Constraints.TxConstraints
import           Ledger.Value                             (geq)
import           Plutus.ChainIndex.Tx                     (ChainIndexTx (..), ChainIndexTxOutputs(..), fromOnChainTx)
import           Plutus.Contract                          (Promise, Endpoint, type (.\/), Contract,
                                                            endpoint, selectList, utxosAt, logInfo, mkTxConstraints,
                                                            submitTxConfirmed, currentTime, ownPaymentPubKeyHash,
                                                            tell, handleError, throwError, submitBalancedTx, balanceTx, utxoRefsAt)
import           Plutus.Contract.CardanoAPI               (fromCardanoTx)
import           Plutus.Contract.Types                    (ContractError(..))
import           Plutus.V1.Ledger.Ada                     (lovelaceValueOf, toValue)
import           Plutus.V1.Ledger.Api                     (Credential(..), StakingCredential (..))
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (String, (<>), show, Show, (<$>))


import           Configuration.PABConfig                  (pabWalletPKH, pabWalletSKH)
import           Contracts.MixerKeysContract              (getMixerKeys)
import           Contracts.MixerStateContract             (MixerStateCache (..), getMixerState)
import           MixerContractParams                      (DepositParams(..), WithdrawParams (..))
import           MixerProofs
import           RelayRequest
import           Scripts.MixerScript
import           Scripts.VestingScript                    (VestingParams(..), vestingScriptHash)
import           Tokens.DepositToken                      (depositTokenMintTx)
import           Utils.Address                            (bech32ToAddress, bech32ToKeyHashes)
import           Utils.ChainIndex                         (txOutsFromRefs)
import           Utils.BalanceTx                          (selectUTXO, balanceTxWithExternalWallet)

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
    let addr = fromMaybe (pubKeyHashAddress pabWalletPKH (Just $ StakePubKeyHash pabWalletSKH)) $ bech32ToAddress txt
    logInfo $ bech32ToAddress txt
    refs <- utxoRefsAt def $ fromMaybe (pubKeyHashAddress pabWalletPKH (Just $ StakePubKeyHash pabWalletSKH)) $ bech32ToAddress "addr_test1qrh8caw4kmlkwydwzdehpyw905dg8ayjv0vpe6vqmkkk5q3psddwydp9ea0gj3jawxyak3d238jpj9fxx3gnfhk7paxqnw2xmw"
    logInfo refs
    utxos <- utxosAt $ fromMaybe (pubKeyHashAddress pabWalletPKH (Just $ StakePubKeyHash pabWalletSKH)) $ bech32ToAddress "addr_test1qrh8caw4kmlkwydwzdehpyw905dg8ayjv0vpe6vqmkkk5q3psddwydp9ea0gj3jawxyak3d238jpj9fxx3gnfhk7paxqnw2xmw"
    logInfo utxos
    logInfo @String "Prebalancing..."
    utx' <- balanceTxWithExternalWallet utx (addr, val') (map (lovelaceValueOf . (\i -> 1_100_000 + 20_000 * i)) [0..100])
    -- logInfo utx'
    -- final balancing with PAB wallet
    ctx <- balanceTx utx'
    txUnsigned <- case ctx of
        CardanoApiTx (SomeTx tx _) -> pure $ Data.Text.Encoding.decodeUtf8 $ toStrict $ encode $ TxUnsignedCW "1234567890" $ encodeByteString $ serialiseToCBOR tx
        Both _ (SomeTx tx _)       -> pure $ Data.Text.Encoding.decodeUtf8 $ toStrict $ encode $ TxUnsignedCW "1234567890" $ encodeByteString $ serialiseToCBOR tx
        EmulatorTx tx              -> pure $ Data.Text.Encoding.decodeUtf8 $ toStrict $ encode tx
    tell $ Just $ Last txUnsigned

-- "deposit" endpoint implementation
depositSubmit :: Promise (Maybe (Last Text)) MixerSchema ContractError ()
depositSubmit = endpoint @"deposit-submit" @Text $ \txSigned -> handleError errorMixerContract $ do
    logInfo @String "deposit-submit"
    logInfo txSigned
    -- converting CBOR text into CardanoTx
    let txSignedByteString = fromRight (error ()) $ tryDecode txSigned
        ctx                = CardanoApiTx $ SomeTx (fromRight (error ()) $ deserialiseFromCBOR AsAlonzoTx txSignedByteString) AlonzoEraInCardanoMode :: CardanoTx
    -- computing PAB wallet reward
    let btx    = case ctx of
                CardanoApiTx (SomeTx eraInMode tx) -> fromRight (error ()) $ fromCardanoTx tx eraInMode
                Both _ (SomeTx eraInMode tx)       -> fromRight (error ()) $ fromCardanoTx tx eraInMode
                EmulatorTx tx                      -> fromOnChainTx $ Valid tx
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
timeToValidateWithdrawal = POSIXTime 500_000

-- "withdraw" endpoint implementation
withdraw :: Promise (Maybe (Last Text)) MixerSchema ContractError ()
withdraw = endpoint @"withdraw" @WithdrawParams $ \params@(WithdrawParams txt v _ subs _) -> handleError errorMixerContract $ do
    logInfo @String "withdraw"
    logInfo params
    let (pkhW, skhW) = fromMaybe (pabWalletPKH, StakePubKeyHash pabWalletSKH) $ bech32ToKeyHashes txt
        mixer = makeMixerFromFees v
    pkhR   <- ownPaymentPubKeyHash
    utxos  <- utxosAt (mixerAddress mixer)
    ct     <- currentTime
    -- TODO: fix empty list error
    let (utxo1, utxos'') = selectUTXO $ Data.Map.filter (\o -> _ciTxOutValue o `geq` (mValue mixer + mTotalFees mixer + mixerAdaUTXO mixer)) utxos

    logInfo @String "Querying MixerState..."
    (state, _) <- getMixerState (MixerStateCache [] 0) ct v
    mKeys <- getMixerKeys v
    case checkRelayRequest state mKeys params of
        RelayRequestAccepted -> do
                let lookups   = unspentOutputs utxos'' <> typedValidatorLookups (mixerInst mixer) <> otherScript (mixerValidator mixer)
                    cons      = mustPayToPubKeyAddress pkhW skhW (mValue mixer + mixerAdaUTXO mixer) <>
                        mustValidateIn (to $ ct + timeToValidateWithdrawal) <>
                        mustPayToOtherScript vestingScriptHash (Datum $ toBuiltinData $ VestingParams
                            (ct + hourPOSIX + 100000 + timeToValidateWithdrawal) pkhR utxo1 (getWithdrawKeyInput subs)) (mRelayerCollateral mixer) <>
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

