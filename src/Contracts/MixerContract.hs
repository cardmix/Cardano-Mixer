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
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (String, (<>), show, Show, (<$>))


import           Configuration.PABConfig                  (PABConfig (..), pabConfig, pabWalletPKH)
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
    logInfo @String msg
    -- tell $ Just $ Last msg

-- "deposit" endpoint implementation
deposit :: Promise (Maybe (Last Text)) MixerSchema ContractError ()
deposit = endpoint @"deposit" @DepositParams $ \dp@(DepositParams txt v leaf) -> handleError errorMixerContract $ do
    logInfo @String "deposit"
    logInfo dp
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
    -- unbalanced transaction
    utx  <- mkTxConstraints lookups cons
    -- adding user wallet inputs and outputs
    let addr = textToAddress txt
    utx' <- balanceTxWithExternalWallet utx (addr, val') (map (lovelaceValueOf . (\i -> 1_000_000 + 10_000 * i)) [0..100])
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
depositSubmit = endpoint @"depositSubmit" @Text $ \txSigned -> handleError errorMixerContract $ do
    logInfo @String "depositSubmit"
    logInfo txSigned
    -- converting CBOR text into CardanoTx
    let 
        -- txSigned           = unTxSignedCW $ fromMaybe (error ()) (decode $ fromStrict $ Data.Text.Encoding.encodeUtf8 bs :: Maybe TxSignedCW)
        -- txSignedByteString = fromRight (error ()) $ Data.ByteString.Base64.decodeBase64 $ Data.Text.Encoding.encodeUtf8 txSigned
        -- txSignedByteString = fromMaybe (error ()) $ decode $ fromStrict $ Data.Text.Encoding.encodeUtf8 txSigned
        txSignedByteString = fromRight (error ()) $ tryDecode txSigned
        ctx                = Left $ SomeTx (fromRight (error ()) $ deserialiseFromCBOR AsAlonzoTx txSignedByteString) AlonzoEraInCardanoMode :: CardanoTx
    -- computing PAB wallet reward
    let btx    = case ctx of
                Left (SomeTx eraInMode tx) -> fromRight (error ()) $ fromCardanoTx tx eraInMode
                Right tx                   -> fromOnChainTx $ Valid tx
        inRefs = map txInRef $ Data.Set.toList $ _citxInputs btx
        outs   = case _citxOutputs btx of
                    ValidTx a -> a
                    InvalidTx -> error ()
    ins <- txOutsFromRefs inRefs
    let inVal  = sum $ map txOutValue $ filter (\o -> txOutAddress o == pubKeyHashAddress pabWalletPKH Nothing) ins
        outVal = sum $ map txOutValue $ filter (\o -> txOutAddress o == pubKeyHashAddress pabWalletPKH Nothing) outs
    if outVal `geq` inVal
        then void $ submitBalancedTx ctx
    else throwError $ OtherContractError $ pack $ show $ outVal-inVal --"PAB fee for the deposit transaction is negative!"

timeToValidateWithdrawal :: POSIXTime
timeToValidateWithdrawal = POSIXTime 100_000

-- "withdraw" endpoint implementation
withdraw :: Promise (Maybe (Last Text)) MixerSchema ContractError ()
withdraw = endpoint @"withdraw" @WithdrawParams $ \params@(WithdrawParams v (_, _) txt subs _) -> handleError errorMixerContract $ do
    logInfo @String "withdraw"
    logInfo params
    let (pkhW, skhW) = textToKeys txt
        addr = pubKeyHashAddress pkhW (Just skhW)
        mixer = makeMixerFromFees v
    pkhR   <- ownPaymentPubKeyHash
    utxos  <- utxosAt (mixerAddress mixer)
    ct     <- currentTime
    -- TODO: fix empty list error
    let (utxo1, utxos'') = selectUTXO $ Data.Map.filter (\o -> _ciTxOutValue o `geq` (mValue mixer + mTotalFees mixer)) utxos

    (state, _) <- getMixerState (MixerStateCache [] 0) v
    mKeys <- getMixerKeys v
    case checkRelayRequest state mKeys params of
        RelayRequestAccepted -> do
                let lookups   = unspentOutputs utxos'' <> typedValidatorLookups (mixerInst mixer) <> otherScript (mixerValidator mixer)
                    cons      = mustPayToPubKeyAddress pkhW skhW (mValue mixer) <> mustValidateIn (to $ ct + timeToValidateWithdrawal) <>
                        mustPayToOtherScript vestingScriptHash (Datum $ toBuiltinData $ VestingParams
                            (ct + hourPOSIX + 100000 + timeToValidateWithdrawal) pkhR addr utxo1 (sha2_256 emptyByteString) (subs !! 2)) (mRelayerCollateral mixer) <>
                        mustSpendScriptOutput utxo1 (Redeemer $ toBuiltinData ())
                utx <- mkTxConstraints lookups cons
                submitTxConfirmed utx
                tell $ Just $ Last "RelayRequestAccepted"
        e                    -> throwError $ OtherContractError $ pack $ show e

type MixerSchema = Endpoint "deposit" DepositParams  .\/ Endpoint "depositSubmit" Text .\/ Endpoint "withdraw" WithdrawParams

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
