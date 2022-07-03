{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
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
    -- MixerSchema,
    -- mixerProgram
) where

import           Cardano.Api                              (EraInMode (..), AsType (..), serialiseToCBOR, deserialiseFromCBOR)
import           Control.Monad                            (void)
import           Data.Aeson                               (FromJSON(..), ToJSON(..), Value (Object), (.:), encode)
import           Data.Aeson.Extras                        (encodeByteString, tryDecode)
import           Data.ByteString.Lazy                     (toStrict)
import           Data.Default                             (def)
import           Data.Either                              (fromRight)
import           Data.Map                                 (minViewWithKey)
import qualified Data.Map
import           Data.Maybe                               (fromJust)
import           Data.Semigroup                           (Last (..))
import qualified Data.Set
import           Data.Text                                (pack, unpack, Text)
import           Data.Text.Encoding                       (decodeUtf8)
import           GHC.Generics                             (Generic)
import           Ledger                                   hiding (txFee, singleton, validatorHash, unspentOutputs)
import           Ledger.Constraints.OffChain              (unspentOutputs, typedValidatorLookups, otherScript)
import           Ledger.Constraints.TxConstraints
import           Ledger.Value                             (geq, TokenName (..))
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
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check, null)
import           Prelude                                  (String, (<>), show, Show, (<$>), null, Monoid (mempty))


import           Configuration.PABConfig                  (pabWalletPKH, pabWalletSKH)
-- import           Contracts.MixerKeysContract              (getMixerKeys)
-- import           Contracts.MixerStateContract             (MixerStateCache (..), getMixerState)
import           MixerContractParams                      (DepositParams(..), WithdrawParams (..))
import           MixerProofs
import           Scripts.MixerScript
-- import           Scripts.VestingScript                    (VestingParams(..), vestingScriptHash)
import           Tokens.DepositToken                      (depositTokenMintTx)
import           Utils.Address                            (bech32ToAddress, bech32ToKeyHashes)
import           Contracts.ChainIndex                         (txOutsFromRefs)
import           Utils.BalanceTx                          (balanceTxWithExternalWallet)
import Scripts.VestingScript (vestingValidatorHash)
import Scripts.FailScript (failAddress)

-- -- General MixerContract error
-- errorMixerContract :: ContractError -> Contract (Maybe (Last Text)) MixerSchema ContractError ()
-- errorMixerContract e = do
--     let msg = case e of
--           OtherContractError txt -> unpack txt
--           _                      -> show e
--     logInfo msg
--     tell $ Just $ Last $ pack msg

-- -- "deposit" endpoint implementation
-- deposit :: Promise (Maybe (Last Text)) MixerSchema ContractError ()
-- deposit = endpoint @"deposit" @DepositParams $ \dp@(DepositParams txt v leaf) -> handleError errorMixerContract $ do
--     logInfo @String "deposit"
--     logInfo dp
--     let mixer = Mixer v vestingValidatorHash vestingValidatorHash failAddress
--         -- value sent to the mixer script
--         -- val   = mValue mixer + mTotalFees mixer + mixerAdaUTXO mixer
--         val = lovelaceValueOf 100_000
--     ct <- currentTime
--     let (lookups', cons') = (mempty, mempty) --depositTokenMintTx (mixer, mixerAddress mixer) ((ct, leaf), zero, TokenName "", PaymentPubKeyHash $ PubKeyHash "")
--         -- total ADA value of all outputs
--         val'              = val + toValue minAdaTxOut
--         lookups           = typedValidatorLookups (mixerTypedValidator mixer) <> lookups'
--         -- must send value to the mixer script and mint deposit token
--         cons              = mustPayToTheScript () val <> cons'
--     -- unbalanced transaction
--     utx  <- mkTxConstraints lookups cons
--     -- adding user wallet inputs and outputs
--     let addr = fromMaybe (pubKeyHashAddress pabWalletPKH (Just $ StakePubKeyHash pabWalletSKH)) $ bech32ToAddress txt
--     logInfo $ bech32ToAddress txt
--     refs <- utxoRefsAt def $ fromMaybe (pubKeyHashAddress pabWalletPKH (Just $ StakePubKeyHash pabWalletSKH)) $ bech32ToAddress "addr_test1qrh8caw4kmlkwydwzdehpyw905dg8ayjv0vpe6vqmkkk5q3psddwydp9ea0gj3jawxyak3d238jpj9fxx3gnfhk7paxqnw2xmw"
--     logInfo refs
--     utxos <- utxosAt $ fromMaybe (pubKeyHashAddress pabWalletPKH (Just $ StakePubKeyHash pabWalletSKH)) $ bech32ToAddress "addr_test1qrh8caw4kmlkwydwzdehpyw905dg8ayjv0vpe6vqmkkk5q3psddwydp9ea0gj3jawxyak3d238jpj9fxx3gnfhk7paxqnw2xmw"
--     logInfo utxos
--     logInfo @String "Prebalancing..."
--     utx' <- balanceTxWithExternalWallet utx (addr, val') (map (lovelaceValueOf . (\i -> 1_100_000 + 20_000 * i)) [0..100])
--     -- logInfo utx'
--     -- final balancing with PAB wallet
--     ctx <- balanceTx utx'
--     txUnsigned <- case ctx of
--         CardanoApiTx (SomeTx tx _) -> pure $ Data.Text.Encoding.decodeUtf8 $ toStrict $ encode $ TxUnsignedCW "1234567890" $ encodeByteString $ serialiseToCBOR tx
--         Both _ (SomeTx tx _)       -> pure $ Data.Text.Encoding.decodeUtf8 $ toStrict $ encode $ TxUnsignedCW "1234567890" $ encodeByteString $ serialiseToCBOR tx
--         EmulatorTx tx              -> pure $ Data.Text.Encoding.decodeUtf8 $ toStrict $ encode tx
--     tell $ Just $ Last txUnsigned

-- -- "deposit" endpoint implementation
-- depositSubmit :: Promise (Maybe (Last Text)) MixerSchema ContractError ()
-- depositSubmit = endpoint @"deposit-submit" @Text $ \txSigned -> handleError errorMixerContract $ do
--     logInfo @String "deposit-submit"
--     logInfo txSigned
--     -- converting CBOR text into CardanoTx
--     let txSignedByteString = fromRight (error ()) $ tryDecode txSigned
--         ctx                = CardanoApiTx $ SomeTx (fromRight (error ()) $ deserialiseFromCBOR AsAlonzoTx txSignedByteString) AlonzoEraInCardanoMode :: CardanoTx
--     -- computing PAB wallet reward
--     let btx    = case ctx of
--                 CardanoApiTx (SomeTx eraInMode tx) -> fromRight (error ()) $ fromCardanoTx tx eraInMode
--                 Both _ (SomeTx eraInMode tx)       -> fromRight (error ()) $ fromCardanoTx tx eraInMode
--                 EmulatorTx tx                      -> fromOnChainTx $ Valid tx
--         inRefs = map txInRef $ Data.Set.toList $ _citxInputs btx
--         outs   = case _citxOutputs btx of
--                     ValidTx a -> a
--                     InvalidTx -> error ()
--     logInfo btx
--     ins <- txOutsFromRefs inRefs
--     let inVal  = sum $ map txOutValue $ filter checkTxOutPKH ins
--         outVal = sum $ map txOutValue $ filter checkTxOutPKH outs
--     if outVal `geq` inVal
--         then void $ submitBalancedTx ctx
--     else throwError $ OtherContractError $ pack $ show $ outVal-inVal --"PAB fee for the deposit transaction is negative!"
--   where
--       checkTxOutPKH o = case txOutAddress o of
--           Address (PubKeyCredential _) (Just (StakingHash (PubKeyCredential skh))) -> skh == pabWalletSKH
--           _ -> False

-- timeToValidateWithdrawal :: POSIXTime
-- timeToValidateWithdrawal = POSIXTime 500_000

-- -- "withdraw" endpoint implementation
-- withdraw :: Promise (Maybe (Last Text)) MixerSchema ContractError ()
-- withdraw = endpoint @"withdraw" @WithdrawParams $ \params@(WithdrawParams txt v _ subs _) -> handleError errorMixerContract $ do
--     logInfo @String "withdraw"
--     logInfo params
--     let mixer = Mixer v vestingValidatorHash vestingValidatorHash failAddress
--     (pkhW, skhW) <- maybe (throwError $ OtherContractError "Wallet address is not correct!") pure $ bech32ToKeyHashes txt
--     let payConstr    = mempty --if isJust skhW
--             -- then mustPayToPubKeyAddress pkhW (fromJust skhW) (mValue mixer + mixerAdaUTXO mixer)
--             -- else mustPayToPubKey pkhW (mValue mixer + mixerAdaUTXO mixer)
--     pkhR   <- ownPaymentPubKeyHash
--     utxos  <- utxosAt (mixerAddress mixer)
--     ct     <- currentTime
--     -- TODO: fix empty list error
--     -- let utxos' = Data.Map.filter (\o -> _ciTxOutValue o `geq` (mValue mixer + mTotalFees mixer + mixerAdaUTXO mixer)) utxos
--     let utxos' = utxos
--     ((utxo1, _), utxos'') <- if null utxos'
--         then throwError $ OtherContractError "The mixing pool is empty!"
--         else pure $ fromJust $ minViewWithKey utxos'

--     logInfo @String "Querying MixerState..."
--     (state, _) <- getMixerState (MixerStateCache [] 0) ct v
--     mKeys <- getMixerKeys v
--     case checkRelayRequest state mKeys params of
--         RelayRequestAccepted -> do
--                 let lookups   = unspentOutputs utxos'' <> typedValidatorLookups (mixerTypedValidator mixer) <> otherScript (mixerValidator mixer)
--                     cons      = payConstr <> mustValidateIn (to $ ct + timeToValidateWithdrawal) <>
--                         mustPayToOtherScript vestingValidatorHash (Datum $ toBuiltinData (ct + 3_600_000 + 100000 + timeToValidateWithdrawal, pkhR))
--                             -- (ct + hourPOSIX + 100000 + timeToValidateWithdrawal) pkhR utxo1 (getWithdrawKeyInput subs))
--                             (mixerCollateral ) <>
--                         mustSpendScriptOutput utxo1 (Redeemer $ toBuiltinData ()) <> mustBeSignedBy pabWalletPKH -- TODO: remove the last constraint after the test
--                 utx <- mkTxConstraints lookups cons
--                 submitTxConfirmed utx
--                 tell $ Just $ Last "RelayRequestAccepted"
--         e                    -> throwError $ OtherContractError $ pack $ show e

-- type MixerSchema = Endpoint "deposit" DepositParams  .\/ Endpoint "deposit-submit" Text .\/ Endpoint "withdraw" WithdrawParams

-- mixerProgram :: Contract (Maybe (Last Text)) MixerSchema ContractError ()
-- mixerProgram = selectList [deposit, withdraw, depositSubmit]

-- -----------------------------------------------------------------------------------

-- data TxUnsignedCW = TxUnsignedCW { passphrase :: String, transaction :: Text }
--     deriving (Generic, FromJSON, ToJSON)

-- newtype TxSignedCW = TxSignedCW { unTxSignedCW :: Text }
--     deriving Show

-- instance FromJSON TxSignedCW where
--     parseJSON (Data.Aeson.Object v) = TxSignedCW <$> v .: "transaction"
--     parseJSON _ = error ()

