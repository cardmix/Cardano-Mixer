{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}


module Contracts.DispenserContract (
    dispenserProgram
) where


import qualified Data.Map
import           Data.Semigroup                           (Last (..))
import qualified Data.Set
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Constraints.OffChain              (unspentOutputs)
import           Ledger.Constraints.TxConstraints
import qualified Ledger.Typed.Scripts                     as Scripts
import           Plutus.ChainIndex                        (ChainIndexTx(..))
import           Plutus.Contract                          (Contract, EmptySchema, mkTxConstraints,
                                                            submitTxConfirmed, txOutFromRef, waitNSlots, logInfo)
import           Plutus.Contract.Request                  (utxosTxOutTxAt)
import           Plutus.Contract.Types                    (ContractError(..), AsContractError)
import           Plutus.V1.Ledger.Ada                     (lovelaceValueOf, toValue)
import           Plutus.V1.Ledger.Credential              (Credential(..), StakingCredential (..))
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (String, Foldable (null, length), (<>), (<$>))

import           Configuration.PABConfig                  (dispenserWalletPKH)
import           Tokens.MIXToken                          (mixToken)


dispenserAddress :: Address
dispenserAddress = pubKeyHashAddress dispenserWalletPKH Nothing

dispenserAmount :: Value
dispenserAmount = toValue minAdaTxOut + scale 100_000 mixToken

getSender :: AsContractError e => ChainIndexTx -> Contract w s e (Maybe (PaymentPubKeyHash, StakePubKeyHash))
getSender tx = do
    let ref = txInRef $ head $ Data.Set.toList $ _citxInputs tx
    txo <- txOutFromRef ref
    return $ do
        addr <- _ciTxOutAddress <$> txo
        case addr of
            Address (PubKeyCredential (PubKeyHash h1)) (Just (StakingHash (PubKeyCredential (PubKeyHash h2)))) -> 
                let pkh = PaymentPubKeyHash $ PubKeyHash h1
                    skh = StakePubKeyHash $ PubKeyHash h2
                in Just (pkh, skh)
            _ -> Nothing

sendTokens :: Contract (Maybe (Last String)) EmptySchema ContractError ()
sendTokens = do
    logInfo @String "Start dispensing..."
    utxosTxOut  <- utxosTxOutTxAt dispenserAddress
    logInfo $ Prelude.length utxosTxOut
    let utxosTxOut' = Data.Map.filter (\o -> _ciTxOutValue (fst o) == lovelaceValueOf 2_000_000) utxosTxOut
    if Prelude.null utxosTxOut' then return () else do
        let (ref, (utxo, tx)) = head $ Data.Map.toList utxosTxOut'
        a <- getSender tx
        case a of
          Nothing  -> return ()
          Just (pkh, skh) -> do
            let lookups = unspentOutputs $ Data.Map.singleton ref utxo
                cons    = mustPayToPubKeyAddress pkh skh dispenserAmount <> mustSpendPubKeyOutput ref
            utx <- mkTxConstraints @Scripts.Any lookups cons
            submitTxConfirmed utx

dispenserProgram :: Contract (Maybe (Last String)) EmptySchema ContractError ()
dispenserProgram = do
    sendTokens
    _ <- waitNSlots 60
    dispenserProgram

