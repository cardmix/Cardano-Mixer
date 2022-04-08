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
                                                            submitTxConfirmed, utxosTxOutTxAt, txOutFromRef, waitNSlots, logInfo)
import           Plutus.Contract.Types                    (ContractError(..), AsContractError)
import           Plutus.V1.Ledger.Ada                     (lovelaceValueOf)
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (String, Foldable (null), (<>), (<$>))

import           Configuration.PABConfig                  (pabWalletPKH)
import           Tokens.MIXToken                          (mixToken)


dispenserAddress :: Address
dispenserAddress = pubKeyHashAddress pabWalletPKH Nothing

dispenserAmount :: Value
dispenserAmount = lovelaceValueOf 2_000_000 + scale 100_000 mixToken

getSender :: AsContractError e => ChainIndexTx -> Contract w s e (Maybe PaymentPubKeyHash)
getSender tx = do
    let ref = txInRef $ head $ Data.Set.toList $ _citxInputs tx
    txo <- txOutFromRef ref
    return $ do
        addr <- _ciTxOutAddress <$> txo
        pkh  <- toPubKeyHash addr
        return $ PaymentPubKeyHash pkh

sendTokens :: Contract (Maybe (Last String)) EmptySchema ContractError ()
sendTokens = do
    utxosTxOut  <- utxosTxOutTxAt dispenserAddress
    let utxosTxOut' = Data.Map.filter (\o -> _ciTxOutValue (fst o) == lovelaceValueOf 2_000_000) utxosTxOut
    if Prelude.null utxosTxOut' then return () else do
        let (ref, (utxo, tx)) = head $ Data.Map.toList utxosTxOut'
        a <- getSender tx
        case a of
          Nothing  -> return ()
          Just pkh -> do
            let lookups = unspentOutputs $ Data.Map.singleton ref utxo
                cons    = mustPayToPubKey pkh dispenserAmount <> mustSpendPubKeyOutput ref
            utx <- mkTxConstraints @Scripts.Any lookups cons
            submitTxConfirmed utx

dispenserProgram :: Integer -> Contract (Maybe (Last String)) EmptySchema ContractError ()
dispenserProgram n = do
    sendTokens
    _ <- waitNSlots 1
    if n <= 0 then logInfo @String "Finished dispenser program!" else dispenserProgram (n-1)

