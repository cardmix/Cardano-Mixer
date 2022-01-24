{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE NumericUnderscores         #-}

module Utils.Contracts where

import           Data.Default                      (def)
import           Data.Map                          (Map, empty, fromList)
import           Data.Text                         (Text, pack)
import           Ledger                            (PaymentPubKeyHash, Value, Address, ChainIndexTxOut, TxOutRef, AssetClass)
import           Ledger.Tx                         (txOutRefId)
import           Plutus.ChainIndex                 (ChainIndexTx, Page(..), nextPageQuery)
import           Plutus.ChainIndex.Api             (TxosResponse(paget), UtxosResponse (page))
import           Plutus.Contract                   (Contract, mapError, AsContractError, txOutFromRef)
import           Plutus.Contract.Request           (txoRefsAt, txsFromTxIds, utxoRefsWithCurrency)
import           Plutus.Contract.StateMachine      (SMContractError(..))
import           Ledger.Constraints                (mustPayToPubKey)
import           Ledger.Constraints.TxConstraints  (TxConstraints)
import           PlutusTx.Prelude                  hiding ((<>))
import           Prelude                           (Show(..), Char, String, (<>))

import           Utils.Common                      (drop)

-------------------------------- ByteStrings --------------------------------

{-# INLINABLE buildByteString #-}
buildByteString :: String -> BuiltinByteString
buildByteString str = foldr (consByteString . g) emptyByteString (f str)
    where f s = if length s > 1 then take 2 s : f (drop 2 s) else []
          g s = charToHex (head s) * 16 + charToHex (s !! 1)

charToHex :: Char -> Integer
charToHex '0' = 0
charToHex '1' = 1
charToHex '2' = 2
charToHex '3' = 3
charToHex '4' = 4
charToHex '5' = 5
charToHex '6' = 6
charToHex '7' = 7
charToHex '8' = 8
charToHex '9' = 9
charToHex 'a' = 10
charToHex 'b' = 11
charToHex 'c' = 12
charToHex 'd' = 13
charToHex 'e' = 14
charToHex 'f' = 15
charToHex _   = error ()

byteStringToList :: BuiltinByteString -> [Integer]
byteStringToList bs = map (indexByteString bs) [0..lengthOfByteString bs-1]

--------------------------- Smart Contracts ---------------------------------

collateralConstraints :: PaymentPubKeyHash -> [Value] -> TxConstraints i o
collateralConstraints pkh vals = mconcat $ map (mustPayToPubKey pkh) vals

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

---------------------------- Additional Chain Index queries ------------------------

-- | Get the transactions at an address.
txosTxTxOutAt ::
    forall w s e.
    ( AsContractError e
    )
    => Address
    -> Contract w s e [(ChainIndexTx, ChainIndexTxOut)]
txosTxTxOutAt addr = do
  foldTxoRefsAt f [] addr
  where
    f acc pg = do
      let txoRefs = pageItems pg
      txOuts <- traverse txOutFromRef txoRefs
      let txIds = txOutRefId <$> txoRefs
      txs <- txsFromTxIds txIds
      pure $ acc <> mapMaybe (\(tx, txo) -> fmap (tx,) txo) (zip txs txOuts)

foldTxoRefsAt ::
    forall w s e a.
    ( AsContractError e
    )
    => (a -> Page TxOutRef -> Contract w s e a)
    -> a
    -> Address
    -> Contract w s e a
foldTxoRefsAt f ini addr = go ini (Just def)
  where
    go acc Nothing = pure acc
    go acc (Just pq) = do
      pg <- paget <$> txoRefsAt pq addr
      newAcc <- f acc pg
      go newAcc (nextPageQuery pg)

-- | Get the unspent transaction outputs with a given Currency.
utxosWithCurrency ::
    forall w s e.
    ( AsContractError e
    )
    => AssetClass
    -> Contract w s e (Map TxOutRef ChainIndexTxOut)
utxosWithCurrency ac = do
  foldUtxoRefsWithCurrency f empty ac
  where
    f acc pg = do
      let utxoRefs = pageItems pg
      txOuts <- traverse txOutFromRef utxoRefs
      let utxos = fromList
                $ mapMaybe (\(ref, txOut) -> fmap (ref,) txOut)
                $ zip utxoRefs txOuts
      pure $ acc <> utxos

-- | Fold through each 'Page's of unspent 'TxOutRef's with a given Currency, and
-- accumulate the result.
foldUtxoRefsWithCurrency ::
    forall w s e a.
    ( AsContractError e
    )
    => (a -> Page TxOutRef -> Contract w s e a) -- ^ Accumulator function
    -> a -- ^ Initial value
    -> AssetClass -- ^ Address which contain the UTXOs
    -> Contract w s e a
foldUtxoRefsWithCurrency f ini ac = go ini (Just def)
  where
    go acc Nothing = pure acc
    go acc (Just pq) = do
      pg <- page <$> utxoRefsWithCurrency pq ac
      newAcc <- f acc pg
      go newAcc (nextPageQuery pg)