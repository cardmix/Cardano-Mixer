{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}


module Utils.ChainIndex where

import           Data.Default                      (def)
import           Data.Map                          (Map, empty, fromList)
import           Data.Maybe                        (catMaybes)
import           Ledger                            (Address, ChainIndexTxOut(..), TxOutRef, AssetClass)
import           Ledger.Tx                         (TxOut(..), txOutRefId, toTxOut)
import           Plutus.ChainIndex                 (ChainIndexTx, Page(..), nextPageQuery)
import           Plutus.ChainIndex.Api             (TxosResponse(paget), UtxosResponse (page))
import           Plutus.Contract                   (AsContractError, Contract, txOutFromRef)
import           Plutus.Contract.Request           (txoRefsAt, txsFromTxIds, utxoRefsWithCurrency)
import           PlutusTx.Prelude                  hiding ((<>))
import           Prelude                           ((<>))

type ChainIndexCache = Map TxOutRef (ChainIndexTxOut, ChainIndexTx)

--------------------------- Tx refs folding --------------------------------

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

------------------------------ Tx outputs query -----------------------------

-- | Get the transactions at an address.
txosTxOutTxAt ::
    forall w s e.
    ( AsContractError e
    )
    => Address
    -> Contract w s e [(ChainIndexTxOut, ChainIndexTx)]
txosTxOutTxAt = foldTxoRefsAt f []
  where
    f acc pg = do
      let txoRefs = pageItems pg
      txOuts <- traverse txOutFromRef txoRefs
      let txIds = txOutRefId <$> txoRefs
      txs <- txsFromTxIds txIds
      pure $ acc <> mapMaybe (\(txo, tx) -> fmap (,tx) txo) (zip txOuts txs)

-- | Get the transactions at an address.
txosTxOutAt ::
    forall w s e.
    ( AsContractError e
    )
    => Address
    -> Contract w s e [ChainIndexTxOut]
txosTxOutAt = foldTxoRefsAt f []
  where
    f acc pg = do
      let txoRefs = pageItems pg
      txOuts <- traverse txOutFromRef txoRefs
      pure $ acc <> catMaybes txOuts

-- | Get the transactions at an address.
txosTxRefTxOutAt ::
    forall w s e.
    ( AsContractError e
    )
    => Address
    -> Contract w s e [(TxOutRef, ChainIndexTxOut)]
txosTxRefTxOutAt = foldTxoRefsAt f []
  where
    f acc pg = do
      let txoRefs = pageItems pg
      txOuts <- traverse txOutFromRef txoRefs
      pure $ acc <> mapMaybe (\(tx, txo) -> fmap (tx,) txo) (zip txoRefs txOuts)

txOutsFromRefs :: forall w s e. (AsContractError e) => [TxOutRef] -> Contract w s e [TxOut]
txOutsFromRefs refs = map toTxOut <$> (catMaybes <$> traverse txOutFromRef refs)

------------------------- Unspent Tx outputs query -----------------------------

-- | Get the unspent transaction outputs with a given Currency.
utxosWithCurrency ::
    forall w s e.
    ( AsContractError e
    )
    => AssetClass
    -> Contract w s e (Map TxOutRef ChainIndexTxOut)
utxosWithCurrency = foldUtxoRefsWithCurrency f empty
  where
    f acc pg = do
      let utxoRefs = pageItems pg
      txOuts <- traverse txOutFromRef utxoRefs
      let utxos = fromList
                $ mapMaybe (\(ref, txOut) -> fmap (ref,) txOut)
                $ zip utxoRefs txOuts
      pure $ acc <> utxos