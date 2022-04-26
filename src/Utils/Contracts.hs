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


module Utils.Contracts where

import           Data.Default                      (def)
import           Data.Map                          (Map, empty, fromList, filterWithKey, keys, elems)
import qualified Data.Map
import           Data.Maybe                        (catMaybes)
import qualified Data.Set
import           Data.Text                         (Text, pack)
import           Ledger                            (PaymentPubKeyHash, Value, Address, ChainIndexTxOut(..), TxOutRef, AssetClass, unPaymentPrivateKey, interval, minAdaTxOut)
import           Ledger.CardanoWallet              (paymentPrivateKey, knownMockWallets)
import           Ledger.Tx                         (Tx(..), TxOut(..), txOutRefId, pubKeyTxIn, toTxOut, addSignature')
import           Plutus.ChainIndex                 (ChainIndexTx, Page(..), nextPageQuery)
import           Plutus.ChainIndex.Api             (TxosResponse(paget), UtxosResponse (page))
import           Plutus.Contract                   (AsContractError, Contract, ContractError (..), mapError, txOutFromRef, throwError, currentSlot)
import           Plutus.Contract.Request           (txoRefsAt, txsFromTxIds, utxoRefsWithCurrency, utxosAt)
import           Plutus.Contract.StateMachine      (SMContractError(..))
import           Plutus.V1.Ledger.Ada              (toValue)
import           Plutus.V1.Ledger.Value            (geq)
import           Ledger.Constraints                (mustPayToPubKey)
import           Ledger.Constraints.OffChain       (UnbalancedTx(..))
import           Ledger.Constraints.TxConstraints  (TxConstraints)
import           PlutusTx.Prelude                  hiding ((<>))
import           Prelude                           (Show(..), Char, String, (<>))


import           Configuration.PABConfig           (PABConfig (..), pabConfig)
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

------------------------------------ Lookups ----------------------------------------

selectUTXO :: Map TxOutRef ChainIndexTxOut -> (TxOutRef, Map TxOutRef ChainIndexTxOut)
selectUTXO utxos = (key, filterWithKey (\k _ -> k == key) utxos)
  where key = head $ keys utxos

selectUTXOWithValue :: Map TxOutRef ChainIndexTxOut -> Value -> (TxOutRef, Value, Map TxOutRef ChainIndexTxOut)
selectUTXOWithValue utxos val = (key, v, filterWithKey (\k _ -> k == key) utxos')
  where utxos' = Data.Map.filter (\o -> _ciTxOutValue o `geq` val) utxos
        key    = head $ keys utxos'
        v      = _ciTxOutValue $ head $ elems utxos'

addUTXOUntil :: Map TxOutRef ChainIndexTxOut -> Value -> [Value] -> Maybe (Value, Map TxOutRef ChainIndexTxOut)
addUTXOUntil utxos val fs = do
    utxos' <- Data.Map.foldrWithKey f (Just empty) utxos
    let change  = sum (map _ciTxOutValue $ Data.Map.elems utxos') - (val + fs !! length (Data.Map.elems utxos'))
    Just (change, utxos')
  where f k o (Just m) = do
          let n = length (Data.Map.elems m)
          actualFee <- if length fs < n - 1 then Nothing else Just $ fs !! n
          if sum (map _ciTxOutValue $ Data.Map.elems m) `geq` (val + actualFee + toValue minAdaTxOut) then Just m else Just $ Data.Map.insert k o m
        f _ _ Nothing = Nothing

--------------------------------- Balancing transactions ----------------------------

balanceTxWithExternalWallet :: UnbalancedTx -> (Address, Value) -> [Value] -> Contract w s ContractError UnbalancedTx
balanceTxWithExternalWallet utx (addr, val) vals = do
    utxos <- utxosAt addr
    (change, utxos') <- case addUTXOUntil utxos val vals of
                          Nothing -> throwError $ OtherContractError "Cannot balance transaction!"
                          Just r  -> pure r -- We assume that val is equal to the difference between outputs and inputs plus the fee
    cs <- case pabConfig of
            Simulator -> currentSlot
            Testnet    -> pure 0
    let tx      = unBalancedTxTx utx
        ins     = txInputs tx `Data.Set.union` Data.Set.fromList (map pubKeyTxIn $ keys utxos')
        outs    = txOutputs tx ++ [TxOut addr change Nothing]
        tx'     = tx { txInputs = ins, txOutputs = outs }
        nInputs = length $ Data.Map.elems utxos'
        actualFee = vals !! nInputs
        utx'    = case pabConfig of
            Simulator -> utx{ unBalancedTxTx = addSignature' (unPaymentPrivateKey $ paymentPrivateKey $ knownMockWallets !! 1) $
              addSignature' (unPaymentPrivateKey $ paymentPrivateKey $ knownMockWallets !! 2) $
              tx' {txFee = actualFee, txValidRange = interval (cs-10) (cs+100), txCollateral = [pubKeyTxIn $ head $ keys utxos']} }
            Testnet -> utx { unBalancedTxTx = tx', unBalancedTxUtxoIndex = Data.Map.map toTxOut utxos' }
    return utx'

---------------------------- Additional Chain Index queries -------------------------

-- | Get the transactions at an address.
txosTxTxOutAt ::
    forall w s e.
    ( AsContractError e
    )
    => Address
    -> Contract w s e [(ChainIndexTx, ChainIndexTxOut)]
txosTxTxOutAt = foldTxoRefsAt f []
  where
    f acc pg = do
      let txoRefs = pageItems pg
      txOuts <- traverse txOutFromRef txoRefs
      let txIds = txOutRefId <$> txoRefs
      txs <- txsFromTxIds txIds
      pure $ acc <> mapMaybe (\(tx, txo) -> fmap (tx,) txo) (zip txs txOuts)

txOutsFromRefs :: forall w s e. (AsContractError e) => [TxOutRef] -> Contract w s e [TxOut]
txOutsFromRefs refs = map toTxOut <$> (catMaybes <$> traverse txOutFromRef refs)

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
utxosWithCurrency = foldUtxoRefsWithCurrency f empty
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