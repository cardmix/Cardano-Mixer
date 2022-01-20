{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module MixerState where

import           Data.Aeson                               (ToJSON, FromJSON)
import           Data.Either                              (rights)
import           Data.Semigroup                           (Last (..))
import           GHC.Generics                             (Generic)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Value                             (geq)
import           Plutus.Contract                          (Promise, ContractError, Endpoint, endpoint, tell, Contract)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (Show, (^))

import           Crypto
import           MixerScript
import           Utility                                  (txosTxTxOutAt, drop)


----------------------- Data types, instances, and constants -----------------------------

treeSize :: Integer
treeSize = 10

data MerkleTree = MerkleTree Integer [Fr]
    deriving (Show, Generic, ToJSON, FromJSON)
type MixerState = [MerkleTree]

---------------------------------------------------------------------
--------------------------- Off-Chain -------------------------------
---------------------------------------------------------------------

-- instance Ord (ChainIndexTx, ChainIndexTxOut) where
--     (compare) (tx1, txo1) (tx2, txo2) = t1 `compare` t2
--         where
--             t1 = ivTo $ _citxValidRange tx1
--             t2 = ivTo $ _citxValidRange tx2

getMixerState :: Value -> Contract (Maybe (Last MixerState)) s ContractError MixerState
getMixerState v = do
    let mixer = makeMixerFromFees v
    txTxos <- txosTxTxOutAt (mixerAddress mixer)
    let txTxos' = filter (\(_, o) -> _ciTxOutValue o `geq` (mValue mixer + mTotalFees mixer)) txTxos -- TODO: implement proper sort?
        leafs   = map (getMixerDatum . unsafeFromBuiltinData . getDatum) $ rights $ map (_ciTxOutDatum . snd) txTxos'
    let state = snd $ constructStateFromList (leafs, [])
    tell $ Just $ Last state
    return state

type MixerStateSchema = Endpoint "Get Mixer state" Value

getMixerStatePromise :: Promise (Maybe (Last MixerState)) MixerStateSchema ContractError MixerState
getMixerStatePromise = endpoint @"Get Mixer state" @Value getMixerState

------------------------------------------------------------------------

getMerkleLeafNumber :: MixerState -> Fr -> Maybe (Integer, Integer)
getMerkleLeafNumber state leaf = do
        k <- nTree state
        m <- nDep $ state !! k
        return (k, m + 1)
    where nDep  (MerkleTree _ tree) = findIndex (leaf ==) tree
          nTree s                   = findIndex ((/=) Nothing . nDep) s

getMerkleTree :: MixerState -> (Integer, Integer) -> Maybe MerkleTree
getMerkleTree state (k, m) = do
    MerkleTree _ tree <- if length state <= k
                            then Nothing
                            else Just $ state !! k
    if 1 > m || m > 2^treeSize
        then Nothing
        else Just $ MerkleTree m $ padToPowerOfTwo treeSize $ take m tree

constructStateFromList :: ([Fr], MixerState) -> ([Fr], MixerState)
constructStateFromList ([], state)  = ([], state)
constructStateFromList (lst, state) = constructStateFromList (drop (2 ^ treeSize) lst, state ++ [MerkleTree n tree'])
    where tree  = take (2 ^ treeSize) lst
          tree' = padToPowerOfTwo treeSize tree
          n     = length tree