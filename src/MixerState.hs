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

import           Data.Either                              (rights)
import           Data.Semigroup                           (Last (..))
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Value                             (geq)
import           Plutus.Contract                          (Promise, ContractError, Endpoint, endpoint, tell)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  ((^))

import           Crypto
import           MixerScript
import           Utility                                  (txosTxTxOutAt, drop)

----------------------- Data types, instances, and constants -----------------------------

treeSize :: Integer 
treeSize = 10

type MerkleTree = [Fr]
type MixerState = [MerkleTree]

---------------------------------------------------------------------
--------------------------- Off-Chain -------------------------------
---------------------------------------------------------------------

-- instance Ord (ChainIndexTx, ChainIndexTxOut) where
--     (compare) (tx1, txo1) (tx2, txo2) = t1 `compare` t2
--         where
--             t1 = ivTo $ _citxValidRange tx1
--             t2 = ivTo $ _citxValidRange tx2

getMixerState :: Promise (Maybe (Last MixerState)) MixerStateSchema ContractError ()
getMixerState = endpoint @"Get Mixer state" $ \v -> do
    let mixer = Mixer v v
    txTxos <- txosTxTxOutAt (mixerAddress mixer)
    let txTxos' = filter (\(_, o) -> _ciTxOutValue o `geq` mValue mixer) txTxos -- TODO: implement proper sort?
        leafs   = map (getMixerDatum . unsafeFromBuiltinData . getDatum) $ rights $ map (_ciTxOutDatum . snd) txTxos'
    let state = snd $ constructStateFromList (leafs, [])
    tell $ Just $ Last state
  
constructStateFromList :: ([Fr], MixerState) -> ([Fr], MixerState)
constructStateFromList ([], state)  = ([], state)
constructStateFromList (lst, state) = constructStateFromList (drop (2 ^ treeSize) lst, state ++ [tree'])
    where tree  = take (2 ^ treeSize) lst
          tree' = padToPowerOfTwo treeSize tree

type MixerStateSchema = Endpoint "Get Mixer state" Value