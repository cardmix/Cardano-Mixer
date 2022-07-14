{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Tokens.MIXToken where

import           Control.Monad.State              (State)
import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)

import           Tokens.OneShotCurrency           (OneShotCurrencyParams, mkCurrency, oneShotCurrencyPolicy, oneShotCurrencyMintTx)
import           Types.TxConstructor              (TxConstructor)

--------------------------------------- On-Chain --------------------------------------------------

{-# INLINABLE mixTokenName #-}
mixTokenName :: TokenName
mixTokenName = TokenName "tMIX"

{-# INLINABLE mixTokenAmount #-}
mixTokenAmount :: Integer
mixTokenAmount = 100_000_000

{-# INLINABLE mixTokenParams #-}
mixTokenParams :: TxOutRef -> OneShotCurrencyParams
mixTokenParams ref = mkCurrency ref [(mixTokenName, mixTokenAmount)]

curPolicy :: TxOutRef -> MintingPolicy
curPolicy = oneShotCurrencyPolicy . mixTokenParams

------------------------------------------ Off-Chain ----------------------------------------------

mixTokenCurrencySymbol :: TxOutRef -> CurrencySymbol
mixTokenCurrencySymbol = scriptCurrencySymbol . curPolicy

mixTokenAssetClass :: TxOutRef -> AssetClass
mixTokenAssetClass ref = AssetClass (mixTokenCurrencySymbol ref, mixTokenName)

mixToken :: TxOutRef -> Value
mixToken = token . mixTokenAssetClass

mixTokenMintTx :: TxOutRef -> State (TxConstructor d a i o) (Maybe (TxOutRef, ChainIndexTxOut))
mixTokenMintTx = oneShotCurrencyMintTx . mixTokenParams