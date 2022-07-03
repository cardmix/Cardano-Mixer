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

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)

import           Configuration.PABConfig          (mixTokenTxOutRef)
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
mixTokenParams :: OneShotCurrencyParams
mixTokenParams = mkCurrency mixTokenTxOutRef [(mixTokenName, mixTokenAmount)]

curPolicy :: MintingPolicy
curPolicy = oneShotCurrencyPolicy mixTokenParams

------------------------------------------ Off-Chain ----------------------------------------------

mixTokenCurrencySymbol :: CurrencySymbol
mixTokenCurrencySymbol = scriptCurrencySymbol curPolicy

mixTokenAssetClass :: AssetClass
mixTokenAssetClass = AssetClass (mixTokenCurrencySymbol, mixTokenName)

mixToken :: Value
mixToken = token mixTokenAssetClass

mixTokenMintTx :: TxConstructor a i o -> TxConstructor a i o
mixTokenMintTx = oneShotCurrencyMintTx mixTokenParams