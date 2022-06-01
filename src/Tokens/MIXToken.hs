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
import           Ledger.Value                     (AssetClass(..), TokenName (..), CurrencySymbol (CurrencySymbol))
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)

import           Configuration.PABConfig          (mixTokenPolicyId)

------------------------------------ MIX Token ---------------------------------------------------

mixTokenName :: TokenName
mixTokenName = TokenName "tMIX"

mixTokenAmount :: Integer
mixTokenAmount = 100_000_000

mixTokenSymbol :: CurrencySymbol
mixTokenSymbol = CurrencySymbol mixTokenPolicyId

mixTokenAssetClass :: AssetClass
mixTokenAssetClass = AssetClass (mixTokenSymbol, mixTokenName)

mixToken :: Value
mixToken = token mixTokenAssetClass