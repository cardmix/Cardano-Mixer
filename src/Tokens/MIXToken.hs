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
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}



module Tokens.MIXToken (
    mixToken,
    mixTokenSymbol,
    mixTokenName,
    mixTokenRequired,
    mixTokenTx
) where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints.OffChain      (ScriptLookups)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..), CurrencySymbol (..))
import           Plutus.Contract                  (Contract)
import           Plutus.Contract.StateMachine
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)
import           Wallet.Types                     (AsContractError)

import           Tokens.Common
import           Configuration.PABConfig          (mixTokenPolicyId)


------------------------------------ MIX Token ---------------------------------------------------

{-# INLINABLE mixTokenName #-}
mixTokenName :: TokenName
mixTokenName = TokenName "MIX"

{-# INLINABLE mixTokenSymbol #-}
mixTokenSymbol :: CurrencySymbol
mixTokenSymbol = CurrencySymbol $ foldr consByteString emptyByteString mixTokenPolicyId

{-# INLINABLE mixTokenAssetClass #-}
mixTokenAssetClass :: AssetClass
mixTokenAssetClass = AssetClass (mixTokenSymbol, mixTokenName)

{-# INLINABLE mixToken #-}
mixToken :: Value
mixToken = token mixTokenAssetClass

--------------------------- On-Chain -----------------------------

{-# INLINABLE mixTokenRequired #-}
mixTokenRequired :: ScriptContext -> Bool
mixTokenRequired = tokensRequired mixToken

-------------------------- Off-Chain -----------------------------

-- TxConstraints that MIX Token is consumed by transaction
mixTokenTx :: (AsContractError e) => Contract w s e (ScriptLookups a, TxConstraints i o)
mixTokenTx = tokensTx mixTokenAssetClass 1