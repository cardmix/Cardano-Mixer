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



module Tokens.OracleToken (
    oracleToken,
    oracleTokenSymbol,
    oracleTokenName,
    oracleTokenRequired,
    oracleTokenTx
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
import           Configuration.PABConfig          (oracleTokenPolicyId)


------------------------------------ Oracle Token ---------------------------------------------------

{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TokenName
oracleTokenName = TokenName "Cardano Mixer Oracle Token"

{-# INLINABLE oracleTokenSymbol #-}
oracleTokenSymbol :: CurrencySymbol
oracleTokenSymbol = CurrencySymbol $ foldr consByteString emptyByteString oracleTokenPolicyId

{-# INLINABLE oracleTokenAssetClass #-}
oracleTokenAssetClass :: AssetClass
oracleTokenAssetClass = AssetClass (oracleTokenSymbol, oracleTokenName)

{-# INLINABLE oracleToken #-}
oracleToken :: Value
oracleToken = token oracleTokenAssetClass

--------------------------- On-Chain -----------------------------

{-# INLINABLE oracleTokenRequired #-}
oracleTokenRequired :: ScriptContext -> Bool
oracleTokenRequired = tokensRequired oracleToken

-------------------------- Off-Chain -----------------------------

-- TxConstraints that Oracle Token is consumed by transaction
oracleTokenTx :: (AsContractError e) => Contract w s e (ScriptLookups a, TxConstraints i o)
oracleTokenTx = tokensTx oracleTokenAssetClass 1