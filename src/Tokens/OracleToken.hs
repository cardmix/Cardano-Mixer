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
import           Plutus.Contract.Types            (AsContractError)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)

import           Tokens.Common
import           Configuration.PABConfig          (adminDecisionTokenPolicyId)


------------------------------------ Oracle Token ---------------------------------------------------

{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TxId -> TokenName
oracleTokenName tx = TokenName $ getTxId tx

{-# INLINABLE oracleTokenSymbol #-}
oracleTokenSymbol :: CurrencySymbol
oracleTokenSymbol = CurrencySymbol $ foldr consByteString emptyByteString adminDecisionTokenPolicyId

{-# INLINABLE oracleTokenAssetClass #-}
oracleTokenAssetClass :: TxId -> AssetClass
oracleTokenAssetClass tx = AssetClass (oracleTokenSymbol, TokenName $ getTxId tx)

{-# INLINABLE oracleToken #-}
oracleToken :: TxId -> Value
oracleToken = token . oracleTokenAssetClass

--------------------------- On-Chain -----------------------------

{-# INLINABLE oracleTokenRequired #-}
oracleTokenRequired :: ScriptContext -> Bool
oracleTokenRequired ctx = tokensRequired (oracleToken bs) ctx
    where bs = txInfoId $ scriptContextTxInfo ctx

-------------------------- Off-Chain -----------------------------

-- TxConstraints that Oracle Token is consumed by transaction
oracleTokenTx :: (AsContractError e) => TxId -> Contract w s e (ScriptLookups a, TxConstraints i o)
oracleTokenTx tx = tokensTx (oracleTokenAssetClass tx) 1