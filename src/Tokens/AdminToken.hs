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


module Tokens.AdminToken (
    adminToken,
    adminTokenSymbol,
    adminTokenName,
    adminTokenRequired,
    adminTokenTx
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
import           Configuration.PABConfig          (adminTokenPolicyId)


------------------------------------ Admin Token ---------------------------------------------------

{-# INLINABLE adminTokenName #-}
adminTokenName :: TokenName
adminTokenName = TokenName "Cardano Mixer Admin Token"

{-# INLINABLE adminTokenSymbol #-}
adminTokenSymbol :: CurrencySymbol
adminTokenSymbol = CurrencySymbol $ foldr consByteString emptyByteString adminTokenPolicyId

{-# INLINABLE adminTokenAssetClass #-}
adminTokenAssetClass :: AssetClass
adminTokenAssetClass = AssetClass (adminTokenSymbol, adminTokenName)

{-# INLINABLE adminToken #-}
adminToken :: Value
adminToken = token adminTokenAssetClass

--------------------------- On-Chain -----------------------------

{-# INLINABLE adminTokenRequired #-}
adminTokenRequired :: ScriptContext -> Bool
adminTokenRequired = tokensRequired adminToken

-------------------------- Off-Chain -----------------------------

-- TxConstraints that Admin Token is consumed by transaction
adminTokenTx :: (AsContractError e) => Contract w s e (ScriptLookups a, TxConstraints i o)
adminTokenTx = tokensTx adminTokenAssetClass 1