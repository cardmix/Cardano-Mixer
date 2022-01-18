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


module Tokens.CollateralToken (
    collateralToken,
    collateralTokenSymbol,
    collateralTokenName,
    collateralTokenRequired,
    collateralTokenTx
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
import           Configuration.PABConfig          (collateralTokenPolicyId)


------------------------------------ Collateral Token ---------------------------------------------------

{-# INLINABLE collateralTokenName #-}
collateralTokenName :: TokenName
collateralTokenName = TokenName "Cardano Mixer Collateral Token"

{-# INLINABLE collateralTokenSymbol #-}
collateralTokenSymbol :: CurrencySymbol
collateralTokenSymbol = CurrencySymbol $ foldr consByteString emptyByteString collateralTokenPolicyId

{-# INLINABLE collateralTokenAssetClass #-}
collateralTokenAssetClass :: AssetClass
collateralTokenAssetClass = AssetClass (collateralTokenSymbol, collateralTokenName)

{-# INLINABLE collateralToken #-}
collateralToken :: Value
collateralToken = token collateralTokenAssetClass

--------------------------- On-Chain -----------------------------

{-# INLINABLE collateralTokenRequired #-}
collateralTokenRequired :: ScriptContext -> Bool
collateralTokenRequired = tokensRequired collateralToken

-------------------------- Off-Chain -----------------------------

-- TxConstraints that Collateral Token is consumed by transaction
collateralTokenTx :: (AsContractError e) => Contract w s e (ScriptLookups a, TxConstraints i o)
collateralTokenTx = tokensTx collateralTokenAssetClass 1