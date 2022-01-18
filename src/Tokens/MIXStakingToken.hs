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



module Tokens.MIXStakingToken (
    mixStakingToken,
    mixStakingTokenSymbol,
    mixStakingTokenName,
    mixStakingTokenRequired,
    mixStakingTokenTx
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
import           Configuration.PABConfig          (mixStakingTokenPolicyId)


------------------------------------ Collateral Token ---------------------------------------------------

{-# INLINABLE mixStakingTokenName #-}
mixStakingTokenName :: TokenName
mixStakingTokenName = TokenName "Cardano Mixer Staking Token"

{-# INLINABLE mixStakingTokenSymbol #-}
mixStakingTokenSymbol :: CurrencySymbol
mixStakingTokenSymbol = CurrencySymbol $ foldr consByteString emptyByteString mixStakingTokenPolicyId

{-# INLINABLE mixStakingTokenAssetClass #-}
mixStakingTokenAssetClass :: AssetClass
mixStakingTokenAssetClass = AssetClass (mixStakingTokenSymbol, mixStakingTokenName)

{-# INLINABLE mixStakingToken #-}
mixStakingToken :: Value
mixStakingToken = token mixStakingTokenAssetClass

--------------------------- On-Chain -----------------------------

{-# INLINABLE mixStakingTokenRequired #-}
mixStakingTokenRequired :: ScriptContext -> Bool
mixStakingTokenRequired = tokensRequired mixStakingToken

-------------------------- Off-Chain -----------------------------

-- TxConstraints that MIX Staking Token is consumed by transaction
mixStakingTokenTx :: (AsContractError e) => Contract w s e (ScriptLookups a, TxConstraints i o)
mixStakingTokenTx = tokensTx mixStakingTokenAssetClass 1