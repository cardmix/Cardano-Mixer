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


module Tokens.RelayToken (
    relayToken,
    relayTokenSymbol,
    relayTokenName,
    relayTokenNumber,
    relayTokenRequired,
    relayTokenTx
) where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints.OffChain      (ScriptLookups)
import           Ledger.Constraints.TxConstraints (TxConstraints)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..), CurrencySymbol (..))
import           Plutus.Contract                  (Contract)
import           Plutus.Contract.Types            (AsContractError)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)


import           Tokens.Common
import           Configuration.PABConfig          (adminDecisionTokenPolicyId)

------------------------------------ Relay Token -----------------------------------------------

relayTokenNumber :: Integer
relayTokenNumber = 60

{-# INLINABLE relayTokenName #-}
relayTokenName :: Integer -> TokenName
relayTokenName i = TokenName $ consByteString i emptyByteString

{-# INLINABLE relayTokenSymbol #-}
relayTokenSymbol :: CurrencySymbol
relayTokenSymbol = CurrencySymbol $ foldr consByteString emptyByteString adminDecisionTokenPolicyId

{-# INLINABLE relayTokenAssetClass #-}
relayTokenAssetClass :: Integer -> AssetClass
relayTokenAssetClass i = AssetClass (relayTokenSymbol, relayTokenName i)

{-# INLINABLE relayToken #-}
relayToken :: Integer -> Value
relayToken = token . relayTokenAssetClass

--------------------------- On-Chain -----------------------------

{-# INLINABLE relayTokenRequired #-}
relayTokenRequired :: Integer -> ScriptContext -> Bool
relayTokenRequired = tokensRequired . relayToken

-------------------------- Off-Chain -----------------------------

-- TxConstraints that Relay Token is consumed by transaction
relayTokenTx :: (AsContractError e) => Integer -> Contract w s e (ScriptLookups a, TxConstraints i o)
relayTokenTx i = tokensTx (relayTokenAssetClass i) 1