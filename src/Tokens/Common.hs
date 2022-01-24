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


module Tokens.Common (
    tokensRequired,
    tokensTx
) where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints.TxConstraints (TxConstraints, mustSpendAtLeast)
import           Ledger.Constraints.OffChain      (ScriptLookups, unspentOutputs)
import           Ledger.Value                     (geq, assetClassValue)
import           Plutus.Contract                  (Contract)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)
import           Wallet.Types                     (AsContractError)

import           Utils.Contracts                  (utxosWithCurrency)

--------------------------- On-Chain -----------------------------

{-# INLINABLE tokensRequired #-}
tokensRequired :: Value -> ScriptContext -> Bool
tokensRequired v ctx = val `geq` v
    where info = scriptContextTxInfo ctx
          ins  = txInfoInputs info
          val  = sum (map (txOutValue . txInInfoResolved) ins)

-------------------------- Off-Chain -----------------------------

-- TxConstraints that the tokens are consumed by the transaction
tokensTx :: (AsContractError e) => AssetClass -> Integer -> Contract w s e (ScriptLookups a, TxConstraints i o)
tokensTx ac n = do
    utxos <- utxosWithCurrency ac
    return (unspentOutputs utxos, mustSpendAtLeast $ assetClassValue ac n)