{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Tokens.MIXToken (
    mixToken,
    mixTokenInSimulator,
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
import           Plutus.Contract.Types            (AsContractError)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)

import           Tokens.Common
import           Configuration.PABConfig          (mixTokenPolicyId)


------------------------------------ MIX Token ---------------------------------------------------

{-# INLINABLE mixTokenName #-}
mixTokenName :: TokenName
mixTokenName = TokenName "tMIX"

{-# INLINABLE mixTokenSymbol #-}
mixTokenSymbol :: CurrencySymbol
mixTokenSymbol = CurrencySymbol $ foldr consByteString emptyByteString mixTokenPolicyId

{-# INLINABLE mixTokenAssetClass #-}
mixTokenAssetClass :: AssetClass
mixTokenAssetClass = AssetClass (mixTokenSymbol, mixTokenName)

{-# INLINABLE mixToken #-}
mixToken :: Value
mixToken = token mixTokenAssetClass

mixTokenInSimulator :: Value
mixTokenInSimulator = token $ AssetClass (CurrencySymbol $ foldr consByteString emptyByteString
    [234,90,69,0,93,247,236,193,240,29,130,189,8,57,128,143,197,107,192,226,136,118,145,236,43,91,163,42], TokenName "tMIX")

--------------------------- On-Chain -----------------------------

{-# INLINABLE mixTokenRequired #-}
mixTokenRequired :: ScriptContext -> Bool
mixTokenRequired = tokensRequired mixToken

-------------------------- Off-Chain -----------------------------

-- TxConstraints that MIX Token is consumed by transaction
mixTokenTx :: (AsContractError e) => Contract w s e (ScriptLookups a, TxConstraints i o)
mixTokenTx = tokensTx mixTokenAssetClass 1
