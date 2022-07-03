{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}


-- | Implements a custom currency with a minting policy that allows
--   the minting of a fixed amount of units.
module Contracts.CurrencyContract (
    CurrencySchema
    , CurrencyError(..)
    , AsCurrencyError(..)
    , mintContract
    , SimpleMPS(..)
    , mintCurrency
    ) where

import           Control.Lens
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Map                        (singleton)
import           Data.Semigroup                  (Last (..))
import           GHC.Generics                    (Generic)
import           Ledger                          (getCardanoTxId)
import qualified Ledger.Constraints              as Constraints
import qualified Ledger.Typed.Scripts            as Scripts
import           Ledger.Value                    (TokenName)
import           Plutus.Contract                 as Contract
import           Plutus.Contract.Wallet          (getUnspentOutput)
import           PlutusTx.Prelude                hiding (Monoid (..), Semigroup (..))
import           Prelude                         (Semigroup (..))
import qualified Prelude                         as Haskell
import           Schema                          (ToSchema)

import           Tokens.OneShotCurrency

newtype CurrencyError =
    CurContractError ContractError
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''CurrencyError

instance AsContractError CurrencyError where
    _ContractError = _CurContractError

-- | @mint [(n1, c1), ..., (n_k, c_k)]@ creates a new currency with
--   @k@ token names, minting @c_i@ units of each token @n_i@.
--   If @k == 0@ then no value is minted. A one-shot minting policy
--   script is used to ensure that no more units of the currency can
--   be minted afterwards.
mintContract :: forall w s e. AsCurrencyError e => SimpleMPS -> Contract w s e OneShotCurrencyParams
mintContract SimpleMPS{tokenName, amount} = mapError (review _CurrencyError) $ do
    txOutRef <- getUnspentOutput
    ciTxOut <- txOutFromRef txOutRef
    let utxos       = maybe Haskell.mempty (Data.Map.singleton txOutRef) ciTxOut
        theCurrency = mkCurrency txOutRef [(tokenName, amount)]
        curVali     = oneShotCurrencyPolicy theCurrency
        lookups     = Constraints.mintingPolicy curVali
                        <> Constraints.unspentOutputs utxos
        mintTx      = Constraints.mustSpendPubKeyOutput txOutRef
                        <> Constraints.mustMintValue (currencyValue theCurrency)
    tx <- submitTxConstraintsWith @Scripts.Any lookups mintTx
    _ <- awaitTxConfirmed (getCardanoTxId tx)
    pure theCurrency

-- | Minting policy for a currency that has a fixed amount of tokens issued
--   in one transaction
data SimpleMPS = SimpleMPS
        {
            tokenName :: TokenName,
            amount    :: Integer
        }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (FromJSON, ToJSON, ToSchema)

type CurrencySchema = Endpoint "create-native-token" SimpleMPS

-- | Use 'mintContract' to create the currency specified by a 'SimpleMPS'
mintCurrency :: Promise (Maybe (Last OneShotCurrencyParams)) CurrencySchema CurrencyError OneShotCurrencyParams
mintCurrency = endpoint @"create-native-token" $ \simpleMPS -> do
    cur <- mintContract simpleMPS
    tell (Just (Last cur))
    pure cur