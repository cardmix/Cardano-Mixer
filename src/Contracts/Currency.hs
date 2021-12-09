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
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE NumericUnderscores #-}


-- | Implements a custom currency with a minting policy that allows
--   the minting of a fixed amount of units.
module Contracts.Currency(
      OneShotCurrency(..)
    , CurrencySchema
    , CurrencyError(..)
    , AsCurrencyError(..)
    , curPolicy
    -- * Actions etc
    , mintContract
    , mintedValue
    , currencySymbol
    -- * Simple minting policy currency
    , SimpleMPS(..)
    , mintCurrency
    ) where

import           Control.Lens
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.String                     (String)
import           Data.Map                        (singleton)
import           Data.Semigroup                  (Last (..))
import           GHC.Generics                    (Generic)
import           Ledger                          (CurrencySymbol, TxId, TxOutRef (..), getCardanoTxId, scriptCurrencySymbol)
import qualified Ledger.Constraints              as Constraints
import qualified Ledger.Contexts                 as V
import           Ledger.Scripts
import qualified Ledger.Typed.Scripts            as Scripts
import           Ledger.Value                    (TokenName, Value)
import qualified Ledger.Value                    as Value
import           Plutus.Contract                 as Contract
import           Plutus.Contract.Wallet          (getUnspentOutput)
import           Plutus.V1.Ledger.Ada            (lovelaceValueOf)
import qualified PlutusTx
import qualified PlutusTx.AssocMap               as AssocMap
import           PlutusTx.Prelude                hiding (Monoid (..), Semigroup (..))
import           Prelude                         (Semigroup (..), Show (show))
import qualified Prelude                         as Haskell
import           Schema                          (ToSchema)

import           Utility                         (collateralConstraints, untilRight)


{- HLINT ignore "Use uncurry" -}

-- | A currency that can be created exactly once
data OneShotCurrency = OneShotCurrency
  { curRefTransactionOutput :: (TxId, Integer)
  -- ^ Transaction input that must be spent when
  --   the currency is minted.
  , curAmounts              :: AssocMap.Map TokenName Integer
  -- ^ How many units of each 'TokenName' are to
  --   be minted.
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''OneShotCurrency

currencyValue :: CurrencySymbol -> OneShotCurrency -> Value
currencyValue s OneShotCurrency{curAmounts = amts} =
    let
        values = map (\(tn, i) -> Value.singleton s tn i) (AssocMap.toList amts)
    in fold values

mkCurrency :: TxOutRef -> [(TokenName, Integer)] -> OneShotCurrency
mkCurrency (TxOutRef h i) amts =
    OneShotCurrency
        { curRefTransactionOutput = (h, i)
        , curAmounts              = AssocMap.fromList amts
        }

checkPolicy :: OneShotCurrency -> () -> V.ScriptContext -> Bool
checkPolicy c@(OneShotCurrency (refHash, refIdx) _) _ ctx@V.ScriptContext{V.scriptContextTxInfo=txinfo} =
    let
        -- see note [Obtaining the currency symbol]
        ownSymbol = V.ownCurrencySymbol ctx

        minted = V.txInfoMint txinfo
        expected = currencyValue ownSymbol c

        -- True if the pending transaction mints the amount of
        -- currency that we expect
        mintOK =
            let v = expected == minted
            in traceIfFalse "C0" {-"Value minted different from expected"-} v

        -- True if the pending transaction spends the output
        -- identified by @(refHash, refIdx)@
        txOutputSpent =
            let v = V.spendsOutput txinfo refHash refIdx
            in  traceIfFalse "C1" {-"Pending transaction does not spend the designated transaction output"-} v

    in mintOK && txOutputSpent

curPolicy :: OneShotCurrency -> MintingPolicy
curPolicy cur = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \c -> Scripts.wrapMintingPolicy (checkPolicy c) ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode cur

{- note [Obtaining the currency symbol]

The currency symbol is the address (hash) of the validator. That is why
we can use 'Ledger.scriptAddress' here to get the symbol  in off-chain code,
for example in 'mintedValue'.

Inside the validator script (on-chain) we can't use 'Ledger.scriptAddress',
because at that point we don't know the hash of the script yet. That
is why we use 'V.ownCurrencySymbol', which obtains the hash from the
'PolicyCtx' value.

-}

-- | The 'Value' minted by the 'OneShotCurrency' contract
mintedValue :: OneShotCurrency -> Value
mintedValue cur = currencyValue (currencySymbol cur) cur

currencySymbol :: OneShotCurrency -> CurrencySymbol
currencySymbol = scriptCurrencySymbol . curPolicy

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
mintContract :: forall w s e. AsCurrencyError e => SimpleMPS -> Contract w s e OneShotCurrency
mintContract SimpleMPS{tokenName, amount} = mapError (review _CurrencyError) $ do
    pkh <- ownPubKeyHash
    txOutRef <- getUnspentOutput
    ciTxOut <- txOutFromRef txOutRef
    let utxos       = maybe Haskell.mempty (Data.Map.singleton txOutRef) ciTxOut
        theCurrency = mkCurrency txOutRef [(tokenName, amount)]
        curVali     = curPolicy theCurrency
        lookups     = Constraints.mintingPolicy curVali
                        <> Constraints.unspentOutputs utxos
        mintTx      = Constraints.mustSpendPubKeyOutput txOutRef
                        <> Constraints.mustMintValue (mintedValue theCurrency)
                        <> collateralConstraints pkh [lovelaceValueOf 10_000_000]
    logInfo @String $ show lookups
    logInfo @String $ show mintTx
    tx <- untilRight $ submitTxConstraintsWith @Scripts.Any lookups mintTx
    _ <- awaitTxConfirmed (getCardanoTxId tx)
    pure theCurrency

-- | Minting policy for a currency that has a fixed amount of tokens issued
--   in one transaction
data SimpleMPS =
    SimpleMPS
        { tokenName :: TokenName
        , amount    :: Integer
        }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (FromJSON, ToJSON, ToSchema)

type CurrencySchema = Endpoint "Create native token" SimpleMPS

-- | Use 'mintContract' to create the currency specified by a 'SimpleMPS'
mintCurrency :: Promise (Maybe (Last OneShotCurrency)) CurrencySchema CurrencyError OneShotCurrency
mintCurrency = endpoint @"Create native token" $ \simpleMPS -> do
    cur <- mintContract simpleMPS
    tell (Just (Last cur))
    pure cur