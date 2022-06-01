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
module Scripts.CurrencyScript (
      OneShotCurrency(..)
    , curPolicy
    -- * Actions etc
    , mkCurrency
    , mintedValue
    , currencySymbol
    ) where

import           Data.Aeson                      (FromJSON, ToJSON)
import           GHC.Generics                    (Generic)
import           Ledger                          (CurrencySymbol, TxOutRef (..), scriptCurrencySymbol)
import qualified Ledger.Contexts                 as V
import           Ledger.Scripts
import qualified Ledger.Typed.Scripts            as Scripts
import           Ledger.Value                    (TokenName, Value)
import qualified Ledger.Value                    as Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap               as AssocMap
import           PlutusTx.Prelude                hiding (Monoid (..), Semigroup (..))
import qualified Prelude                         as Haskell

{- HLINT ignore "Use uncurry" -}

-- | A currency that can be created exactly once
data OneShotCurrency = OneShotCurrency
  { curRefTransactionOutput :: TxOutRef
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
mkCurrency ref amts =
    OneShotCurrency
        { curRefTransactionOutput = ref
        , curAmounts              = AssocMap.fromList amts
        }

checkPolicy :: OneShotCurrency -> () -> V.ScriptContext -> Bool
checkPolicy c@(OneShotCurrency (TxOutRef refHash refIdx) _) _ ctx@V.ScriptContext{V.scriptContextTxInfo=txinfo} =
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
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . checkPolicy ||])
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