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
module Tokens.OneShotCurrency (
    OneShotCurrencyParams(..),
    oneShotCurrencyPolicy,
    -- * Actions etc
    mkCurrency,
    currencySymbol,
    currencyValue,    
    oneShotCurrencyMintTx
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

import           Scripts.Constraints             (tokensMintedTx, utxoSpentPublicKeyTx)
import           Types.TxConstructor             (TxConstructor)


{- HLINT ignore "Use uncurry" -}

-------------------------------- On-Chain ------------------------------------

data OneShotCurrencyParams = OneShotCurrencyParams
    {
        curRef         :: TxOutRef,
        curAmounts     :: AssocMap.Map TokenName Integer
    }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''OneShotCurrencyParams

oneShotCurrencyValue :: CurrencySymbol -> OneShotCurrencyParams -> Value
oneShotCurrencyValue s OneShotCurrencyParams{curAmounts = amts} =
    let
        values = map (\(tn, i) -> Value.singleton s tn i) (AssocMap.toList amts)
    in fold values

checkPolicy :: OneShotCurrencyParams -> () -> V.ScriptContext -> Bool
checkPolicy c@(OneShotCurrencyParams (TxOutRef refHash refIdx) _) _ ctx@V.ScriptContext{V.scriptContextTxInfo=txinfo} =
    let
        -- see note [Obtaining the currency symbol]
        ownSymbol = V.ownCurrencySymbol ctx

        minted = V.txInfoMint txinfo
        expected = oneShotCurrencyValue ownSymbol c

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

oneShotCurrencyPolicy :: OneShotCurrencyParams -> MintingPolicy
oneShotCurrencyPolicy cur = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode cur

-------------------------------- Off-Chain -----------------------------------

mkCurrency :: TxOutRef -> [(TokenName, Integer)] -> OneShotCurrencyParams
mkCurrency ref amts =
    OneShotCurrencyParams
        { curRef     = ref
        , curAmounts = AssocMap.fromList amts
        }

currencySymbol :: OneShotCurrencyParams -> CurrencySymbol
currencySymbol = scriptCurrencySymbol . oneShotCurrencyPolicy

currencyValue :: OneShotCurrencyParams -> Value
currencyValue cur = oneShotCurrencyValue (currencySymbol cur) cur

-- TODO: add spending a TxOutRef here!!
-- Constraints that the OneShotCurrency is minted in the transaction
oneShotCurrencyMintTx :: OneShotCurrencyParams -> TxConstructor a i o -> TxConstructor a i o
oneShotCurrencyMintTx par@(OneShotCurrencyParams ref _) = tokensMintedTx (oneShotCurrencyPolicy par) () (currencyValue par) .
    utxoSpentPublicKeyTx (\r _ -> r == ref)