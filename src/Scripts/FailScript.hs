{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module Scripts.FailScript where

import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Typed.Scripts                     (TypedValidator, ValidatorTypes(..), mkTypedValidator, validatorScript, validatorHash, wrapValidator)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

import           Scripts.Constraints                      (utxoProducedScriptTx)
import           Types.TxConstructor                      (TxConstructor (..))

--------------------------------------- On-Chain ------------------------------------------

type FailDatum = ()

type FailRedeemer = ()

data Failing
instance ValidatorTypes Failing where
  type instance DatumType Failing = FailDatum
  type instance RedeemerType Failing = FailRedeemer

{-# INLINABLE mkFailValidator #-}
mkFailValidator :: () -> () -> ScriptContext -> Bool
mkFailValidator _ _ _ = False

failInst :: TypedValidator Failing
failInst = mkTypedValidator @Failing
    $$(PlutusTx.compile [|| mkFailValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator @FailDatum @FailRedeemer

------------------------------------ Off-chain ---------------------------------------------

failValidator :: Validator
failValidator = validatorScript failInst

failValidatorHash :: ValidatorHash
failValidatorHash = validatorHash failInst

failAddress :: Address
failAddress = scriptAddress failValidator

payToFailScriptTx :: Value -> TxConstructor a i o -> TxConstructor a i o
payToFailScriptTx v = utxoProducedScriptTx failValidatorHash Nothing v ()
