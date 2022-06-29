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
import           Prelude                                  (undefined)

import           Types.TxConstructor                      (TxConstructor (..))

----------------------- Data types, instances, and constants -----------------------------

type FailDatum = ()
type FailRedeemer = ()

data Failing
instance ValidatorTypes Failing where
  type instance DatumType Failing = FailDatum
  type instance RedeemerType Failing = FailRedeemer

------------------------------ Validator --------------------------------

-- The script must:
-- 1) always fail
{-# INLINABLE mkFailValidator #-}
mkFailValidator :: () -> () -> ScriptContext -> Bool
mkFailValidator _ _ _ = False

-- Validator instance
failInst :: TypedValidator Failing
failInst = mkTypedValidator @Failing
    $$(PlutusTx.compile [|| mkFailValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator @FailDatum @FailRedeemer

-- Validator script
failValidator :: Validator
failValidator = validatorScript failInst

-- Validator Hash
failValidatorHash :: ValidatorHash
failValidatorHash = validatorHash failInst

-- Validator address
failAddress :: Address
failAddress = scriptAddress failValidator

----------------------------- Off-chain --------------------------------

payToFailScriptTx :: Value -> TxConstructor a i o -> TxConstructor a i o
payToFailScriptTx _ _ = undefined
