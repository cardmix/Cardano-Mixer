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


module Scripts.ADAWithdrawScript where

import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Typed.Scripts                     (TypedValidator, ValidatorTypes(..), mkTypedValidator, 
                                                            validatorScript, validatorHash, wrapValidator)
import           Ledger.Value                             (geq, flattenValue)
import           Plutus.V1.Ledger.Ada                     (fromValue, toValue)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

import           Scripts.Constraints                      (utxoProducedScriptTx, utxoSpentScriptTx)
import           Types.TxConstructor                      (TxConstructor (..))

--------------------------------------- On-Chain ------------------------------------------

type ADAWithdrawDatum = ()

type ADAWithdrawRedeemer = ()

data ADAWithdrawing
instance ValidatorTypes ADAWithdrawing where
  type instance DatumType ADAWithdrawing = ADAWithdrawDatum
  type instance RedeemerType ADAWithdrawing = ADAWithdrawRedeemer

{-# INLINABLE mkADAWithdrawValidator #-}
mkADAWithdrawValidator :: () -> () -> ScriptContext -> Bool
mkADAWithdrawValidator _ _ ctx@ScriptContext{scriptContextTxInfo=info} = nonADAFlowVal `geq` zero
  where
    ownAddr       = maybe (error ()) (txOutAddress . txInInfoResolved) (findOwnInput ctx)
    inVal         = sum $ map txOutValue $ filter (\o -> txOutAddress o == ownAddr) $ map txInInfoResolved $ txInfoInputs info
    outVal        = sum $ map txOutValue $ filter (\o -> txOutAddress o == ownAddr) $ txInfoOutputs info
    flowVal       = inVal - outVal
    nonADAFlowVal = flowVal - toValue (fromValue flowVal)

adaWithdrawInst :: TypedValidator ADAWithdrawing
adaWithdrawInst = mkTypedValidator @ADAWithdrawing
    $$(PlutusTx.compile [|| mkADAWithdrawValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator @ADAWithdrawDatum @ADAWithdrawRedeemer

------------------------------------ Off-chain --------------------------------------------

adaWithdrawValidator :: Validator
adaWithdrawValidator = validatorScript adaWithdrawInst

adaWithdrawValidatorHash :: ValidatorHash
adaWithdrawValidatorHash = validatorHash adaWithdrawInst

adaWithdrawAddress :: Address
adaWithdrawAddress = scriptAddress adaWithdrawValidator

payToADAWithdrawScriptTx :: Value -> TxConstructor a i o -> TxConstructor a i o
payToADAWithdrawScriptTx v = utxoProducedScriptTx adaWithdrawValidatorHash Nothing v () .
    utxoSpentScriptTx f (const . const adaWithdrawValidator) (const . const ())
  where
    f _ o = length (flattenValue $ _ciTxOutValue o) <= 16
