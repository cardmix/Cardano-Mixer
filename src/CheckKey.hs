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
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module CheckKey (CheckKeyState(..), checkKeyValidator, checkKeyValidatorHash, checkKeyRequired, checkKeyTx, checkKeyAddress) where

import           Data.Either                       (fromRight)
import           Data.Map                          (filter, elems, null, keys)
import           Ledger                            hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Constraints                (ScriptLookups(..), unspentOutputs, otherScript)
import           Ledger.Constraints.TxConstraints  (TxConstraints, mustPayToOtherScript, mustSpendScriptOutput)
import           Ledger.Typed.Scripts.Validators
import           Ledger.Value                      (TokenName(..), geq)
import           Plutus.Contract                   (Contract, AsContractError, utxosAt)
import           Plutus.V1.Ledger.Ada              (toValue)
import           PlutusTx
import           PlutusTx.Prelude                  hiding (Semigroup(..), unless, mapMaybe, find, toList, fromInteger, check, null)
import           Prelude                           (Show (..), (<>))

import           Crypto


----------------------- Data types, instances, and constants -----------------------------

checkKeyTokenName :: TokenName
checkKeyTokenName = TokenName "Cardano Mixer CheckKey Token"

-- CheckKey state
data CheckKeyState = CheckKeyState
  {
    curKey  :: Fr,
    nextKey :: Fr
  }
  deriving Show

PlutusTx.unstableMakeIsData ''CheckKeyState

data CheckingKey
instance ValidatorTypes CheckingKey where
  type instance DatumType CheckingKey = CheckKeyState
  type instance RedeemerType CheckingKey = ()

--------------------------------- Validator --------------------------------

{-# INLINABLE checkKeyPolicy #-}
checkKeyPolicy :: Value -> CheckKeyState -> () -> ScriptContext -> Bool
checkKeyPolicy mt _ () ctx = mixerOK
    where
        txinfo = scriptContextTxInfo ctx

        -- mt will be the Value of mixer ThreadToken
        mixerOK = valueSpent txinfo `geq` mt

-- Validator instance
{-# INLINABLE checkKeyInst #-}
checkKeyInst :: Value -> TypedValidator CheckingKey
checkKeyInst mt = mkTypedValidator @CheckingKey
    ($$(PlutusTx.compile [|| checkKeyPolicy ||])
    `PlutusTx.applyCode` PlutusTx.liftCode mt)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator @CheckKeyState @()

-- Validator script
{-# INLINABLE checkKeyValidator #-}
checkKeyValidator :: Value -> Validator
checkKeyValidator = validatorScript . checkKeyInst

-- Validator Hash
{-# INLINABLE checkKeyValidatorHash #-}
checkKeyValidatorHash :: Value -> ValidatorHash
checkKeyValidatorHash = validatorHash . checkKeyInst

-- Validator address
{-# INLINABLE checkKeyAddress #-}
checkKeyAddress :: Value -> Address
checkKeyAddress = scriptAddress . checkKeyValidator

-------------------------- CheckKey required to run the Script -----------------------------

-- Here we do not check that mKey and iKey are consistent with the mixer
{-# INLINABLE checkKeyRequired #-}
checkKeyRequired :: Value -> state -> input -> ScriptContext -> Bool
checkKeyRequired ct _ _ ctx = outputsOK              -- mt is the mixer's ThreadToken Value, ct is the CheckToken Value
    where
        txinfo   = scriptContextTxInfo ctx
        mixerHash = ownHash ctx

        ins      = map txInInfoResolved $ txInfoInputs txinfo
        -- Filtering CheckKey input
        ins'     = PlutusTx.Prelude.filter (\o -> txOutValue o `geq` ct && txOutAddress o /= scriptHashAddress mixerHash) ins

        -- Getting Datum at the unmodified key script
        CheckKeyState cKey nKey = case txOutDatumHash <$> ins' of
                  [Just dh] -> f dh
                  _ -> CheckKeyState zero zero

        -- Filtering CheckKey outputs
        outs  = txInfoOutputs txinfo
        outs' = PlutusTx.Prelude.filter (\o -> txOutValue o `geq` ct && txOutAddress o /= scriptHashAddress mixerHash) outs
        (CheckKeyState cKey1 nKey1, CheckKeyState cKey2 nKey2) = case map txOutDatumHash outs' of
          [Just dh1, Just dh2] -> (f dh1, f dh2)
          _                    -> (CheckKeyState zero zero, CheckKeyState zero zero)

        (cKey', nKey', cKey'', nKey'') = if cKey1 < cKey2 then (cKey1, nKey1, cKey2, nKey2) else (cKey2, nKey2, cKey1, nKey1)

        outputsOK = (cKey' < nKey') && (nKey' == cKey'') && (cKey'' < nKey'') && -- expected outputs
                      (cKey == cKey') && (nKey == nKey'')                        -- correspond to inputs

        f dh = case findDatum dh txinfo of
                    Just (Datum d)  -> unsafeFromBuiltinData d
                    Nothing         -> CheckKeyState zero zero

------------------- Check Key transaction Lookups and Constraints ---------------------------

-- ScriptLookups and TxConstraints for CheckKey validator
checkKeyTx :: (AsContractError e) => Value -> Value -> Fr -> Contract w s e (ScriptLookups a, TxConstraints i o)
checkKeyTx mt ct key = do
    utxos <- utxosAt $ checkKeyAddress mt
    let utxos'  = Data.Map.filter (\o -> _ciTxOutValue o `geq` ct) utxos
        utxos'' = Data.Map.filter (fst . checkKeyInequality key) utxos'
        (cKey, nKey) = if null utxos'' then (zero, zero) else snd $ checkKeyInequality key $ head $ elems utxos''
        lookups = unspentOutputs utxos'' <> otherScript (checkKeyValidator mt)
        cons    = mustSpendScriptOutput (head $ keys utxos'') (Redeemer $ toBuiltinData ()) <>
                  mustPayToOtherScript (checkKeyValidatorHash mt) (Datum $ toBuiltinData (CheckKeyState cKey key)) (ct <> toValue minAdaTxOut) <>
                  mustPayToOtherScript (checkKeyValidatorHash mt) (Datum $ toBuiltinData (CheckKeyState key nKey)) (ct <> toValue minAdaTxOut)
    return (lookups, cons)

checkKeyInequality :: Fr -> ChainIndexTxOut -> (Bool, (Fr, Fr))
checkKeyInequality iKey o =
  let Datum d = fromRight (Datum $ toBuiltinData (CheckKeyState zero zero)) (_ciTxOutDatum o)
      CheckKeyState cKey nKey = unsafeFromBuiltinData d
  in (cKey < iKey && iKey < nKey, (cKey, nKey))