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


module Scripts.MixerDepositScript where

import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Tokens                            (token)
import           Ledger.Typed.Scripts                     (TypedValidator, ValidatorTypes(..), mkTypedValidator,
                                                            validatorScript, validatorHash, wrapValidator, validatorAddress)
import           Ledger.Value                             (AssetClass(..), geq)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

import           Crypto
import           Scripts.Constraints                      (utxoProducedScriptTx, utxoSpentScriptTx)
import           Tokens.DepositToken                      (depositTokenName)
import           Types.TxConstructor                      (TxConstructor)

--------------------------------------- On-Chain ------------------------------------------

type MixerDepositParams = CurrencySymbol

type MixerDepositDatum = Fr

type MixerDepositRedeemer = Fr

data MixerDepositing
instance ValidatorTypes MixerDepositing where
  type instance DatumType MixerDepositing = MixerDepositDatum
  type instance RedeemerType MixerDepositing = MixerDepositRedeemer

{-# INLINABLE mkMixerDepositValidator #-}
mkMixerDepositValidator :: MixerDepositParams -> MixerDepositDatum -> MixerDepositRedeemer -> ScriptContext -> Bool
mkMixerDepositValidator dSymb oldLeaf newLeaf ScriptContext{scriptContextTxInfo=info} = cond1 && cond2
    where
        vDep = token $ AssetClass (dSymb, depositTokenName newLeaf)

        cond1 = txInfoMint info `geq` vDep
        cond2 = oldLeaf == newLeaf

mixerDepositTypedValidator :: MixerDepositParams -> TypedValidator MixerDepositing
mixerDepositTypedValidator params = mkTypedValidator @MixerDepositing
    ($$(PlutusTx.compile [|| mkMixerDepositValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode params)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator @MixerDepositDatum @MixerDepositRedeemer

---------------------------------------- Off-chain -------------------------------------------

mixerDepositValidator :: MixerDepositParams -> Validator
mixerDepositValidator = validatorScript . mixerDepositTypedValidator

mixerDepositValidatorHash :: MixerDepositParams -> ValidatorHash
mixerDepositValidatorHash = validatorHash . mixerDepositTypedValidator

mixerDepositAddress :: MixerDepositParams -> Address
mixerDepositAddress = validatorAddress . mixerDepositTypedValidator

payToMixerDepositScriptTx :: Value -> MixerDepositDatum -> MixerDepositParams -> TxConstructor a i o -> TxConstructor a i o
payToMixerDepositScriptTx v leaf par = utxoProducedScriptTx (mixerDepositValidatorHash par) Nothing v leaf

withdrawFromMixerDepositScriptTx :: Value -> MixerDepositDatum -> MixerDepositParams -> TxConstructor a i o -> TxConstructor a i o
withdrawFromMixerDepositScriptTx v leaf par = utxoSpentScriptTx f (const . const $ mixerDepositValidator par) (const . const leaf)
  where
    f _ o = _ciTxOutValue o `geq` v && either (const False) (== Datum (toBuiltinData leaf)) (_ciTxOutDatum o)