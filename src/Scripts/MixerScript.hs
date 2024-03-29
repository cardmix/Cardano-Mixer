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


module Scripts.MixerScript where

import           Control.Monad.State                      (State)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Tokens                            (token)
import           Ledger.Typed.Scripts                     (TypedValidator, ValidatorTypes(..), mkTypedValidator, mkUntypedValidator,
                                                            validatorScript, validatorHash, validatorAddress)
import           Ledger.Value                             (AssetClass(..), geq)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

import           Scripts.Constraints                      (utxoSpentScriptTx, utxoProducedScriptTx)
import           Tokens.WithdrawToken                     (withdrawTokenName)
import           Types.Mixer                              (mixerValueBeforeDeposit)
import           Types.MixerInput                         (WithdrawTokenNameParams)
import           Types.MixerInstance                      (MixerInstance (..))
import           Types.TxConstructor                      (TxConstructor)

--------------------------------------- On-Chain ------------------------------------------

type MixerParams = CurrencySymbol

type MixerDatum = ()

type MixerRedeemer = WithdrawTokenNameParams

data Mixing
instance ValidatorTypes Mixing where
  type instance DatumType Mixing = MixerDatum
  type instance RedeemerType Mixing = MixerRedeemer

{-# INLINABLE mkMixerValidator #-}
mkMixerValidator :: MixerParams -> MixerDatum -> MixerRedeemer -> ScriptContext -> Bool
mkMixerValidator wSymb _ wTNParams ScriptContext{scriptContextTxInfo=info} = cond
    where
        vWD = token $ AssetClass (wSymb, withdrawTokenName wTNParams)
        cond = txInfoMint info `geq` vWD

-- Validator instance
mixerTypedValidator :: MixerParams -> TypedValidator Mixing
mixerTypedValidator params = mkTypedValidator @Mixing
    ($$(PlutusTx.compile [|| mkMixerValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode params)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator @MixerDatum @MixerRedeemer

---------------------------------------- Off-chain -------------------------------------------

mixerValidator :: MixerParams -> Validator
mixerValidator = validatorScript . mixerTypedValidator

mixerValidatorHash :: MixerParams -> ValidatorHash
mixerValidatorHash = validatorHash . mixerTypedValidator

mixerAddress :: MixerParams -> Address
mixerAddress = validatorAddress . mixerTypedValidator

payToMixerScriptTx :: MixerInstance -> State (TxConstructor d a i o) ()
payToMixerScriptTx mi = utxoProducedScriptTx val Nothing v ()
  where val = mixerValidatorHash        $ miWithdrawCurrencySymbol mi
        v   = mixerValueBeforeDeposit   $ miMixer  mi

withdrawFromMixerScriptTx :: MixerInstance -> MixerRedeemer -> State (TxConstructor d a i o) (Maybe (TxOutRef, ChainIndexTxOut))
withdrawFromMixerScriptTx mi red = do
  let f _ o = _ciTxOutValue o `geq` v && either (const False) (== Datum (toBuiltinData ())) (_ciTxOutDatum o)
  utxoSpentScriptTx f (const . const val) (const . const red)
  where val = mixerValidator            $ miWithdrawCurrencySymbol mi
        v   = mixerValueBeforeDeposit   $ miMixer  mi