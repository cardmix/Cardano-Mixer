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

import           Control.Monad.State                      (State)
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
import           Types.Mixer                              (mixerValueBeforeDeposit)
import           Types.MixerInstance                      (MixerInstance (..))
import           Types.TxConstructor                      (TxConstructor)

--------------------------------------- On-Chain ------------------------------------------

type MixerDepositParams = CurrencySymbol

type MixerDepositDatum = Fr

type MixerDepositRedeemer = ()

data MixerDepositing
instance ValidatorTypes MixerDepositing where
  type instance DatumType MixerDepositing = MixerDepositDatum
  type instance RedeemerType MixerDepositing = MixerDepositRedeemer

{-# INLINABLE mkMixerDepositValidator #-}
mkMixerDepositValidator :: MixerDepositParams -> MixerDepositDatum -> MixerDepositRedeemer -> ScriptContext -> Bool
mkMixerDepositValidator dSymb leaf _ ScriptContext{scriptContextTxInfo=info} = cond
    where
        vDep = token $ AssetClass (dSymb, depositTokenName leaf)
        cond = txInfoMint info `geq` vDep

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

payToMixerDepositScriptTx :: MixerInstance -> MixerDepositDatum -> State (TxConstructor d a i o) ()
payToMixerDepositScriptTx mi = utxoProducedScriptTx vh Nothing v
  where vh = mixerDepositValidatorHash $ miDepositCurrencySymbol mi
        v  = mixerValueBeforeDeposit   $ miMixer  mi

withdrawFromMixerDepositScriptTx :: MixerInstance -> State (TxConstructor d a i o) (Maybe MixerDepositDatum)
withdrawFromMixerDepositScriptTx mi = do
  let g     = (fromBuiltinData :: BuiltinData -> Maybe Fr) . getDatum
      f _ o = _ciTxOutValue o `geq` v && either (const False) (isJust . g) (_ciTxOutDatum o)
  res <- utxoSpentScriptTx f (const . const val) (const . const ())
  case res of
    Just (_, ciTxOut) -> return $ either (const Nothing) g (_ciTxOutDatum ciTxOut)
    _                 -> return Nothing
  where val = mixerDepositValidator $ miDepositCurrencySymbol mi
        v   = mixerValueBeforeDeposit   $ miMixer  mi