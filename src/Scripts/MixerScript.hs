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

import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Typed.Scripts                     (TypedValidator, ValidatorTypes(..), mkTypedValidator,
                                                            validatorScript, validatorHash, wrapValidator, validatorAddress)
import           Plutus.V1.Ledger.Ada                     (lovelaceValueOf)
import           Plutus.V1.Ledger.Api                     (Credential(..))
import           Plutus.V1.Ledger.Value                   (geq)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

import           Scripts.Constraints                      (getUpperTimeEstimate, checkDatum, findUtxoProduced)

----------------------- Data types, instances, and constants -----------------------------

data Mixer = Mixer {
    mFee                 :: !Value,
    mDepositVestingHash  :: !ValidatorHash,    -- ValidatorHash of the deposit vesting script
    mWithdrawVestingHash :: !ValidatorHash,    -- ValidatorHash of the withdraw vesting script
    mFailAddress         :: !Address
}

PlutusTx.makeLift ''Mixer

mixerFixedFee :: Value
mixerFixedFee = lovelaceValueOf 1_500_000

mixerPureValue :: Mixer -> Value
mixerPureValue (Mixer v _ _ _) = scale 500 v

mixerDepositValue :: Mixer -> Value
mixerDepositValue (Mixer v _ _ _) = scale 500 v + v + mixerFixedFee

mixerCollateral :: Value
mixerCollateral = lovelaceValueOf 100_000_000

mixerVestingValue :: Value -> Value
mixerVestingValue v = scale 2 v + mixerCollateral

mixerDepositVestingAddress :: Mixer -> Address
mixerDepositVestingAddress (Mixer _ vh _ _) = Address (ScriptCredential vh) Nothing

mixerWithdrawVestingAddress :: Mixer -> Address
mixerWithdrawVestingAddress (Mixer _ _ vh _) = Address (ScriptCredential vh) Nothing

-- TODO: this should be moved to config
vestingDuration :: POSIXTime
vestingDuration = POSIXTime 3_600_000

type MixerDatum = ()
type MixerRedeemer = ()

data Mixing
instance ValidatorTypes Mixing where
  type instance DatumType Mixing = MixerDatum
  type instance RedeemerType Mixing = MixerRedeemer

------------------------------ Validator --------------------------------

-- The script must:
-- 1) produce output in the vesting script with the value greater or equal then in the own input and with correct vesting time
{-# INLINABLE mkMixerValidator #-}
mkMixerValidator :: Mixer -> MixerDatum -> MixerRedeemer -> ScriptContext -> Bool
mkMixerValidator mixer _ _ ctx@ScriptContext{scriptContextTxInfo=info} = cond
  where
    ownValue = txOutValue $ txInInfoResolved $ fromMaybe (error ()) $ findOwnInput ctx
    vMixer   = mixerDepositValue mixer
    vTime    = ownValue - vMixer
    ct       = getUpperTimeEstimate info
    f o      = txOutValue o `geq` (mixerVestingValue vMixer + vTime) && txOutAddress o == mixerWithdrawVestingAddress mixer
    g (t, _) = t == ct + vestingDuration

    cond     = checkDatum info (g :: (POSIXTime, PaymentPubKeyHash) -> Bool) $ findUtxoProduced info f

-- Validator instance
mixerTypedValidator :: Mixer -> TypedValidator Mixing
mixerTypedValidator mixer = mkTypedValidator @Mixing
    ($$(PlutusTx.compile [|| mkMixerValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode mixer)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator @MixerDatum @MixerRedeemer

-- Validator script
mixerValidator :: Mixer -> Validator
mixerValidator = validatorScript . mixerTypedValidator

-- Validator Hash
mixerValidatorHash :: Mixer -> ValidatorHash
mixerValidatorHash = validatorHash . mixerTypedValidator

-- Validator address
mixerAddress :: Mixer -> Address
mixerAddress = validatorAddress . mixerTypedValidator

----------------------------- Off-chain --------------------------------

-- getRelayTicketUTXOs :: AsContractError e => Mixer -> Contract w s e (Map TxOutRef ChainIndexTxOut)
-- getRelayTicketUTXOs mixer = do
--     utxos <- utxosAt (mixerAddress mixer)
--     return $ Data.Map.filter (\o -> f o == Just zero) utxos
--   where f o = do
--             d <- either (const Nothing) Just $ _ciTxOutDatum o
--             m <- fromBuiltinData $ getDatum d
--             return $ getMixerDatum m
