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


module MixerScript (
    Mixer(..),
    mixerInst,
    mixerValidator,
    mixerValidatorHash,
    mixerAddress,
    makeMixerFromFees,
    hourPOSIX
) where

import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Typed.Scripts                     (TypedValidator, ValidatorTypes(..), mkTypedValidator, validatorScript, validatorHash, wrapValidator)
import           Plutus.V1.Ledger.Ada                     (lovelaceValueOf)
import           Plutus.V1.Ledger.Credential              (Credential(..))
import           Plutus.V1.Ledger.Value                   (geq)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

import           Configuration.PABConfig                  (vestingScriptPermanentHash)
import           Contracts.Vesting                        (VestingParams(..))


----------------------- Data types, instances, and constants -----------------------------

data Mixer = Mixer {
    mValue              :: !Value,            -- Mixing value
    mRelayerCollateral  :: !Value,            -- Relayer collateral
    mTotalFees          :: !Value             -- Total fees that relayer collects
}

PlutusTx.makeLift ''Mixer

mixerFixedFee :: Value
mixerFixedFee = lovelaceValueOf 3_000_000

makeMixerFromFees :: Value -> Mixer
makeMixerFromFees v = Mixer (scale 500 v) (scale 1000 v) (v + mixerFixedFee)

type MixerDatum = ()
type MixerRedeemer = ()

data Mixing
instance ValidatorTypes Mixing where
  type instance DatumType Mixing = MixerDatum
  type instance RedeemerType Mixing = MixerRedeemer

------------------------------ Validator --------------------------------

hourPOSIX :: POSIXTime
hourPOSIX = POSIXTime 3600000

{-# INLINABLE mkMixerValidator #-}
mkMixerValidator :: Mixer -> MixerDatum -> MixerRedeemer -> ScriptContext -> Bool
mkMixerValidator mixer _ _ ctx = vestingOK && paymentOK
    where
        txinfo = scriptContextTxInfo ctx
        outs   = txInfoOutputs txinfo
        outs'  = head $ filter (\o -> (txOutValue o `geq` mRelayerCollateral mixer) &&
            (txOutAddress o == Address (ScriptCredential vestingScriptPermanentHash) Nothing)) outs

        VestingParams vTime _ pkhW ref _ _ = unsafeFromBuiltinData $ getDatum $ fromMaybe (error ()) $
            findDatum (fromMaybe (error ()) $ txOutDatumHash outs') txinfo

        -- finding current time estimate
        dateOK = case ivTo (txInfoValidRange txinfo) of
                  UpperBound (Finite t) True -> vTime >= t + hourPOSIX
                  _                          -> False

        -- finding this input's TxOutRef
        ownRef = txInInfoOutRef $ fromMaybe (error ()) $ findOwnInput ctx

        vestingOK = (ownRef == ref) && dateOK
        paymentOK = any (\o -> (txOutValue o `geq` mValue mixer) &&
            (txOutAddress o == pubKeyHashAddress pkhW Nothing)) outs

-- Validator instance
mixerInst :: Mixer -> TypedValidator Mixing
mixerInst mixer = mkTypedValidator @Mixing
    ($$(PlutusTx.compile [|| mkMixerValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode mixer)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator @MixerDatum @MixerRedeemer

-- Validator script
mixerValidator :: Mixer -> Validator
mixerValidator = validatorScript . mixerInst

-- Validator Hash
mixerValidatorHash :: Mixer -> ValidatorHash
mixerValidatorHash = validatorHash . mixerInst

-- Validator address
mixerAddress :: Mixer -> Address
mixerAddress = scriptAddress . mixerValidator

----------------------------- Off-chain --------------------------------

-- getRelayTicketUTXOs :: AsContractError e => Mixer -> Contract w s e (Map TxOutRef ChainIndexTxOut)
-- getRelayTicketUTXOs mixer = do
--     utxos <- utxosAt (mixerAddress mixer)
--     return $ Data.Map.filter (\o -> f o == Just zero) utxos
--   where f o = do
--             d <- either (const Nothing) Just $ _ciTxOutDatum o
--             m <- fromBuiltinData $ getDatum d
--             return $ getMixerDatum m