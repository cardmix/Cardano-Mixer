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
{-# LANGUAGE TypeSynonymInstances       #-}

module MixerScript (
    Mixer(..),
    MixerDatum(..),
    MixerRedeemer(..),
    mixerInst,
    mixerValidator,
    mixerValidatorHash,
    mixerAddress,
    makeMixerFromFees
) where

import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Typed.Scripts                     (TypedValidator, ValidatorTypes(..), mkTypedValidator, validatorScript, validatorHash, wrapValidator)
import           Plutus.V1.Ledger.Credential              (Credential(ScriptCredential))
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (Show (..))


import           Configuration.PABConfig                  (vestingScriptPermanentHash)
import           Contracts.Vesting                        (VestingParams(..))
import           Crypto

----------------------- Data types, instances, and constants -----------------------------

data Mixer = Mixer {
    mValue              :: !Value,            -- Mixing value
    mRelayerCollateral  :: !Value,            -- Relayer collateral
    mTotalFees          :: !Value             -- Total fees that relayer collects
}

PlutusTx.makeLift ''Mixer

makeMixerFromFees :: Value -> Mixer
makeMixerFromFees v = Mixer (scale 500 v) (scale 1000 v) v

newtype MixerDatum = MixerDatum { getMixerDatum :: Fr }
  deriving Show

PlutusTx.unstableMakeIsData ''MixerDatum

data MixerRedeemer = MixerRedeemer PaymentPubKeyHash (Integer, Integer) [Fr] Proof
    deriving Show

PlutusTx.unstableMakeIsData ''MixerRedeemer

data Mixing
instance ValidatorTypes Mixing where
  type instance DatumType Mixing = MixerDatum
  type instance RedeemerType Mixing = MixerRedeemer

------------------------------ Validator --------------------------------

hourPOSIX :: POSIXTime
hourPOSIX = POSIXTime 3600000

{-# INLINABLE mkMixerValidator #-}
mkMixerValidator :: Mixer -> MixerDatum -> MixerRedeemer -> ScriptContext -> Bool
mkMixerValidator mixer _ (MixerRedeemer pkhR _ _ _) ctx = txout `elem` outs
    where
        txinfo = scriptContextTxInfo ctx
        outs   = txInfoOutputs txinfo

        -- finding current time estimate
        date = case ivTo (txInfoValidRange txinfo) of
                UpperBound (Finite t) True -> t
                _                          -> error ()
        
        -- finding this input's datum hash
        (_, dhash) = ownHashes ctx

        -- constructing the desired TxOut
        addr   = Address (ScriptCredential vestingScriptPermanentHash) Nothing
        val    = mRelayerCollateral mixer
        d      = Datum $ toBuiltinData $ VestingParams (date + hourPOSIX) pkhR dhash
        dh     = fromMaybe (error ()) $ findDatumHash d txinfo -- we need a very specific datum hash in order to reverse invalid transaction
        txout  = TxOut addr val (Just dh)

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

