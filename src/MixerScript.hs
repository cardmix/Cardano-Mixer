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
    MixerDatum(..),
    MixerRedeemer(..),
    mixerInst,
    mixerValidator,
    mixerValidatorHash,
    mixerAddress,
    makeMixerFromFees,
    hourPOSIX
) where

import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Typed.Scripts                     (TypedValidator, ValidatorTypes(..), mkTypedValidator, validatorScript, validatorHash, wrapValidator)
import           Plutus.V1.Ledger.Credential              (Credential(ScriptCredential))
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (Show (..), (<$>))


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

PlutusTx.unstableMakeIsData ''Zp
PlutusTx.unstableMakeIsData ''R
PlutusTx.unstableMakeIsData ''Q
PlutusTx.unstableMakeIsData ''Proof
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

---------------------------- For PlutusTx ------------------------------

instance ToData t => ToData (Extension t e) where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData (E (P a)) = toBuiltinData a

instance FromData t => FromData (Extension t e) where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData i = E . P <$> fromBuiltinData i

instance UnsafeFromData t => UnsafeFromData (Extension t e) where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData i = E $ P $ unsafeFromBuiltinData i

instance (ToData t) => ToData (Polynomial t) where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData (P a) = toBuiltinData a

instance (FromData t) => FromData (Polynomial t) where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData i = P <$> fromBuiltinData i

instance (UnsafeFromData t) => UnsafeFromData (Polynomial t) where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData i = P $ unsafeFromBuiltinData i

instance (ToData t, Ring t) => ToData (CurvePoint t) where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData O        = toBuiltinData (False, (zero :: t, zero :: t))
    toBuiltinData (CP x y) = toBuiltinData (True,  (x,    y))

instance FromData t => FromData (CurvePoint t) where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData i = case fromBuiltinData i of
      Just (b, (x, y)) -> if b then Just $ CP x y else Just O
      Nothing          -> Nothing

instance UnsafeFromData t => UnsafeFromData (CurvePoint t) where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData i = if b then CP x y else O
      where (b, (x, y)) = unsafeFromBuiltinData i