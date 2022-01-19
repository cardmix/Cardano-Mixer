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

module MixerScript (
    Mixer(..),
    MixerDatum(..),
    MixerRedeemer(..),
    mixerInst,
    mixerValidator,
    mixerValidatorHash,
    mixerAddress,
    mapError',
    verifierTokenCurrency,
    verifierTokenName,
    verifierTokenAssetClass
) where

import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Constraints.OnChain               (checkTxConstraint)
import           Ledger.Constraints.TxConstraints
import           Ledger.Tokens                            (token)
import           Ledger.Typed.Scripts                     (TypedValidator, ValidatorTypes(..), mkTypedValidator, validatorScript, validatorHash, wrapValidator)
import           Ledger.Value                             (AssetClass(..), CurrencySymbol(..), TokenName(..))
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (Show (..))


import           Configuration.PABConfig                  (vestingScriptPermanentHash)
import           Contracts.Vesting                        (VestingParams(..))
import           Crypto
import           Utility                                  (mapError')


----------------------- Data types, instances, and constants -----------------------------

data Mixer = Mixer {
    mValue              :: !Value,            -- Mixing value
    mRelayerToken       :: !Value             -- Relayer token
}

PlutusTx.makeLift ''Mixer

-- Mixer verifier token
verifierTokenCurrency :: CurrencySymbol
verifierTokenCurrency = CurrencySymbol "9724bb"

verifierTokenName :: TokenName
verifierTokenName = TokenName "vMIX"

verifierTokenAssetClass :: AssetClass
verifierTokenAssetClass = AssetClass (verifierTokenCurrency, verifierTokenName)

verifierNFT :: Value
verifierNFT = token verifierTokenAssetClass

newtype MixerDatum = MixerDatum { getMixerDatum :: Fr }
  deriving Show

PlutusTx.unstableMakeIsData ''MixerDatum

data MixerRedeemer = MixerRedeemer PaymentPubKeyHash (Integer, Integer) PaymentPubKeyHash [Fr] Proof
    deriving Show

PlutusTx.unstableMakeIsData ''MixerRedeemer

data Mixing
instance ValidatorTypes Mixing where
  type instance DatumType Mixing = MixerDatum
  type instance RedeemerType Mixing = MixerRedeemer

------------------------------ Validator --------------------------------

{-# INLINABLE mkMixerValidator #-}
mkMixerValidator :: Mixer -> MixerDatum -> MixerRedeemer -> ScriptContext -> Bool
mkMixerValidator mixer _ (MixerRedeemer pkhR _ pkhW _ _) ctx =
      checkTxConstraint ctx txc1 &&
       checkTxConstraint ctx txc2
    where
        -- Must relay to the recipient
        txc1 = MustPayToPubKeyAddress pkhW Nothing Nothing (mValue mixer)
        -- Must time lock the relayer token
        date = case ivTo (txInfoValidRange $ scriptContextTxInfo ctx) of
                UpperBound (Finite d) True -> d
                _                          -> error ()
        hourPOSIX = POSIXTime 3600000  -- TODO: Fix the issue with multiple script utxos
        (_, dhash) = ownHashes ctx
        txc2 = MustPayToOtherScript vestingScriptPermanentHash (Datum $ toBuiltinData $ VestingParams (date + hourPOSIX) pkhR dhash) (mRelayerToken mixer)

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

