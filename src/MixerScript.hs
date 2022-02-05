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
    hourPOSIX,
    getRelayTicketUTXOs
) where

import           Data.Map                                 (Map)
import qualified Data.Map
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Typed.Scripts                     (TypedValidator, ValidatorTypes(..), mkTypedValidator, validatorScript, validatorHash, wrapValidator)
import           Plutus.Contract                          (Contract, AsContractError, utxosAt)
import           Plutus.V1.Ledger.Credential              (Credential(..))
import           Plutus.V1.Ledger.Value                   (geq)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (Show (..))

import           Configuration.PABConfig                  (vestingScriptPermanentHash)
import           Contracts.Vesting                        (VestingParams(..), VestingData (VestingData))
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

data MixerRedeemer = Withdraw | PayRelayTicket
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
mkMixerValidator mixer _ Withdraw ctx = vestingOK && paymentOK --txout `elem` outs
    where
        txinfo = scriptContextTxInfo ctx
        outs   = txInfoOutputs txinfo
        outs'  = head $ filter (\o -> (txOutValue o `geq` mRelayerCollateral mixer) &&
            (txOutAddress o == Address (ScriptCredential vestingScriptPermanentHash) Nothing)) outs

        VestingParams vTime _ h (VestingData pkhW _ _ _) = unsafeFromBuiltinData $ getDatum $ fromMaybe (error ()) $
            findDatum (fromMaybe (error ()) $ txOutDatumHash outs') txinfo

        -- finding current time estimate
        date = case ivTo (txInfoValidRange txinfo) of
                UpperBound (Finite t) True -> t
                _                          -> error ()

        -- finding this input's datum hash
        (_, dhash) = ownHashes ctx

        vestingOK = (vTime == date + hourPOSIX) && (dhash == h)
        paymentOK = any (\o -> (txOutValue o `geq` mValue mixer) &&
            (txOutAddress o == pubKeyHashAddress pkhW Nothing)) outs
mkMixerValidator _ (MixerDatum d) PayRelayTicket ctx = (valOuts `geq` valIn) && (d == zero)
    where
        ownIn   = txInInfoResolved $ fromMaybe (error ()) $ findOwnInput ctx
        valIn   = txOutValue ownIn
        ownOuts = getContinuingOutputs ctx
        valOuts = sum $ map txOutValue ownOuts

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

getRelayTicketUTXOs :: AsContractError e => Mixer -> Contract w s e (Map TxOutRef ChainIndexTxOut)
getRelayTicketUTXOs mixer = do
    utxos <- utxosAt (mixerAddress mixer)
    return $ Data.Map.filter (\o -> f o == Just zero) utxos
  where f o = do
            d <- either (const Nothing) Just $ _ciTxOutDatum o
            m <- fromBuiltinData $ getDatum d
            return $ getMixerDatum m