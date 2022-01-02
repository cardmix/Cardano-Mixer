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
    mixerValidatorHash,
    mixerAddress,
    mapError',
    verifierTokenCurrency,
    verifierTokenName,
    verifierTokenAssetClass,
    DepositParams(..),
    WithdrawParams(..),
    MixerSchema,
    mixerProgram
) where

import           Data.Aeson                               (FromJSON, ToJSON)
import qualified Data.Map
import           Data.Map                                 (keys, findMin, fromList)
import           GHC.Generics                             (Generic)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Constraints.OffChain              (unspentOutputs, typedValidatorLookups, otherScript)
import           Ledger.Constraints.OnChain               (checkTxConstraint)
import           Ledger.Constraints.TxConstraints
import           Ledger.Tokens                            (token)
import           Ledger.Typed.Scripts                     (TypedValidator, ValidatorTypes(..), mkTypedValidator, validatorScript, validatorHash, wrapValidator)
import           Ledger.Value                             (AssetClass(..), CurrencySymbol(..), TokenName(..), geq)
import           Plutus.Contract                          (Promise, ContractError, Endpoint, type (.\/), Contract,
                                                            endpoint, selectList, utxosAt, logInfo, mkTxConstraints, submitTxConfirmed)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (Show (..), last, String, (<>))
import           Schema                                   (ToSchema)


import           Crypto
import           MixerProofs                              (verifyWithdraw)
import           Utility                                  (mapError', replicate)


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

newtype MixerDatum = MixerDatum Fr
  deriving Show

PlutusTx.unstableMakeIsData ''MixerDatum

data MixerRedeemer = MixerRedeemer PaymentPubKeyHash Fr Proof
    deriving Show

PlutusTx.unstableMakeIsData ''MixerRedeemer

data Mixing
instance ValidatorTypes Mixing where
  type instance DatumType Mixing = MixerDatum
  type instance RedeemerType Mixing = MixerRedeemer

------------------------------ Validator --------------------------------

{-# INLINABLE mkMixerValidator #-}
mkMixerValidator :: Mixer -> MixerDatum -> MixerRedeemer -> ScriptContext -> Bool
mkMixerValidator mixer _ (MixerRedeemer pkh _ _) ctx = checkTxConstraint ctx txc1
    where
        -- Must relay to the recipient
        txc1 = MustPayToPubKeyAddress pkh Nothing Nothing (mValue mixer)
        -- Must time lock the relayer token
        -- txc2 = MustPayToOtherScript ValidatorHash Datum Value

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

---------------------------------------------------------------------
--------------------------- Off-Chain -------------------------------
---------------------------------------------------------------------

-- Parameters for the "deposit" endpoint
data DepositParams = DepositParams
    {
        dpValue          :: !Value,
        dpLeaf           :: !Fr
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

-- "deposit" endpoint implementation
deposit :: Promise () MixerSchema ContractError ()
deposit = endpoint @"deposit" @DepositParams $ \(DepositParams v leaf) -> do
    let mixer = Mixer v v
        lookups = typedValidatorLookups $ mixerInst mixer
        cons    = mustPayToTheScript (MixerDatum leaf) v
    utx <- mkTxConstraints lookups cons
    submitTxConfirmed utx

-- Parameters for the "withdraw" endpoint
data WithdrawParams = WithdrawParams
    {
        wpValue         :: !Value,
        wpPKH           :: !PaymentPubKeyHash,
        wpKey           :: !Fr,
        wpKeyA          :: !Fr,
        wpOldHash       :: !Fr,
        wpNewHash       :: !Fr,
        wpProof         :: !Proof
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- "withdraw" endpoint implementation
withdraw :: Promise () MixerSchema ContractError ()
withdraw = endpoint @"withdraw" @WithdrawParams $ \(WithdrawParams v pkh key keyA oh nh proof) -> do
    let mixer = Mixer v v
    utxos <- utxosAt (mixerAddress mixer)
    let utxo           = Data.Map.filter (\o -> _ciTxOutValue o `geq` mValue mixer) utxos
        utxo'          = fromList [findMin utxo]
        txo            = head $ keys  utxo
    
    let coPath    = addMerkleLeaf (Zp 6607553988888913206274753584799503904250064978416565150316268919259420287010) 1 (replicate 10 zero)
        pubParams = [one, zero, zero, zero, zero, zero, last coPath, dataToZp pkh, key, keyA, toZp 1 :: Fr, oh, nh]
    let lookups   = unspentOutputs utxo' <> typedValidatorLookups (mixerInst mixer) <> otherScript (mixerValidator mixer)
        cons      = mustPayToPubKey pkh v <> mustSpendScriptOutput txo (Redeemer $ toBuiltinData $ MixerRedeemer pkh key proof)
    if verifyWithdraw pubParams proof
        then do
            utx <- mkTxConstraints lookups cons
            submitTxConfirmed utx
        else do
            logInfo @String "Supplied proof is not correct"

type MixerSchema = Endpoint "deposit" DepositParams .\/ Endpoint "withdraw" WithdrawParams

mixerProgram :: Contract () MixerSchema ContractError MixerDatum
mixerProgram = do
    selectList [deposit, withdraw] >> mixerProgram

