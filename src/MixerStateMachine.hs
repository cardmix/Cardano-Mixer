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

module MixerStateMachine (
    Mixer(..),
    MixerState(..),
    MixerInput(..),
    mixerStateMachine,
    mixerValidatorHash,
    mixerAddress,
    mixerClient,
    mixerTokenValue,
    withdrawToken,
    mapError',
    verifierTokenCurrency,
    verifierTokenName,
    verifierTokenAssetClass
) where

import           Ledger                                   hiding (singleton, validatorHash)
import           Ledger.Constraints.TxConstraints
import           Ledger.Tokens                            (token)
import           Ledger.Typed.Scripts                     (TypedValidator, mkTypedValidator, validatorScript, validatorHash, wrapValidator)
import           Ledger.Value                             (AssetClass(..), CurrencySymbol(..), TokenName(..))
import           Plutus.Contract.StateMachine
import           Plutus.Contract.StateMachine.ThreadToken (threadTokenValue, ThreadToken (ttCurrencySymbol))
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (Show (..))

import           AdminKey                                 (adminKeyRequired)
import           CheckKey                                 (checkKeyRequired)

import           Crypto
import           Utility                                  (mapError', init)


----------------------- Data types, instances, and constants -----------------------------

data Mixer = Mixer {
    mValue              :: !Value,            -- Mixing value
    mToken              :: !ThreadToken,      -- Thread token
    mWithdrawAssetClass :: !AssetClass        -- Withdraw token asset class
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

-- Mixer withdraw token
withdrawToken :: Mixer -> Value
withdrawToken = token . mWithdrawAssetClass

-- Mixer state
data MixerState = MixerState
  {
      coPath          :: [Fr],
      depositCounter  :: Integer
  }
  deriving Show

PlutusTx.unstableMakeIsData ''MixerState

data MixerInput = Deposit Fr | Withdraw PaymentPubKeyHash Fr Proof
    deriving Show

PlutusTx.unstableMakeIsData ''MixerInput

-------------------------- State Machine ----------------------------

{-# INLINABLE transition #-}
transition :: Mixer -> State MixerState -> MixerInput -> Maybe (TxConstraints Void Void, State MixerState)
transition mixer state input = case (stateValue state, stateData state, input) of
    (v, MixerState cp c, Deposit leaf)                       ->
        Just (mempty, State (MixerState (addMerkleLeaf leaf (c+1) (init cp)) (c+1)) (v + mValue mixer))
    (v, MixerState cp c, Withdraw pkh _ _) ->
        let txCons = mustPayToPubKey pkh (mValue mixer)
        in Just (txCons, State (MixerState cp c) (v - mValue mixer - withdrawToken mixer))

{-# INLINABLE final #-}
final :: Mixer -> MixerState -> Bool
final _ _ = False

{-# INLINABLE check #-}
check :: Mixer -> MixerState -> MixerInput -> ScriptContext -> Bool
check _     _     (Deposit _)       _   = True
-- check _     state input@Deposit  {} ctx = adminKeyRequired state input ctx  -- TEMPORARY: one-server implementation
check mixer state input@Withdraw {} ctx =
    adminKeyRequired state input ctx && -- TEMPORARY: one-server implementation
      checkKeyRequired (withdrawToken mixer) state input ctx

{-# INLINABLE mixerStateMachine #-}
mixerStateMachine :: Mixer -> StateMachine MixerState MixerInput
mixerStateMachine mixer = StateMachine
    {
        smTransition  = transition mixer,
        smFinal       = final mixer,
        smCheck       = check mixer,
        smThreadToken = Just $ mToken mixer
    }

------------------------------ Validator --------------------------------

{-# INLINABLE mkMixerValidator #-}
mkMixerValidator :: Mixer -> MixerState -> MixerInput -> ScriptContext -> Bool
mkMixerValidator mixer = mkValidator $ mixerStateMachine mixer

type Mixing = StateMachine MixerState MixerInput

-- Validator instance
mixerInst :: Mixer -> TypedValidator Mixing
mixerInst mixer = mkTypedValidator @Mixing
    ($$(PlutusTx.compile [|| mkMixerValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode mixer)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator @MixerState @MixerInput

-- Validator script
mixerValidator :: Mixer -> Validator
mixerValidator = validatorScript . mixerInst

-- Validator Hash
mixerValidatorHash :: Mixer -> ValidatorHash
mixerValidatorHash = validatorHash . mixerInst

-- Validator address
mixerAddress :: Mixer -> Address
mixerAddress = scriptAddress . mixerValidator

-- Mixer thread token value
mixerTokenValue :: Mixer -> Value
mixerTokenValue mixer = threadTokenValue (ttCurrencySymbol $ mToken mixer) (mixerValidatorHash mixer)

----------------------------- For off-chain ------------------------------

-- Mixer client
mixerClient :: Mixer -> StateMachineClient MixerState MixerInput
mixerClient mixer = mkStateMachineClient $ StateMachineInstance (mixerStateMachine mixer) (mixerInst mixer)


