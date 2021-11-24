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
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module MixerFactory (
    StartParams(..),
    MixerFactorySchema,
    mixerFactoryProgram,
    getMixerByValue
) where

import           Data.Aeson                       (FromJSON, ToJSON)
import           GHC.Generics                     (Generic)
import           Ledger                           (Datum(..), Validator, Address, Value, AssetClass, ScriptContext, scriptAddress)
import           Ledger.Constraints.TxConstraints ( mustPayToOtherScript )
import           Ledger.Tokens                    (token)
import           Ledger.Typed.Scripts             (TypedValidator, mkTypedValidator, validatorScript, wrapValidator)
import           Ledger.Typed.Tx                  (TypedScriptTxOut(tyTxOutData))
import           Plutus.Contract                  (Contract, endpoint, selectList, throwError, Endpoint, Promise)
import           Plutus.Contracts.Currency        (SimpleMPS(..), currencySymbol)
import           Plutus.Contract.StateMachine
import           PlutusTx.IsData.Class            (ToData(..))
import           Plutus.V1.Ledger.Value           (AssetClass(..), assetClassValue)
import qualified PlutusTx
import           PlutusTx.AssocMap                (Map, singleton, empty, insert, lookup)
import           PlutusTx.Prelude                 hiding (mempty, filter, check)
import           Prelude                          (Monoid(..), Show (..), (^))
import           Schema                           (ToSchema)

import           AdminKey
import           CheckKey                         (CheckKeyState(..), checkKeyValidatorHash)
import           Crypto.BLS12381                  (R(..))
import           Crypto.Zp                        (fieldPrime, toZp)
import           MixerStateMachine
import           Utility

----------------------- Data types, instances, and constants -----------------------------

newtype MixerFactoryState = MFState (Map Value (ThreadToken, AssetClass))
  deriving Show

PlutusTx.unstableMakeIsData ''MixerFactoryState

newtype MixerFactoryInput = MFInput MixerFactoryState
    deriving Show

PlutusTx.unstableMakeIsData ''MixerFactoryInput

-- Parameters to start a new Mixer
data StartParams = StartParams
    {
        sValue       :: !Value,
        sTreeDepth   :: !Integer
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.makeLift ''StartParams

---------------------------------------------------------------------
---------------------------- On-Chain -------------------------------
---------------------------------------------------------------------

-------------------------- State Machine ----------------------------

{-# INLINABLE transition #-}
transition :: State MixerFactoryState -> MixerFactoryInput -> Maybe (TxConstraints Void Void, State MixerFactoryState)
transition s i = case (stateData s, i) of
    (MFState _, MFInput (MFState s')) -> Just (Prelude.mempty, State (MFState s') zero)

{-# INLINABLE final #-}
final :: MixerFactoryState -> Bool
final _ = False

{-# INLINABLE check #-}
check :: MixerFactoryState -> MixerFactoryInput -> ScriptContext -> Bool
check = adminKeyRequired

{-# INLINABLE mixerFactoryStateMachine #-}
mixerFactoryStateMachine :: StateMachine MixerFactoryState MixerFactoryInput
mixerFactoryStateMachine = StateMachine
    {
        smTransition  = transition,
        smFinal       = final,
        smCheck       = check,
        smThreadToken = Nothing
    }

------------------------------ Validator --------------------------------

type ThreadTokenPosting = StateMachine MixerFactoryState MixerFactoryInput

{-# INLINABLE mkMixerFactoryValidator #-}
mkMixerFactoryValidator :: MixerFactoryState -> MixerFactoryInput -> ScriptContext -> Bool
mkMixerFactoryValidator = mkValidator mixerFactoryStateMachine

type Mixing = StateMachine MixerState MixerInput

-- Validator instance
mixerFactoryInst :: TypedValidator ThreadTokenPosting
mixerFactoryInst = mkTypedValidator @ThreadTokenPosting
    $$(PlutusTx.compile [|| mkMixerFactoryValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator @MixerFactoryState @MixerFactoryInput

-- Validator script
mixerFactoryValidator :: Validator
mixerFactoryValidator = validatorScript mixerFactoryInst

-- Validator address
mixerFactoryAddress :: Address
mixerFactoryAddress = scriptAddress mixerFactoryValidator

---------------------------------------------------------------------
--------------------------- Off-Chain -------------------------------
---------------------------------------------------------------------

-- Mixer Factory client
mixerFactoryClient :: StateMachineClient MixerFactoryState MixerFactoryInput
mixerFactoryClient = mkStateMachineClientAdmin $ StateMachineInstance mixerFactoryStateMachine mixerFactoryInst

type MixerFactorySchema = Endpoint "start" StartParams

start :: Promise () MixerFactorySchema SMContractError MixerFactoryState
start = endpoint @"start" @StartParams $ \(StartParams v d) -> do
    withdrawCurrency <- mintTokens $ SimpleMPS "Cardano Mixer Withdraw Token" (2^d+1)
    tt <- getThreadToken    
    let ac      = AssetClass (currencySymbol withdrawCurrency, "Cardano Mixer Withdraw Token")
        mixer   = Mixer { mValue = v, mToken = tt, mWithdrawAssetClass = ac}
        client  = mixerClient mixer
        cp      = replicate (d+1) zero
        c       = 0
        ttVal   = mixerTokenValue mixer
        lookups = mempty
        ckState = CheckKeyState zero (toZp (fieldPrime R - 1))
        cons    = mustPayToOtherScript (checkKeyValidatorHash ttVal) (Datum $ toBuiltinData ckState) (token ac)
    _ <- runInitialiseWith lookups cons client (MixerState cp c) (assetClassValue ac (2^d))    -- initialise a new mixer
    updateMixerFactoryState (v, tt, ac)                     -- now we post thread token to the blockchain

mixerFactoryProgram :: Contract () MixerFactorySchema SMContractError MixerFactoryState
mixerFactoryProgram = do
    selectList [start] >> mixerFactoryProgram

----------------------------- Auxiliary -------------------------------

getMixerByValue :: Value -> Contract w s SMContractError Mixer
getMixerByValue v = do
    MFState state <- getMixerFactoryState
    case lookup v state of
        Nothing -> throwError $ ChooserError "Mixer not found!"
        Just (tt, ac) -> return $ Mixer v tt ac


getMixerFactoryState :: Contract w s SMContractError MixerFactoryState
getMixerFactoryState = do
    ocs <- getOnChainState mixerFactoryClient
    case ocs of
        Nothing -> return $ MFState empty
        Just (o, _) -> return $ tyTxOutData $ ocsTxOut o


updateMixerFactoryState :: (Value, ThreadToken, AssetClass) -> Contract w s SMContractError MixerFactoryState
updateMixerFactoryState (v, t, ac) = do
    (lookups, constraints) <- adminKeyTx
    MFState state <- getMixerFactoryState
    if null state
        then runInitialiseWith lookups constraints mixerFactoryClient (MFState $ singleton v (t, ac)) zero
        else do
            -- TODO: What if we insert a new mixer with the same value?
            tResult <- runStepWith lookups constraints mixerFactoryClient (MFInput $ MFState $ insert v (t, ac) state)
            case tResult of
                TransitionFailure _  -> return $ MFState state
                TransitionSuccess ns -> return ns
