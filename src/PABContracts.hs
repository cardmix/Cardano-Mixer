{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}


module PABContracts where

import           Data.Aeson                          (FromJSON(..), ToJSON(..))
import qualified Data.OpenApi
import           Data.Semigroup                      (Last)
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Plutus.Contract                     (Contract)
import           Plutus.Contract.Schema              (EmptySchema)
import           Plutus.PAB.Effects.Contract.Builtin (SomeBuiltin (..), HasDefinitions(..), endpointsToSchemas)
import           Prettyprinter                       (Pretty(..), viaShow)

import           Contracts.ConnectToPABContract      (ConnectToPABSchema, connectToPABPromise)
import           Contracts.CurrencyContract          (CurrencySchema, mintCurrency)
import           Contracts.DispenserContract         (dispenserProgram)
import           Contracts.MixerContract             (mixerProgram)
import           Contracts.MixerRelayerContract      (MixerRelayerSchema, mixerRelayerProgram)
import           Contracts.MixerStateContract        (MixerStateSchema, getMixerStatePromise)
import           Contracts.VestingContract           (retrieveFundsLoop)
import           Scripts.VestingScript               (VestingError)

--------------------------------------- PAB Contracts -------------------------------------------

data MixerFrontendContracts = MixerUse | MixerStateQuery | ConnectToPAB
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
    deriving anyclass (Data.OpenApi.ToSchema)

data MixerBackendContracts = MintCurrency | MixerRelay | RetrieveTimeLocked | Dispense
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
    deriving anyclass (Data.OpenApi.ToSchema)

-- We use a wrapper to define contracts here
data PABContracts = BackendContracts MixerBackendContracts | FrontendContracts MixerFrontendContracts
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
    deriving anyclass (Data.OpenApi.ToSchema)

instance Pretty PABContracts where
    pretty = viaShow

-- TODO: Proof data type does not have ToSchema
instance HasDefinitions PABContracts where
    getDefinitions = map BackendContracts [MintCurrency, MixerRelay, RetrieveTimeLocked, Dispense] ++
        map FrontendContracts [MixerUse, MixerStateQuery, ConnectToPAB]
    getSchema = \case
        BackendContracts MintCurrency       -> endpointsToSchemas @CurrencySchema
        BackendContracts MixerRelay         -> endpointsToSchemas @MixerRelayerSchema
        BackendContracts RetrieveTimeLocked -> endpointsToSchemas @EmptySchema
        BackendContracts Dispense           -> endpointsToSchemas @EmptySchema
        FrontendContracts MixerUse          -> [] --endpointsToSchemas  @MixerSchema
        FrontendContracts MixerStateQuery   -> endpointsToSchemas @MixerStateSchema
        FrontendContracts ConnectToPAB      -> endpointsToSchemas @ConnectToPABSchema
    getContract = \case
        BackendContracts MintCurrency       -> SomeBuiltin mintCurrency
        BackendContracts MixerRelay         -> SomeBuiltin mixerRelayerProgram
        BackendContracts RetrieveTimeLocked -> SomeBuiltin (retrieveFundsLoop :: Contract (Maybe (Last Text)) EmptySchema VestingError ())
        BackendContracts Dispense           -> SomeBuiltin dispenserProgram
        FrontendContracts MixerUse          -> SomeBuiltin mixerProgram
        FrontendContracts MixerStateQuery   -> SomeBuiltin getMixerStatePromise
        FrontendContracts ConnectToPAB      -> SomeBuiltin connectToPABPromise
        