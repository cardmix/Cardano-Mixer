{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}


module PABContracts where

import           Data.Aeson                          (FromJSON(..), ToJSON(..))
import           Data.Default                        (Default (..))
import qualified Data.OpenApi
import           Control.Monad.Freer                 (interpret)

import           GHC.Generics                        (Generic)

import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), BuiltinHandler(..), HasDefinitions(..),
                                                        handleBuiltin, endpointsToSchemas)
import           Plutus.PAB.Run.PSGenerator          (HasPSTypes(..))
import qualified Plutus.PAB.Simulator                as Simulator
import           Prettyprinter                       (Pretty(..), viaShow)

import           Contracts.Currency                  (CurrencySchema, mintCurrency)
import           Contracts.Vesting                   (VestingSchema, vestingContract)
import           MixerContract                       (mixerProgram)
import           MixerStateContract                  (MixerStateSchema, getMixerStatePromise)
import           MixerContractsDefinition            (MixerContractsDefinition(..))

--------------------------------------- PAB Contracts -------------------------------------------

-- We use a wrapper to define backend instances here
newtype PABContracts = PABContracts MixerContractsDefinition
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
    deriving anyclass (Data.OpenApi.ToSchema)

instance Pretty PABContracts where
    pretty = viaShow

instance HasPSTypes PABContracts where

-- TODO: Proof data type does not have ToSchema
instance HasDefinitions PABContracts where
    getDefinitions = map PABContracts [MintAdminKey, UseMixer, QueryMixer, RetrieveTimeLocked]
    getSchema = \case
        PABContracts MintAdminKey       -> endpointsToSchemas @CurrencySchema
        PABContracts UseMixer           -> [] --endpointsToSchemas  @MixerSchema
        PABContracts QueryMixer         -> endpointsToSchemas @MixerStateSchema
        PABContracts RetrieveTimeLocked -> endpointsToSchemas @VestingSchema
    getContract = \case
        PABContracts MintAdminKey       -> SomeBuiltin mintCurrency
        PABContracts UseMixer           -> SomeBuiltin mixerProgram
        PABContracts QueryMixer         -> SomeBuiltin getMixerStatePromise
        PABContracts RetrieveTimeLocked -> SomeBuiltin vestingContract

handlers :: Simulator.SimulatorEffectHandlers (Builtin PABContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin PABContracts) def def
    $ interpret (contractHandler handleBuiltin)