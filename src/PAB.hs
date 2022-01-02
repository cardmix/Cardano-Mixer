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


module PAB where

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
import           MixerScript                         (mixerProgram)


data MixerContracts = MintAdminKey | UseMixer
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
    deriving anyclass (Data.OpenApi.ToSchema)

instance Pretty MixerContracts where
    pretty = viaShow

instance HasPSTypes MixerContracts where

-- TODO: Proof data type does not have ToSchema
instance HasDefinitions MixerContracts where
    getDefinitions = [MintAdminKey, UseMixer]
    getSchema = \case
        MintAdminKey     -> endpointsToSchemas  @CurrencySchema
        UseMixer         -> [] --endpointsToSchemas  @MixerSchema
    getContract = \case
        MintAdminKey -> SomeBuiltin mintCurrency
        UseMixer     -> SomeBuiltin mixerProgram

handlers :: Simulator.SimulatorEffectHandlers (Builtin MixerContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin MixerContracts) def def
    $ interpret (contractHandler handleBuiltin)

