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
import           Mixer                               (mixerProgram)
import           MixerFactory                        (mixerFactoryProgram, MixerFactorySchema)


data MixerContracts = MintAdminKey | Start | UseMixer
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
    deriving anyclass (Data.OpenApi.ToSchema)

instance Pretty MixerContracts where
    pretty = viaShow

instance HasPSTypes MixerContracts where
    -- psTypes =
    --     [ equal . genericShow . argonaut $ mkSumType @MixerContracts
    --     -- These types come from the Uniswap contract and need to be available in PS
    --     , equal . genericShow . argonaut $ mkSumType @Uniswap
    --     , equal . genericShow . argonaut $ mkSumType @(Coin A)
    --     , order . equal . genericShow $ argonaut $ mkSumType @U
    --     ]

-- TODO: Proof data type does not have ToSchema
instance HasDefinitions MixerContracts where
    getDefinitions = [MintAdminKey, Start, UseMixer]
    getSchema = \case
        MintAdminKey     -> endpointsToSchemas  @CurrencySchema
        Start            -> endpointsToSchemas  @MixerFactorySchema
        UseMixer         -> [] --endpointsToSchemas  @MixerSchema
    getContract = \case
        MintAdminKey -> SomeBuiltin mintCurrency
        Start        -> SomeBuiltin mixerFactoryProgram
        UseMixer     -> SomeBuiltin mixerProgram

handlers :: Simulator.SimulatorEffectHandlers (Builtin MixerContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin MixerContracts) def def
    $ interpret (contractHandler handleBuiltin)
