{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}


module Main
    ( main
    ) where

import           Data.Aeson                          (FromJSON(..), ToJSON(..), encode)
import           Control.Monad                       (void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Default                        (Default (..))
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Ledger.Value                        (Value(..))                        
import           PlutusTx.AssocMap                   (elems)
import           PlutusTx.Prelude                    ((+), length)
import           Plutus.Contracts.Currency           (CurrencySchema, mintCurrency)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), BuiltinHandler(..), HasDefinitions(..),
                                                        handleBuiltin, endpointsToSchemas)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Wallet.Emulator.Types               (Wallet (..))

import           Crypto
import           Mixer                               (mixerProgram)
import           MixerFactory                        (mixerFactoryProgram)

import Data.ByteString.Lazy (writeFile)
import System.CPUTime
import Wallet.Emulator.Wallet (Wallet(Wallet), walletPubKey)
import Ledger (pubKeyHash)

r1csFile :: String
r1csFile = "circuit-mixer.json"

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin MixerContracts) "Starting Oracle PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    void $ liftIO getLine

    Simulator.logString @(Builtin MixerContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin MixerContracts) b

    f <- head . elems . head . elems . getValue <$> Simulator.walletFees (Wallet 1)
    Simulator.logString @(Builtin MixerContracts) $ "Total fees paid: " ++ show ((fromIntegral f :: Double) / 1_000_000) ++ " ADA"

    shutdown

data MixerContracts = MintAdminKey | Start | UseMixer
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Pretty MixerContracts where
    pretty = viaShow

-- TODO: Proof data type does not have ToSchema
instance HasDefinitions MixerContracts where
    getDefinitions = [MintAdminKey, Start, UseMixer]
    getSchema = \case
        MintAdminKey     -> endpointsToSchemas  @CurrencySchema
        Start            -> [] --endpointsToSchemas  @MixerFactorySchema
        UseMixer         -> [] --endpointsToSchemas  @MixerSchema
    getContract = \case
        MintAdminKey -> SomeBuiltin mintCurrency
        Start        -> SomeBuiltin mixerFactoryProgram
        UseMixer     -> SomeBuiltin mixerProgram

handlers :: Simulator.SimulatorEffectHandlers (Builtin MixerContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin MixerContracts) def def
    $ interpret (contractHandler handleBuiltin)
    