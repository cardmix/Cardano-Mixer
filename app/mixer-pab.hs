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



module Main
    ( main
    ) where

import           Control.Monad                       (void)
import           Control.Monad.IO.Class              (MonadIO (..))

import           Ledger.Value                        (Value(..))
import           PlutusTx.AssocMap                   (elems)

import           Plutus.PAB.Effects.Contract.Builtin (Builtin, handleBuiltin)
import           Plutus.PAB.Run                      (runWith)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Plutus.Trace.Emulator
import           System.Environment                  (getArgs)

import           Config                              (pabWallet)
import           PAB

--- For Emulator
import Control.Monad.Freer.Extras (logInfo)
import Contracts.Currency (mintCurrency, SimpleMPS (SimpleMPS))
import MixerFactory (mixerFactoryProgram, StartParams (StartParams))
import AdminKey (adminKeyTokenName)
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.Trace (writeScriptsTo, ScriptsConfig (..), Command (Scripts, Transactions))
import Data.Default (def)
import Ledger (ValidatorMode(FullyAppliedValidators))
-- import Cardano.Api
-- import Cardano.Api
import Cardano.Api.NetworkId.Extra (testnetNetworkId, NetworkIdWrapper (unNetworkIdWrapper))

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["emulator"]  -> --void $ writeScriptsTo (ScriptsConfig "testnet" (Transactions (unNetworkIdWrapper testnetNetworkId) "testnet/protocol-parameters.json") ) "trace" pabEmulator def
            -- void $ writeScriptsTo (ScriptsConfig "testnet" (Scripts FullyAppliedValidators) ) "trace" pabEmulator def
            runEmulatorTraceIO pabEmulator
        ["simulator"] -> pabSimulator
        _             -> pabTestNet

pabTestNet :: IO ()
pabTestNet = runWith (handleBuiltin @MixerContracts)

--------------------------------------- Simulator -----------------------------------------------

pabSimulator :: IO ()
pabSimulator = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin MixerContracts) "Starting Oracle PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    void $ liftIO getLine

    Simulator.logString @(Builtin MixerContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin MixerContracts) b

    f <- head . elems . head . elems . getValue <$> Simulator.walletFees pabWallet
    Simulator.logString @(Builtin MixerContracts) $ "Total fees paid: " ++ show ((fromIntegral f :: Double) / 1_000_000) ++ " ADA"

    shutdown

--------------------------------------- Emulator  trace -----------------------------------------------

pabEmulator :: EmulatorTrace ()
pabEmulator = do
    c1 <- activateContractWallet pabWallet (void mintCurrency)
    callEndpoint @"Create native token" c1 (SimpleMPS adminKeyTokenName 1)

    _ <- waitNSlots 10

    c2 <- activateContractWallet pabWallet (void mixerFactoryProgram)
    callEndpoint @"start" c2 (StartParams (lovelaceValueOf 200_000_000) 10)

    _ <- waitNSlots 10

    logInfo @String "Finished."
