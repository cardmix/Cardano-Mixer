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

import           Cardano.Api.NetworkId.Extra         (NetworkIdWrapper (..), testnetNetworkId)
import           Control.Monad                       (void)
import           Control.Monad.Freer.Extras          (logInfo)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Default                        (def)
import           Ledger                              (ValidatorMode(FullyAppliedValidators))
import           Ledger.Value                        (Value(..))
import           PlutusTx.AssocMap                   (elems)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, handleBuiltin)
import           Plutus.PAB.Run                      (runWith)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Plutus.V1.Ledger.Ada                (lovelaceValueOf)
import           Plutus.Trace                        (ScriptsConfig (..), Command (Scripts, Transactions), writeScriptsTo)
import           Plutus.Trace.Emulator
import           System.Environment                  (getArgs)

import           Configuration.PABConfig             (pabWallet, pabTestValues, pabWalletPKH)
import           Crypto
import           MixerScript
import           PAB



main :: IO ()
main = do
    args <- getArgs
    vals <- pabTestValues
    case args of
        ["emulator", "transactions"] -> void $ writeScriptsTo (ScriptsConfig "emulator" (Transactions (unNetworkIdWrapper testnetNetworkId)
                                             "testnet/protocol-parameters.json") ) "transaction" (pabEmulator vals) def
        ["emulator", "scripts"]      -> void $ writeScriptsTo (ScriptsConfig "emulator" (Scripts FullyAppliedValidators))
                                             "script" (pabEmulator vals) def
        ["emulator", "trace"]        -> runEmulatorTraceIO (pabEmulator vals)
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

pabEmulator :: (Fr, Proof, Fr, Fr, Fr, Fr) -> EmulatorTrace ()
pabEmulator (leaf, proof, key, keyA, oh, nh) = do
    c1 <- activateContractWallet pabWallet (void mixerProgram)
    callEndpoint @"deposit" c1 (DepositParams (lovelaceValueOf 10_000_000) leaf)

    _ <- waitNSlots 10

    c2 <- activateContractWallet pabWallet (void mixerProgram)
    callEndpoint @"withdraw" c2 (WithdrawParams (lovelaceValueOf 10_000_000) pabWalletPKH key keyA oh nh proof)

    _ <- waitNSlots 10

    logInfo @String "Finished."
