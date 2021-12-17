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
import           Wallet.Emulator.Types               (mockWalletPaymentPubKeyHash)

import           AdminKey                            (adminKeyTokenName)
import           Configuration.PABConfig             (pabWallet, pabTestValues)
import           Contracts.Currency                  (SimpleMPS (..), mintCurrency)
import           Mixer                               (DepositParams(..), WithdrawParams (..), mixerProgram)
import           MixerFactory                        (StartParams (..), mixerFactoryProgram)
import           PAB



main :: IO ()
main = do
    args <- getArgs
    case args of
        ["emulator", "transactions"] -> void $ writeScriptsTo (ScriptsConfig "testnet" (Transactions (unNetworkIdWrapper testnetNetworkId) "testnet/protocol-parameters.json") ) "trace" pabEmulator def
        ["emulator", "scripts"]      -> void $ writeScriptsTo (ScriptsConfig "testnet" (Scripts FullyAppliedValidators) ) "trace" pabEmulator def
        ["emulator", "trace"]        -> runEmulatorTraceIO pabEmulator
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
    let (leaf, proof, key, keyA, oh, nh) = pabTestValues

    c1 <- activateContractWallet pabWallet (void mintCurrency)
    callEndpoint @"Create native token" c1 (SimpleMPS adminKeyTokenName 1)

    _ <- waitNSlots 10

    c2 <- activateContractWallet pabWallet (void mixerFactoryProgram)
    callEndpoint @"start" c2 (StartParams (lovelaceValueOf 50_000_000) 10)

    _ <- waitNSlots 10

    c3 <- activateContractWallet pabWallet (void mixerProgram)
    callEndpoint @"deposit" c3 (DepositParams (lovelaceValueOf 50_000_000) leaf)

    _ <- waitNSlots 10

    c4 <- activateContractWallet pabWallet (void mixerProgram)
    callEndpoint @"withdraw" c4 (WithdrawParams (lovelaceValueOf 50_000_000) (mockWalletPaymentPubKeyHash pabWallet) key keyA oh nh proof)

    _ <- waitNSlots 10

    logInfo @String "Finished."
