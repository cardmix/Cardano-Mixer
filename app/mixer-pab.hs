{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}


module Main
    ( main
    ) where

import           Cardano.Api.NetworkId.Extra         (NetworkIdWrapper (..), testnetNetworkId)
import           Control.Monad                       (void)
import           Control.Monad.Freer.Extras          (logInfo)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Default                        (Default(..))
import           Data.Semigroup                      (Last(..))
import           Ledger                              (ValidatorMode(FullyAppliedValidators))
import           Ledger.Value                        (Value(..))
import           Plutus.Contract.Test (knownWallet, mockWalletPaymentPubKeyHash)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, handleBuiltin)
import           Plutus.PAB.Run                      (runWith)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Plutus.V1.Ledger.Ada                (lovelaceValueOf)
import           Plutus.Trace                        (ScriptsConfig (..), Command (Scripts, Transactions), writeScriptsTo)
import           Plutus.Trace.Emulator
import           PlutusTx.AssocMap                   (elems)
import           PlutusTx.Prelude                    hiding (Eq, Ord, (<$>))
import           Prelude                             (IO, Double, String, (/), (^), fromIntegral, print, show, getLine, (<$>), )
import           System.CPUTime                      (getCPUTime)
import           System.Environment                  (getArgs)

import           Configuration.PABConfig             (pabWallet, pabWalletPKH)
import           Contracts.CurrencyContract          (SimpleMPS (..), mintCurrency)
import           Contracts.DispenserContract         (dispenserProgram)
import           Contracts.MixerContract
import           Contracts.VestingContract           (vestingContract)
import           Crypto
import           Crypto.Conversions
import           MixerProofs                         (generateSimulatedWithdrawProof, verifyWithdraw)
import           MixerState                          (MerkleTree(..), treeSize)
import           MixerUserData
import           PABContracts                        (PABContracts (BackendContracts), MixerBackendContracts (..), handlers)
import           Tokens.MIXToken                     (mixTokenSimulator)
import           Utils.Common                        (replicate, last)


main :: IO ()
main = do
    args <- getArgs
    ds   <- generateDepositSecret
    sas  <- generateShieldedAccountSecret

    case args of
        "emulator" : t -> do
            vals <- pabTestValues ds sas
            case t of
                ["transactions"] -> void $ writeScriptsTo (ScriptsConfig "emulator" (Transactions (unNetworkIdWrapper testnetNetworkId)
                                             "testnet/protocol-parameters.json") ) "transaction" (pabEmulator vals) def
                ["scripts"]      -> void $ writeScriptsTo (ScriptsConfig "emulator" (Scripts FullyAppliedValidators))
                                             "script" (pabEmulator vals) def
                ["trace"]        -> runEmulatorTraceIO (pabEmulator vals)
                _                -> print ("Unknown command" :: String)
        ["simulator"] -> pabSimulator
        _             -> pabTestNet

pabTestNet :: IO ()
pabTestNet = runWith (handleBuiltin @PABContracts)


--------------------------------------- Simulator -----------------------------------------------

pabSimulator :: IO ()
pabSimulator = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin PABContracts) "Starting Oracle PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    c0 <- Simulator.activateContract pabWallet (BackendContracts MintCurrency)
    _  <- Simulator.callEndpointOnInstance c0 "Create native token" (SimpleMPS "tMIX" 100_000_000_000)
    _  <- Simulator.payToWallet (knownWallet 3) pabWallet (lovelaceValueOf 2_000_000)

    void $ liftIO getLine

    Simulator.logString @(Builtin PABContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin PABContracts) b

    f <- head . elems . head . elems . getValue <$> Simulator.walletFees pabWallet
    Simulator.logString @(Builtin PABContracts) $ "Total fees paid: " ++ show ((fromIntegral f :: Double) / 1_000_000) ++ " ADA"

    shutdown

--------------------------------------- Emulator  trace -----------------------------------------------

pabEmulatorMIXFee :: Value
pabEmulatorMIXFee = scale 10 mixTokenSimulator + lovelaceValueOf 4_000

-- pabEmulatorMIXFee :: Value
-- pabEmulatorMIXFee = lovelaceValueOf 30_000

pabEmulator :: (Fr, [Fr], Proof) -> EmulatorTrace ()
pabEmulator (leaf, subs, proof) = do
    c0 <- activateContractWallet pabWallet (void mintCurrency)
    callEndpoint @"Create native token" c0 (SimpleMPS "tMIX" 100_000_000)
    _ <- waitNSlots 10

    _ <- payToWallet pabWallet (knownWallet 2) (scale 50_000_000 mixTokenSimulator)
    _ <- waitNSlots 10

    c1 <- activateContractWallet pabWallet (void mixerProgram)
    callEndpoint @"deposit" c1 (DepositParams (mockWalletPaymentPubKeyHash $ knownWallet 2) pabEmulatorMIXFee leaf)
    _ <- waitNSlots 10

    obs <- observableState c1
    let ctx = case obs of
                Just (Last a) -> a
                Nothing -> error ()
    c2 <- activateContractWallet pabWallet (void mixerProgram)
    callEndpoint @"depositSubmit" c2 ctx
    _ <- waitNSlots 10

    c3 <- activateContractWallet pabWallet (void mixerProgram)
    callEndpoint @"withdraw" c3 (WithdrawParams pabEmulatorMIXFee (0, 1) pabWalletPKH subs proof)
    _ <- waitNSlots 4000

    c4 <- activateContractWallet pabWallet (void vestingContract)
    callEndpoint @"retrieve funds" c4 ()
    _ <- waitNSlots 10

    _ <- payToWallet (knownWallet 2) pabWallet (lovelaceValueOf 2_000_000)
    _ <- waitNSlots 10

    _ <- activateContractWallet pabWallet (void $ dispenserProgram 1000)
    _ <- waitNSlots 10

    logInfo @String "Finished."

-------------------------------------- Test values for the Emulator ------------------------------------

pabTestValues :: DepositSecret -> ShieldedAccountSecret -> IO (Fr, [Fr], Proof)
pabTestValues (DepositSecret r1 r2) (ShieldedAccountSecret v1 v2 v3) = do
          let h  = mimcHash (toZp 0) r1
              hA = mimcHash (dataToZp pabWalletPKH) r2
              oh = mimcHash v1 v2
              nh = mimcHash v1 v3
              leaf = mimcHash r1 r2
              d   = 10
              a   = dataToZp pabWalletPKH
              c0  = 0
              cp0 = replicate d zero
              cp  = addMerkleLeaf leaf (c0+1) cp0
              root = last cp

          t1 <- getCPUTime
          (_, subs, proof) <- generateSimulatedWithdrawProof (dataToZp pabWalletPKH) (DepositSecret r1 r2) (ShieldedAccountSecret v1 v2 v3) [MerkleTree 1 $ padToPowerOfTwo treeSize [leaf]]
          t2 <- getCPUTime
          print $ verifyWithdraw [one, zero, zero, zero, zero, zero, root, a, h, hA, one, oh, nh] proof
          t3 <- getCPUTime
          print $ (fromIntegral (t2 - t1) :: Double) / 10^(12 :: Integer)
          print $ (fromIntegral (t3 - t2) :: Double) / 10^(12 :: Integer)
          return (leaf, subs, proof)