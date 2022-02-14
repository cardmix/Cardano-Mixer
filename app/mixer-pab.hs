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
import           Ledger                              (ValidatorMode(FullyAppliedValidators))
import           Ledger.Scripts                      (ValidatorHash(ValidatorHash))
import           Ledger.Value                        (Value(..), CurrencySymbol (unCurrencySymbol))
import           PlutusTx.AssocMap                   (elems)
import           PlutusTx.Prelude                    hiding (Eq, Ord, (<$>))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, handleBuiltin)
import           Plutus.PAB.Run                      (runWith)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Plutus.V1.Ledger.Ada                (lovelaceValueOf)
import           Plutus.Trace                        (ScriptsConfig (..), Command (Scripts, Transactions), writeScriptsTo)
import           Plutus.Trace.Emulator
import           Prelude                             (IO, Double, String, (/), (^), fromIntegral, print, show, getLine, (<$>), )
import           System.CPUTime                      (getCPUTime)
import           System.Environment                  (getArgs)


import           Configuration.PABConfig             (pabWallet, pabWalletPKH)
import           Contracts.Vesting                   (vestingScriptAddress, vestingScriptHash, vestingContract)
import           Crypto
import           Crypto.Conversions
import           MixerContract
import           MixerProofs                         (generateSimulatedWithdrawProof, verifyWithdraw)
import           MixerState                          (MerkleTree(..), treeSize)
import           MixerUserData
import           PABContracts                        (PABContracts, handlers)
import           Tokens.RelayTicketToken             (relayTicketTokenSymbol)
import           Utils.Common                        (replicate, last)
import           Utils.Contracts                     (byteStringToList)


main :: IO ()
main = do
    print $ byteStringToList $ unCurrencySymbol relayTicketTokenSymbol
    print vestingScriptAddress
    args <- getArgs
    ds   <- generateDepositSecret
    sas  <- generateShieldedAccountSecret
    vals <- pabTestValues ds sas
    case args of
        ["emulator", "transactions"] -> void $ writeScriptsTo (ScriptsConfig "emulator" (Transactions (unNetworkIdWrapper testnetNetworkId)
                                             "testnet/protocol-parameters.json") ) "transaction" (pabEmulator vals) def
        ["emulator", "scripts"]      -> void $ writeScriptsTo (ScriptsConfig "emulator" (Scripts FullyAppliedValidators))
                                             "script" (pabEmulator vals) def
        ["emulator", "trace"]        -> runEmulatorTraceIO (pabEmulator vals)
        ["simulator"] -> pabSimulator
        _             -> pabTestNet

pabTestNet :: IO ()
pabTestNet = runWith (handleBuiltin @PABContracts)


--------------------------------------- Simulator -----------------------------------------------

pabSimulator :: IO ()
pabSimulator = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin PABContracts) "Starting Oracle PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    void $ liftIO getLine

    Simulator.logString @(Builtin PABContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin PABContracts) b

    f <- head . elems . head . elems . getValue <$> Simulator.walletFees pabWallet
    Simulator.logString @(Builtin PABContracts) $ "Total fees paid: " ++ show ((fromIntegral f :: Double) / 1_000_000) ++ " ADA"

    shutdown

--------------------------------------- Emulator  trace -----------------------------------------------

pabEmulator :: (Fr, [Fr], Proof) -> EmulatorTrace ()
pabEmulator (leaf, subs, proof) = do
    c1 <- activateContractWallet pabWallet (void mixerProgram)
    callEndpoint @"deposit" c1 (DepositParams (lovelaceValueOf 20_000) leaf)

    _ <- waitNSlots 10

    c2 <- activateContractWallet pabWallet (void mixerProgram)
    callEndpoint @"withdraw" c2 (WithdrawParams (lovelaceValueOf 20_000) (0, 1) pabWalletPKH subs proof)

    _ <- waitNSlots 4000

    c3 <- activateContractWallet pabWallet (void vestingContract)
    callEndpoint @"retrieve funds" c3 ()

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

          print root

          let ValidatorHash vh = vestingScriptHash
          print $ byteStringToList vh

          t1 <- getCPUTime
          (_, subs, proof) <- generateSimulatedWithdrawProof (dataToZp pabWalletPKH) (DepositSecret r1 r2) (ShieldedAccountSecret v1 v2 v3) [MerkleTree 1 $ padToPowerOfTwo treeSize [leaf]]
          t2 <- getCPUTime
          print $ verifyWithdraw [one, zero, zero, zero, zero, zero, root, a, h, hA, one, oh, nh] proof
          t3 <- getCPUTime
          print $ (fromIntegral (t2 - t1) :: Double) / 10^(12 :: Integer)
          print $ (fromIntegral (t3 - t2) :: Double) / 10^(12 :: Integer)
          return (leaf, subs, proof)