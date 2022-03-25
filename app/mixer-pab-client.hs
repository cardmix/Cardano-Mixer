{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}


module Main
    (
        main
    ) where

import           Control.Concurrent                           (threadDelay)
import           Control.Monad                                (void)
import           Data.Text                                    (Text)
import           Ledger.Value                                 (TokenName(..))
import           Plutus.V1.Ledger.Ada                         (lovelaceValueOf)
import           PlutusTx.Builtins.Class                      (stringToBuiltinByteString)
import           PlutusTx.Prelude                             hiding ((<$>))
import           Prelude                                      (IO, String, print, read)
import           System.Environment                           (getArgs)
import           Wallet.Emulator.Wallet                       (Wallet(..))

import           Configuration.PABConfig                      (pabWallet)
import           Contracts.CurrencyContract                   (SimpleMPS(SimpleMPS))
import           PABContracts                                 (PABContracts(..), MixerBackendContracts(..))
import           Requests


pabIP :: Text
pabIP = "127.0.0.1"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["mint", strTokenName, strAmount] -> mintCurrencyProcedure pabWallet 
            (TokenName $ stringToBuiltinByteString strTokenName) (read strAmount)  -- for testing purposes
        ["tickets", str]                  -> mintRelayTicketsProcedure $ read str
        ["retrieve"]                      -> go
        ["dispense"]                      -> dispenseProcedure
        _                                 -> print ("Unknown command" :: String)
  where
      go = do
          retrieveTimeLockedProcedure
          threadDelay 5_000_000

---------------------------------- Relayer logic ---------------------------------

mintCurrencyProcedure :: Wallet -> TokenName -> Integer -> IO ()
mintCurrencyProcedure w tn n = do
    cidCurrency <- activateRequest pabIP (BackendContracts MintCurrency) (Just w)
    endpointRequest pabIP "Create native token" cidCurrency (SimpleMPS tn n)

mintRelayTicketsProcedure :: Integer -> IO ()
mintRelayTicketsProcedure n = do
    cidMintTickets <- activateRequest pabIP (BackendContracts MixerRelay) (Just pabWallet)
    endpointRequest pabIP "Mint Relay Tickets" cidMintTickets (lovelaceValueOf 400_000, n)

retrieveTimeLockedProcedure :: IO ()
retrieveTimeLockedProcedure = do
    cidTimeLocked <- activateRequest pabIP (BackendContracts RetrieveTimeLocked) (Just pabWallet)
    endpointRequest pabIP "retrieve funds" cidTimeLocked ()

dispenseProcedure :: IO ()
dispenseProcedure = do
    void $ activateRequest pabIP (BackendContracts Dispense) (Just pabWallet)