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

import           Control.Monad                                (void)
import           Data.Text                                    (Text)
import           Ledger.Value                                 (TokenName(..), Value)
import           Plutus.V1.Ledger.Ada                         (lovelaceValueOf)
import           PlutusTx.Builtins.Class                      (stringToBuiltinByteString)
import           PlutusTx.Prelude                             hiding ((<$>))
import           Prelude                                      (IO, String, print, read)
import           System.Environment                           (getArgs)
import           Wallet.Emulator.Wallet                       (Wallet(..))

import           Configuration.PABConfig                      (pabWallet, dispenserWallet)
import           Contracts.CurrencyContract                   (SimpleMPS(SimpleMPS))
import           PABContracts                                 (PABContracts(..), MixerBackendContracts(..), MixerFrontendContracts(..))
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
        ["retrieve"]                      -> retrieveTimeLockedProcedure
        ["dispense"]                      -> dispenseProcedure
        ["state"]                         -> stateProcedure
        ["startup"]                       -> do
            retrieveTimeLockedProcedure
            dispenseProcedure
        _                                 -> print ("Unknown command" :: String)

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
    void $ activateRequest pabIP (BackendContracts RetrieveTimeLocked) (Just pabWallet)

dispenseProcedure :: IO ()
dispenseProcedure = do
    void $ activateRequest pabIP (BackendContracts Dispense) (Just dispenserWallet)

stateProcedure :: IO ()
stateProcedure = do
    cidMixerState <- activateRequest pabIP (FrontendContracts MixerStateQuery) Nothing
    endpointRequest pabIP "get-mixer-state" cidMixerState ([] :: [Value])