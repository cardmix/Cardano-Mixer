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
import           Data.Text                                    (Text)
import           Plutus.V1.Ledger.Ada                         (lovelaceValueOf)
import           PlutusTx.Prelude                             hiding ((<$>))
import           Prelude                                      (IO, String, print, read)
import           System.Environment                           (getArgs)
import           Wallet.Emulator                              (mockWalletPaymentPubKeyHash)

import           Configuration.PABConfig                      (pabWallet, pabWalletIdString)
import           Contracts.Currency                           (SimpleMPS(SimpleMPS))
import           MixerContractsDefinition                     (MixerContractsDefinition(..), Wallet(..))
import           Requests
import           Tokens.AdminToken                            (adminTokenName)



pabIP :: Text
pabIP = "127.0.0.1"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["retrieve"] -> go
        ["tickets", str] -> mintRelayTicketsProcedure $ read str
        ["admin"]    -> mintAdminKeyProcedure $ Wallet pabWalletIdString   -- for testing purposes
        _            -> print ("Unknown command" :: String)

  where
      go = do
          retrieveTimeLockedProcedure
          threadDelay 5_000_000

---------------------------------- Relayer logic ---------------------------------

mintRelayTicketsProcedure :: Integer -> IO ()
mintRelayTicketsProcedure n = do
    cidMintTickets <- activateRequest pabIP MixerRelay (Just $ Wallet pabWalletIdString)
    endpointRequest pabIP "Mint Relay Tickets" cidMintTickets (lovelaceValueOf 400_000, n)

retrieveTimeLockedProcedure :: IO ()
retrieveTimeLockedProcedure = do
    cidTimeLocked <- activateRequest pabIP RetrieveTimeLocked (Just $ Wallet pabWalletIdString)
    endpointRequest pabIP "retrieve funds" cidTimeLocked ()


----------------------- Create mixer admin key -----------------------------------

mintAdminKeyProcedure :: Wallet -> IO ()
mintAdminKeyProcedure w = do
    cidAdmin <- activateRequest pabIP MintAdminKey (Just w)
    endpointRequest pabIP "Create native token" cidAdmin (SimpleMPS adminTokenName 1 (mockWalletPaymentPubKeyHash pabWallet))