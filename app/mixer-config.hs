{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}


module Main
    ( main
    ) where

import           Ledger.Scripts                      (ValidatorHash(ValidatorHash))
import           Ledger.Value                        (CurrencySymbol(..))
import           PlutusTx.Prelude                    hiding (Eq, Ord, (<$>))
import           Prelude                             (IO, String, print, show, writeFile, FilePath )
import           System.Environment                  (getArgs)

import           Scripts.VestingScript               (vestingScriptHash)
import           Tokens.RelayTicketToken             (relayTicketTokenSymbol)
import           Utils.Contracts                     (byteStringToList, buildByteString)


configPath :: FilePath
configPath = "src/Configuration/Constants/"

main :: IO ()
main = do
    print $ byteStringToList $ unCurrencySymbol relayTicketTokenSymbol
    print $ byteStringToList $ buildByteString "1635becc4c6640a3c9beaec5057dc45f186c29ab5f7805aa122f4f90"

    args <- getArgs
    case args of
        ["1"] -> let ValidatorHash h = vestingScriptHash
                 in writeFile (configPath ++ "vestingScriptHash") $ show $ byteStringToList h
        _     -> print ("unknown config stage" :: String)
