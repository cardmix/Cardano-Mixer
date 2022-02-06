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
import           PlutusTx.Prelude                    hiding (Eq, Ord, (<$>))
import           Prelude                             (IO, String, print, show, writeFile, FilePath )
import           System.Environment                  (getArgs)


import           Contracts.Vesting                   (vestingScriptHash)
import           Utils.Contracts                     (byteStringToList)

configPath :: FilePath
configPath = "src/Configuration/Constants/"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] -> let ValidatorHash h = vestingScriptHash
                 in writeFile (configPath ++ "vestingScriptHash") $ show $ byteStringToList h
        _     -> print ("unknown config stage" :: String)
