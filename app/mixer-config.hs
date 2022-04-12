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
import           Prelude                             (IO, print)

import           Scripts.VestingScript               (vestingScriptHash)
import           Utils.Address                       (textToAddress)
import           Utils.Contracts                     (byteStringToList, buildByteString)


main :: IO ()
main = do
    print $ textToAddress "addr_test1qprx98hun3wa7xncxc85mfxu8peg02pr3ftj0nnaqtmtvk8n5s2dl4zqs6gnp36lmcqdz6j65wqlyq5lc3mye7g8kussq5skqf"
    print $ byteStringToList $ buildByteString "f3a414dfd440869130c75fde00d16a5aa381f2029fc4764cf907b721"

    -- tMIX token
    print $ byteStringToList $ buildByteString "f21f17dd8a772ada1ead262823a224f1ec9dafad65dc6939cf3c4848"

    let ValidatorHash h = vestingScriptHash
    print $ byteStringToList h