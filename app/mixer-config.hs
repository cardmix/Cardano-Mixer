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
import           Utils.ByteString                     (byteStringToList, buildByteString)


main :: IO ()
main = do
    print $ textToAddress "addr_test1qrh8caw4kmlkwydwzdehpyw905dg8ayjv0vpe6vqmkkk5q3psddwydp9ea0gj3jawxyak3d238jpj9fxx3gnfhk7paxqnw2xmw"
    print $ byteStringToList $ buildByteString "ee7c75d5b6ff6711ae13737091c57d1a83f49263d81ce980ddad6a02"
    print $ byteStringToList $ buildByteString "f3a414dfd440869130c75fde00d16a5aa381f2029fc4764cf907b721"

    -- tMIX token
    print $ byteStringToList $ buildByteString "f21f17dd8a772ada1ead262823a224f1ec9dafad65dc6939cf3c4848"

    let ValidatorHash h = vestingScriptHash
    print $ byteStringToList h