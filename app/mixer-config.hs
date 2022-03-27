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
import           Prelude                             (IO, print)

import           Scripts.VestingScript               (vestingScriptHash)
import           Tokens.RelayTicketToken             (relayTicketTokenSymbol)
import           Utils.Contracts                     (byteStringToList, buildByteString)


main :: IO ()
main = do
    print $ byteStringToList $ unCurrencySymbol relayTicketTokenSymbol

    print $ byteStringToList $ buildByteString "2c922c0b34abf5ab0d7f3f290b9f6c8874a4d300"

    let ValidatorHash h = vestingScriptHash
    print $ byteStringToList h