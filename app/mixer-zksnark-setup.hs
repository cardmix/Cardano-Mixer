{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main
    (
        main
    ) where

import           Control.Monad                                (void)
import           Prelude                                      (IO, String)
import           PlutusTx.Prelude

import           Crypto

r1csFile :: String
r1csFile = "circuit-mixer.json"
-- r1csFile = "circuit-test.json"

main :: IO ()
main = do
    r1cs <- loadR1CSFile r1csFile
    let sa = SetupArguments 9 34 21213 r1cs
    void $ generateCRS sa
