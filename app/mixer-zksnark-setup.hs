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


main :: IO ()
main = do
    (r1cs, wires) <- loadR1CSFile fileMerkleWithdrawQAP
    let sa = SetupArguments r1cs wires
    void $ generateCRS sa
