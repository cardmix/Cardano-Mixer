{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}


module Main
    (
        main
    ) where

import           Control.Monad                                (void)
import           Prelude                                      (IO)
import           PlutusTx.Prelude

import           Configuration.QAPConfig                      (fileWithdrawR1CS)
import           Crypto



main :: IO ()
main = do
    (r1cs, wires) <- loadR1CSFile fileWithdrawR1CS
    let sa = SetupArguments r1cs wires
    void $ generateCRS sa
