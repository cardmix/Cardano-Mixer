{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Main
    (
        main
    ) where

import           Prelude                                      (IO)

import           Configuration.QAPConfig                      (fileWithdrawR1CS)
import           Crypto


main :: IO ()
main = do
    (r1cs, wires) <- loadR1CSFile fileWithdrawR1CS
    let sa = SetupArguments r1cs wires
    compileQAP sa