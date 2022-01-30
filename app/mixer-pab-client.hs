{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main
    (
        main
    ) where

import           Control.Concurrent                           (threadDelay)
import           PlutusTx.Prelude                             hiding ((<$>))
import           Prelude                                      (IO)

import           Configuration.PABConfig                      (pabWallet)
import           MixerContractsDefinition
import           Requests


main :: IO ()
main = do
    go
  where
      go = do
          retrieveTimeLockedProcedure
          threadDelay 5_000_000

---------------------------------- Relayer logic ---------------------------------

retrieveTimeLockedProcedure :: IO ()
retrieveTimeLockedProcedure = do
    cidTimeLocked <- activateRequest RetrieveTimeLocked (Just pabWallet)
    endpointRequest "retrieve funds" cidTimeLocked ()


    