{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}


module Main
    ( main
    ) where

import           Plutus.PAB.Effects.Contract.Builtin (handleBuiltin)
import           Plutus.PAB.Run                      (runWith)
import           Prelude                             (IO)

import           PABContracts                        (PABContracts)


main :: IO ()
main = runWith (handleBuiltin @PABContracts)