{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Utils.Tx where

import           Cardano.Api.Shelley     (serialiseToCBOR)
import           Data.Aeson.Extras       (encodeByteString)
import           Data.Default            (def)
import           Data.Either             (fromRight)
import           Data.Text               (Text)
import           Ledger.Constraints      (UnbalancedTx)
import           Plutus.Contract.Wallet  (ExportTx (..), export)
import           PlutusTx.Prelude        hiding ((<>))
import           Prelude                 (undefined)

import           Configuration.PABConfig

unbalancedTxToCBOR :: UnbalancedTx -> Text
unbalancedTxToCBOR = encodeByteString . serialiseToCBOR . partialTx . fromRight (error ()) . f
    where f utx = case pabConfig of
            PABMainnet -> undefined
            PABTestnet -> export testnetParams testnetId def utx
