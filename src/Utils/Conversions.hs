{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE NumericUnderscores         #-}

module Utils.Conversions where

import           Cardano.Api.Shelley    (ProtocolParameters, NetworkId(..), NetworkMagic(..), serialiseToCBOR)
import           Data.Aeson             (decode)
import           Data.Aeson.Extras      (encodeByteString)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Either            (fromRight)
import           Data.FileEmbed         (embedFile)
import           Data.Text              (unpack)
import           Ledger.Constraints     (UnbalancedTx)
import           Plutus.Contract.Wallet (ExportTx (..), export)
import           PlutusTx.Prelude        hiding ((<>))
import           Prelude                 (String)


testnetId :: NetworkId
testnetId = Testnet $ NetworkMagic 1097911063

testnetParams :: ProtocolParameters
testnetParams = fromMaybe (error ()) $ decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")

unbalancedTxToCBOR :: UnbalancedTx -> String
unbalancedTxToCBOR = unpack . encodeByteString . serialiseToCBOR . partialTx . fromRight (error ()) . export testnetParams testnetId
