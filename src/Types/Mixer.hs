{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module Types.Mixer where

import           Cardano.Api (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Ada                               (lovelaceValueOf)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (Show)
import qualified Prelude

------------------------------------- Mixer type -----------------------------------------

-- Protocol fee is the fee that a relayer takes on every deposit/withdraw operation.
-- It's value should contaun enough ADA so that a UTXO with pure value is allowed by the ledger rules.
data Mixer = Mixer
    {
        mPureValue    :: !Value,
        mProtocolFee  :: !Value,
        mDepositFee   :: !Value,
        mWithdrawFee  :: !Value,
        mRoundsLeft   :: !Integer
    }
    deriving (Prelude.Eq, Show, Generic, FromJSON, ToJSON)

instance Eq Mixer where
    (==) = (Prelude.==)

PlutusTx.makeLift ''Mixer

mixerFixedDepositFee :: Value
mixerFixedDepositFee = lovelaceValueOf 1_500_000

mixerFixedWithdrawFee :: Value
mixerFixedWithdrawFee = lovelaceValueOf 1_500_000

mixerFromProtocol :: Value -> Integer -> Mixer
mixerFromProtocol v = Mixer (scale 1000 v) v (v + mixerFixedDepositFee) (v + mixerFixedWithdrawFee)

mixerValueBeforeDeposit :: Mixer -> Value
mixerValueBeforeDeposit (Mixer pv _ df wf r) = pv + scale r df + scale r wf

mixerValueAfterDeposit :: Mixer -> Value
mixerValueAfterDeposit (Mixer pv _ df wf r) = pv + scale (r-1) df + scale r wf

mixerValueAfterWithdraw :: Mixer -> Value
mixerValueAfterWithdraw (Mixer pv _ df wf r) = pv + scale (r-1) df + scale (r-1) wf
