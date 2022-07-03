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


module Mixer where

import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Plutus.V1.Ledger.Ada                     (lovelaceValueOf)
import           PlutusTx
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

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

PlutusTx.makeLift ''Mixer

mixerFixedDepositFee :: Value
mixerFixedDepositFee = lovelaceValueOf 1_500_000

mixerFixedWithdrawFee :: Value
mixerFixedWithdrawFee = lovelaceValueOf 1_500_000

mixerFromProtocol :: Value -> Integer -> Mixer
mixerFromProtocol v = Mixer (scale 1000 v) v (v + mixerFixedDepositFee) (v + mixerFixedWithdrawFee)

mixerValueAfterDeposit :: Mixer -> Value
mixerValueAfterDeposit (Mixer pv _ df wf r) = pv + scale (r-1) df + scale r wf

mixerValueAfterWithdraw :: Mixer -> Value
mixerValueAfterWithdraw (Mixer pv _ df wf r) = pv + scale (r-1) df + scale (r-1) wf