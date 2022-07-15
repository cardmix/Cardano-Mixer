{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module MixerInstance where

import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           PlutusTx.Prelude
import           Prelude                                  (Show)
import qualified Prelude

import           Mixer                                    (Mixer)

---------------------------------- MixerInstance type -----------------------------------------

data MixerInstance = MixerInstance
    {
        miMixer                      :: Mixer,
        miMixerBeaconTxOutRef        :: TxOutRef,
        miWithdrawTxOutRef           :: TxOutRef,
        miMixerBeaconTokenName       :: TokenName,
        miMixerBeaconCurrencySymbol  :: CurrencySymbol,
        miDepositCurrencySymbol      :: CurrencySymbol,
        miWithdrawCurrencySymbol     :: CurrencySymbol,
        miMixerDepositAddress        :: Address,
        miMixerAddress               :: Address,
        miADAWithdrawAddress         :: Address,
        miMixerValidatorHash         :: ValidatorHash
    }
    deriving (Prelude.Eq, Show)

instance Eq MixerInstance where
    (==) = (Prelude.==)
