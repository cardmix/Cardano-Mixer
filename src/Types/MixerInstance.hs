{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Types.MixerInstance where

import           Cardano.Api                              (FromJSON, ToJSON)
import           GHC.Generics                             (Generic)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           PlutusTx.Prelude
import           Prelude                                  (Show)
import qualified Prelude

import           Types.Mixer                              (Mixer)
import           Utils.ByteString                         (ToBuiltinByteString(..), byteStringToInteger)

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
    deriving (Prelude.Eq, Show, Generic, FromJSON, ToJSON)

instance Eq MixerInstance where
    (==) = (Prelude.==)

mixerInstanceHash :: MixerInstance -> Integer
mixerInstanceHash mi = byteStringToInteger $ sha2_256 x
    where
        TxOutRef id1 n1 = miMixerBeaconTxOutRef mi
        TxOutRef id2 n2 = miWithdrawTxOutRef mi
        x = getTxId id1 `appendByteString` toBytes n1 `appendByteString` getTxId id2 `appendByteString` toBytes n2

findMixerInstanceByHash :: [MixerInstance] -> Integer -> Maybe MixerInstance
findMixerInstanceByHash mis h = find ((h ==) . mixerInstanceHash) mis