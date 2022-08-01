{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Types.Conversions where

import           Data.Map                                 (empty)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

import           IO.ChainIndex                            (ChainIndexCache (..))
import           Scripts.ADAWithdrawScript                (adaWithdrawAddress)
import           Scripts.MixerDepositScript               (mixerDepositAddress)
import           Scripts.MixerScript                      (mixerAddress, mixerValidatorHash)
import           Tokens.DepositToken                      (depositTokenSymbol)
import           Tokens.MixerBeaconToken                  (mixerBeaconCurrencySymbol, mixerBeaconTokenName)
import           Tokens.WithdrawToken                     (withdrawTokenSymbol)
import           Types.Mixer                              (Mixer, mixerFromProtocol)
import           Types.MixerInstance                      (MixerInstance (..))


mkMixerInstance :: Value -> Integer -> TxOutRef -> TxOutRef -> MixerInstance
mkMixerInstance v r = mkMixerInstance' (mixerFromProtocol v r)

mkMixerInstance' :: Mixer -> TxOutRef -> TxOutRef -> MixerInstance
mkMixerInstance' mixer beaconRef withdrawRef =
     MixerInstance
     {
        miMixer                      = mixer,
        miMixerBeaconTxOutRef        = beaconRef,
        miWithdrawTxOutRef           = withdrawRef,
        miMixerBeaconTokenName       = mixerBeaconTokenName,
        miMixerBeaconCurrencySymbol  = bSymb,
        miDepositCurrencySymbol      = dSymb,
        miWithdrawCurrencySymbol     = wSymb,
        miMixerDepositAddress        = mixerDepositAddress dSymb,
        miMixerAddress               = mixerAddress wSymb,
        miADAWithdrawAddress         = adaWithdrawAddress,
        miMixerValidatorHash         = mixerValidatorHash wSymb
     }
     where
          bSymb = mixerBeaconCurrencySymbol beaconRef
          dSymb = depositTokenSymbol (mixer, (bSymb, mixerBeaconTokenName), adaWithdrawAddress)
          wSymb = withdrawTokenSymbol (mixer, dSymb, adaWithdrawAddress, withdrawRef)

mkMixerChainIndexCache :: [MixerInstance] -> ChainIndexCache
mkMixerChainIndexCache mis = ChainIndexCache addrs empty zero
    where addrsMI acc mi = acc ++ map ($ mi) [miMixerDepositAddress, miMixerAddress, miADAWithdrawAddress]
          addrs          = foldl addrsMI [] mis
