{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Types.MixerApp.Internal where

import           Data.List                                (find)
import           Data.Map                                 (empty)
import           Data.Maybe                               (catMaybes)
import           Data.Set                                 (Set, fromList)
import           Ledger                                   (Value, TxOutRef)
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check, concatMap)
import           Prelude                                  (concatMap)

import           IO.ChainIndex                            (ChainIndexCache (..))
import           MixerProofs.SigmaProtocol                (WithdrawRequest (..), BaseField)
import           Scripts.ADAWithdrawScript                (adaWithdrawAddress)
import           Scripts.MixerDepositScript               (mixerDepositAddress)
import           Scripts.MixerScript                      (mixerAddress, mixerValidatorHash)
import           Tokens.DepositToken                      (depositTokenSymbol, depositKeys)
import           Tokens.MixerBeaconToken                  (mixerBeaconCurrencySymbol, mixerBeaconTokenName)
import           Tokens.WithdrawToken                     (withdrawTokenSymbol, withdrawKeys)
import           Types.Mixer                              (Mixer (..), mixerFromProtocol)
import           Types.MixerInput                         (MixerInput (..), mixerInputFilter)
import           Types.MixerTransactions                  (MixerTransactionBuilder, mixerTxs, mixerMakeTxs)
import           Types.MixerInstance                      (MixerInstance (..), findMixerInstanceByHash)
import           Utils.Address                            (bech32ToAddress)


data MixerAppType = RelayApp | MakeMixerApp

data MixerApp = MixerApp
    {
        maInstances    :: [MixerInstance],
        maInputs       :: [MixerInput],
        maCache        :: ChainIndexCache,
        maTxs          :: [MixerTransactionBuilder]
    }


newMixerInstance :: Value -> Integer -> TxOutRef -> TxOutRef -> MixerInstance
newMixerInstance v r = newMixerInstance' (mixerFromProtocol v r)

newMixerInstance' :: Mixer -> TxOutRef -> TxOutRef -> MixerInstance
newMixerInstance' mixer beaconRef withdrawRef =
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

newMixerChainIndexCache :: [MixerInstance] -> ChainIndexCache
newMixerChainIndexCache mis = ChainIndexCache addrs empty zero
    where addrsMI acc mi = acc ++ map ($ mi) [miMixerDepositAddress, miMixerAddress, miADAWithdrawAddress]
          addrs          = foldl addrsMI [] mis

newMixerApp :: MixerAppType -> [MixerInstance] -> MixerApp
newMixerApp appType mis = case appType of
    RelayApp     -> MixerApp mis [] (newMixerChainIndexCache mis) (mixerTxs mis)
    MakeMixerApp -> MixerApp mis [] (newMixerChainIndexCache [])  (mixerMakeTxs mis)

getDepositKeys :: MixerApp -> Set BaseField
getDepositKeys MixerApp { maCache = cache, maInstances = mis } = fromList $ concatMap (`depositKeys` cacheData cache) mis

getWithdrawKeys :: MixerApp -> Set (BaseField, BaseField)
getWithdrawKeys MixerApp { maCache = cache, maInstances = mis } = fromList $ concatMap (`withdrawKeys` cacheData cache) mis

fromWithdrawRequest :: [MixerInstance] -> Set (BaseField, BaseField) -> WithdrawRequest -> Maybe MixerInput
fromWithdrawRequest mis wts (WithdrawRequest h spi spp wopt) = do
        mi <- findMixerInstanceByHash mis h
        let v = mPureValue (miMixer mi)
            r = mRoundsLeft (miMixer mi)
            (_, wKey, _, _) = spi
        miNext <- find (\a -> mPureValue (miMixer a) == v && mRoundsLeft (miMixer a) == r-1) mis
        wPar   <- find (\(prev, next) -> prev < wKey && wKey < next) wts
        addr   <- either bech32ToAddress (pure . const (miMixerDepositAddress miNext)) wopt
        let key = either (const zero) id wopt
        return $ MixerInput mi spi spp wPar addr key

newMixerInputs :: MixerApp -> [WithdrawRequest] -> [MixerInput]
newMixerInputs app reqs =
    let dKeys  = getDepositKeys app
        wKeys  = getWithdrawKeys app
        newIns = catMaybes $ map (fromWithdrawRequest (maInstances app) wKeys) reqs
    in mixerInputFilter dKeys wKeys $ maInputs app ++ newIns
        