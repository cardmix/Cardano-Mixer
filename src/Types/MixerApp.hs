{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Types.MixerApp (MixerAppType (..), newMixerApp, newMixerInstance, mixerApp) where

import           Control.Concurrent                       (threadDelay)
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check, error)
import           Prelude                                  (IO, error, undefined)

import           Configuration.RelayerConfig              (relayerPKH, relayerSKH, ledgerParams)
import           IO.ChainIndex                            (ChainIndexCache (..), updateChainIndexCache)
import           IO.Wallet                                (balanceTx, submitTxConfirmed)
import           MixerProofs.SigmaProtocol                (WithdrawRequest)
import           Types.MixerApp.Internal
import           Types.MixerTransactions                  (MixerTransaction, execTxs)
import           Types.TxConstructor                      (TxConstructor (..), mkTxConstructor)


getWithdrawRequests :: IO [WithdrawRequest]
getWithdrawRequests = undefined

mixerApp :: MixerApp -> IO ()
mixerApp app = do
    cache  <- updateChainIndexCache $ maCache app
    reqs   <- getWithdrawRequests
    let inputs = newMixerInputs app { maCache = cache } reqs

    -- trying to find and submit admissible transaction
    let constrInit = mkTxConstructor (relayerPKH, Just relayerSKH) (cacheTime cache) inputs (cacheData cache) :: MixerTransaction
    case execTxs (maTxs app) constrInit of
        Just constr -> case txConstructorResult constr of
            Just res -> do
                tx <- uncurry (balanceTx ledgerParams) res
                submitTxConfirmed tx
            Nothing -> error "Unexpected error"
        Nothing     -> threadDelay 1000000 -- no admissible transaction found

    mixerApp $ app { maInputs = inputs, maCache = cache }
