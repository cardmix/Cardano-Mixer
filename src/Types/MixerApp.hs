{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Types.MixerApp where

import           Control.Concurrent                       (threadDelay)
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check, error)
import           Prelude                                  (IO, error)

import           Configuration.RelayerConfig              (relayerPKH, relayerSKH, ledgerParams)
import           IO.ChainIndex                            (ChainIndexCache (..), updateChainIndexCache)
import           IO.Wallet                                (balanceTx, submitTxConfirmed)
import           Types.MixerInput                         (MixerInput (..), updateMixerInputs)
import           Types.MixerInstance                      (MixerInstance (..))
import           Types.MixerTransactions                  (MixerTransactionBuilder, MixerTransaction, execTxs)
import           Types.TxConstructor                      (TxConstructor (..), mkTxConstructor)


data MixerApp = MixerApp
    {
        maInstances    :: [MixerInstance],
        maInputs       :: [MixerInput],
        maCache        :: ChainIndexCache,
        maTxs          :: [MixerTransactionBuilder]
    }

mixerApp :: MixerApp -> IO ()
mixerApp app = do
    cache  <- updateChainIndexCache $ maCache app
    inputs <- updateMixerInputs $ maInputs app

    -- trying to find and submit admissible transaction
    let constrInit = mkTxConstructor (relayerPKH, Just relayerSKH) (cacheTime cache) inputs (cacheData cache) :: MixerTransaction
    case execTxs (maTxs app) constrInit of
        Just constr -> case txConstructorResult constr of
            Just res -> do
                tx <- uncurry (balanceTx ledgerParams) res
                submitTxConfirmed tx
            Nothing -> error "Unexpected error"
        Nothing     -> threadDelay 1000000 -- no admissible transaction found

    let app' = app { maInputs = inputs, maCache = cache }
    mixerApp app'
