{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Relayer where

import           GHC.Base                                 (undefined)
import           Ledger.Address                           (PaymentPubKeyHash, StakePubKeyHash)
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (IO)

import           Configuration.PABConfig                  (networkId, protocolParams)
import           IO.ChainIndex                            (ChainIndexCache (..), updateChainIndexCache)
import           IO.Wallet                                (balanceTx, submitTxConfirmed)
import           Types.MixerApp                           (mkMixerChainIndexCache, execMixerTx, MixerTxConstructor)
import           Types.MixerInput                         (MixerInput (..))
import           Types.TxConstructor                      (TxConstructor (..), mkTxConstructor)


relayServer :: IO ()
relayServer = do
    relayServerLoop mkMixerChainIndexCache []

relayServerLoop :: ChainIndexCache -> [MixerInput] -> IO ()
relayServerLoop cache inputs = do
    cache'  <- updateChainIndexCache cache
    inputs' <- updateMixerInputs inputs

    -- trying to find and submit admissible transaction
    let constrInit = mkTxConstructor relayerHashes (cacheTime cache') inputs' (cacheData cache') :: MixerTxConstructor
    case execMixerTx constrInit of
        Just constr -> case txConstructorResult constr of
            Just lookups -> do
                tx <- uncurry (balanceTx protocolParams networkId) lookups
                submitTxConfirmed tx
            Nothing -> return ()
        Nothing     -> return ()
    
    relayServerLoop cache' inputs'

updateMixerInputs :: [MixerInput] -> IO [MixerInput]
updateMixerInputs = undefined

relayerHashes :: (PaymentPubKeyHash, Maybe StakePubKeyHash)
relayerHashes = undefined