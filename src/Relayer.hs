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
import           Ledger.Constraints                       (TxConstraints, ScriptLookups)
import           Ledger.Tx                                (CardanoTx)
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (IO)

import           Contracts.ChainIndex                     (ChainIndexCache (..), updateChainIndexCache)
import           MixerApp                                 (mkMixerChainIndexCache, execMixerTx)
import           MixerInput                               (MixerInput (..))
import           Types.TxConstructor                      (TxConstructor (..), mkTxConstructor)

relayServer :: IO ()
relayServer = do
    relayServerLoop mkMixerChainIndexCache []

relayServerLoop :: ChainIndexCache -> [MixerInput] -> IO ()
relayServerLoop cache inputs = do
    cache'  <- updateChainIndexCache cache
    inputs' <- updateMixerInputs inputs

    -- trying to find and submit admissible transaction
    let constrInit = mkTxConstructor relayerHashes (cacheTime cache') inputs' (cacheData cache')
    case execMixerTx constrInit of
        Just constr -> case txConstructorResult constr of
            Just lookups -> do
                tx <- balanceTx lookups
                submitTxConfirmed tx
            Nothing -> return ()
        Nothing     -> return ()
    
    relayServerLoop cache' inputs'

updateMixerInputs :: [MixerInput] -> IO [MixerInput]
updateMixerInputs = undefined

relayerHashes :: (PaymentPubKeyHash, Maybe StakePubKeyHash)
relayerHashes = undefined

balanceTx :: (ScriptLookups a, TxConstraints i o) -> IO CardanoTx
balanceTx = undefined

submitTx :: CardanoTx -> IO ()
submitTx = undefined

submitTxConfirmed :: CardanoTx -> IO ()
submitTxConfirmed = undefined