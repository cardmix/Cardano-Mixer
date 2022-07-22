{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Relayer where

import           Control.Monad.Extra                      (mconcatMapM)
import           Data.Aeson                               (decodeFileStrict, encodeFile)
import           GHC.Base                                 (undefined)
import           Options.Applicative                      (execParser)
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (IO, print, FilePath, (<$>), show)

import           CommandLine                              (Command(..), commandParserInfo)
import           Configuration.RelayerConfig              (ledgerParams, relayerPKH, relayerSKH)
import           IO.ChainIndex                            (ChainIndexCache (..), updateChainIndexCache)
import           IO.Wallet                                (balanceTx, submitTxConfirmed, getWalletTxOutRefs)
import           Types.Mixer                              (Mixer(..))
import           Types.MixerApp                           (MixerTxConstructor, execMixerTx, mkMixerInstance, mkMixerChainIndexCache)
import           Types.MixerInput                         (MixerInput (..))
import           Types.MixerInstance                      (MixerInstance(..))
import           Types.TxConstructor                      (TxConstructor (..), mkTxConstructor)


relayServer :: IO ()
relayServer = do
    cmd <- execParser commandParserInfo
    case cmd of
      Serve          -> do
          print "Starting relayer server ..."
          relayServerLoop mkMixerChainIndexCache []
      MakeMixer file r -> do
          makeMixer file r

makeMixer :: FilePath -> Integer -> IO ()
makeMixer file r = do
    let fileV    = "../pools/" ++ file ++ ".json"
        fileMI i = "../pools/" ++ file ++ "/" ++ show i ++ ".json"
    v    <- fromMaybe (error ()) <$> decodeFileStrict fileV
    refs <- getWalletTxOutRefs ledgerParams relayerPKH relayerSKH (2*r)
    let mis = map (\i -> mkMixerInstance v i (refs !! (2*i)) (refs !! (2*i+1))) [0..r-1]
    mconcatMapM (\mi -> encodeFile (fileMI $ mRoundsLeft $ miMixer mi) mi) mis

relayServerLoop :: ChainIndexCache -> [MixerInput] -> IO ()
relayServerLoop cache inputs = do
    cache'  <- updateChainIndexCache cache
    inputs' <- updateMixerInputs inputs

    -- trying to find and submit admissible transaction
    let constrInit = mkTxConstructor (relayerPKH, Just relayerSKH) (cacheTime cache') inputs' (cacheData cache') :: MixerTxConstructor
    case execMixerTx constrInit of
        Just constr -> case txConstructorResult constr of
            Just res -> do
                tx <- uncurry (balanceTx ledgerParams) res
                submitTxConfirmed tx
            Nothing -> return ()
        Nothing     -> return ()

    relayServerLoop cache' inputs'

updateMixerInputs :: [MixerInput] -> IO [MixerInput]
updateMixerInputs = undefined
