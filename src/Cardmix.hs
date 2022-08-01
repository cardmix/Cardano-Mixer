{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Cardmix where

import           Data.Aeson                               (decodeFileStrict, encodeFile)
import           GHC.Base                                 (undefined)
import           Options.Applicative                      (execParser)
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (IO, FilePath, (<$>), print)

import           CommandLine                              (Command(..), commandParserInfo)
import           Configuration.RelayerConfig              (ledgerParams, relayerPKH, relayerSKH)
import           IO.Wallet                                (getWalletTxOutRefs)
import           Types.Conversions                        (mkMixerInstance, mkMixerChainIndexCache)
import           Types.MixerApp                           (MixerApp(..), mixerApp)
import           Types.MixerTransactions                  (mixerTxs, mixerMakeTxs)


cardmix :: IO ()
cardmix = do
    cmd <- execParser commandParserInfo
    case cmd of
      MakeMixer file r -> makeMixer file r
      Serve -> startAPIServer
      Relay -> startRelayer

makeMixer :: FilePath -> Integer -> IO ()
makeMixer file r = do
    let fileV   = "pools/values/" ++ file ++ ".json"
        fileMIS = "pools/instances/" ++ file ++ ".json"
    v    <- fromMaybe (error ()) <$> decodeFileStrict fileV
    refs <- getWalletTxOutRefs ledgerParams relayerPKH relayerSKH (2*r)
    let mis = map (\i -> mkMixerInstance v i (refs !! (2*i)) (refs !! (2*i+1))) [0..r-1]
    encodeFile fileMIS mis
    let app = MixerApp mis [] (mkMixerChainIndexCache []) (mixerMakeTxs mis)
    mixerApp app

startAPIServer :: IO ()
startAPIServer = do
  print "Starting a relay API server ..."
  undefined -- TODO: implement API server

startRelayer :: IO ()
startRelayer = do
  print "Starting a relayer app ..."

  -- loading mixer instances
  let mis = undefined -- TODO: load mixer instances

  let app = MixerApp mis [] (mkMixerChainIndexCache mis) (mixerTxs mis)
  mixerApp app