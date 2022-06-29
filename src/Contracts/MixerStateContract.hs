{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Contracts.MixerStateContract where

import           Cardano.Api                              (FromJSON, ToJSON)
import qualified Data.Map
import           Data.Semigroup                           (Last (..))
import           GHC.Generics                             (Generic)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Plutus.Contract                          (Promise, Contract, ContractError, Endpoint, currentTime, endpoint, tell, logInfo)
import           PlutusTx
import           PlutusTx.Prelude                         hiding (Semigroup, (<>), (<$>), unless, find, toList, fromInteger, check)
import           Prelude                                  (Show, String, (<$>))

import           Contracts.ChainIndex                     (getUtxosAt)
import           Crypto
import           MixerState                               (MixerState, constructStateFromList)
import           Scripts.FailScript                       (failAddress)
import           Scripts.MixerScript                      (Mixer (..), mixerAddress)
import           Scripts.VestingScript                    (vestingValidatorHash, vestingValidatorAddress)

--------------------------- Types -----------------------------------

data MixerStateCache = MixerStateCache
    {
        mixerStateCacheTxs  :: [ChainIndexTxOut],
        mixerStateCacheTime :: POSIXTime
    }
    deriving (Show, Generic, FromJSON, ToJSON)

-- cache validity is 10s
cacheValidityPeriod :: POSIXTime
cacheValidityPeriod = 10000

-- instance Ord (ChainIndexTx, ChainIndexTxOut) where
--     (compare) (tx1, txo1) (tx2, txo2) = t1 `compare` t2
--         where
--             t1 = ivTo $ _citxValidRange tx1
--             t2 = ivTo $ _citxValidRange tx2

--------------------------- Off-Chain -------------------------------

-- TODO: update the implementation
getMixerState :: MixerStateCache -> POSIXTime -> Value -> Contract w s ContractError (MixerState, MixerStateCache)
getMixerState oldCache@(MixerStateCache cTxs cTime) curTime v = do
    let mixer = Mixer v vestingValidatorHash vestingValidatorHash failAddress
        addr  = mixerAddress mixer

    logInfo curTime
    logInfo cTime
    logInfo $ curTime - cTime <= cacheValidityPeriod
    txTxos  <- mixerStateCacheIsValid curTime (pure cTxs) (map fst . Data.Map.elems <$> getUtxosAt vestingValidatorAddress)
    cache   <- mixerStateCacheIsValid curTime (pure oldCache) (pure $ MixerStateCache txTxos curTime)

    logInfo @String "Got txos and cache"

    -- TODO: implement proper sort?
    let outs  = txTxos
    let f o = do
            d  <- either (const Nothing) Just $ _ciTxOutDatum o
            (leaf, t) <- fromBuiltinData $ getDatum d :: Maybe (Fr, POSIXTime)
            -- TODO: change here!
            -- let tokenCheck = _ciTxOutValue o `geq` depositToken (mixer, addr) ((t, leaf), zero, TokenName "", PaymentPubKeyHash $ PubKeyHash "")
            -- if tokenCheck then Just leaf else Nothing
            Just leaf
        leafs = mapMaybe f outs
        state = snd $ constructStateFromList (leafs, [])
    return (state, cache)
  where
      mixerStateCacheIsValid :: POSIXTime -> Contract w s ContractError a -> Contract w s ContractError a -> Contract w s ContractError a
      mixerStateCacheIsValid ct y n = if ct - cTime <= cacheValidityPeriod then y else n

type MixerStateSchema = Endpoint "get-mixer-state" [Value]

getMixerStatePromise :: Promise (Maybe (Last [MixerState])) MixerStateSchema ContractError ()
getMixerStatePromise = endpoint @"get-mixer-state" @[Value] $ \vals -> do
    curTime <- currentTime
    (_, cache@(MixerStateCache _ ct)) <- getMixerState (MixerStateCache [] 0) curTime zero
    logInfo @String "Cached txos"
    logInfo $ curTime - ct
    states <- mapM (fmap fst . getMixerState cache curTime) vals
    logInfo @String "Retrieved states"
    logInfo states
    tell $ Just $ Last states

    -- mixerStateLoop (MixerStateCache [] 0)
    -- where defVals = map lovelaceValueOf [40_000, 60_000, 80_000, 100_000] ++ map (`scale` mixToken) [20, 40, 60]
    --       mixerStateLoop oldCache = do
    --         logInfo @String "Enter mixerStateLoop"
    --         curTime <- currentTime
    --         (_, cache) <- getMixerState oldCache curTime zero
    --         states <- mapM (fmap fst . getMixerState cache curTime) defVals
    --         tell $ Just $ Last states
    --         logInfo @String "Exit mixerStateLoop"
    --         _ <- waitNSlots 30
    --         mixerStateLoop cache
