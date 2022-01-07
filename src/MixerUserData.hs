{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module MixerUserData where

import           Data.Aeson                       (FromJSON, ToJSON)
import           GHC.Generics                     (Generic)
import           PlutusTx.Prelude                 hiding ((<*>), Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger)
import           Prelude                          (IO, (<$>), (<*>), Show)

import           Crypto


data DepositSecret = DepositSecret
    {
        getR1 :: Fr,
        getR2 :: Fr,
        getN  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

generateDepositSecret :: Integer -> IO DepositSecret
generateDepositSecret n = do
    ds <- DepositSecret <$> generateFr <*> generateFr
    return $ ds n

data ShieldedAccountSecret = ShieldedAccountSecret
    {
        getV1 :: Fr,
        getV2 :: Fr,
        getV3 :: Fr
    } deriving (Show, Generic, FromJSON, ToJSON)

generateShieldedAccountSecret :: IO ShieldedAccountSecret
generateShieldedAccountSecret = ShieldedAccountSecret <$> generateFr <*> generateFr <*> generateFr











