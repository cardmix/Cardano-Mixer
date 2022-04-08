{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Types.MixerContractTypes where

import           Data.Aeson                               (FromJSON, ToJSON)
import           Data.Text                                (Text)
import           GHC.Generics                             (Generic)
import           Ledger                                   (Value)
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (Show (..))

import           Crypto

-- Parameters for the "deposit" endpoint
data DepositParams = DepositParams
    {
        dpAddress        :: !Text,
        dpValue          :: !Value,
        dpLeaf           :: !Fr
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- Parameters for the "withdraw" endpoint
data WithdrawParams = WithdrawParams
    {
        wpValue         :: !Value,
        wpDepositNum    :: !(Integer, Integer),
        wpAddress       :: !Text,
        wpPublicInputs  :: ![Fr],
        wpProof         :: !Proof
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)