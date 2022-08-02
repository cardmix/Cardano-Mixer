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


module Types.MixerInput where

import           Cardano.Api                      (FromJSON, ToJSON)
import           Data.Set                         (Set, fromList, isSubsetOf, member)
import           GHC.Generics                     (Generic)
import           Ledger                           hiding (singleton, validatorHash, unspentOutputs, member)
import           PlutusTx.Prelude                 hiding ((<$>))
import           Prelude                          (Show)
import qualified Prelude

import           Crypto
import           Crypto.Conversions               (dataToZp)
import           MixerProofs.SigmaProtocol
import           Types.MixerInstance              (MixerInstance)


type WithdrawTokenNameParams = (BaseField, BaseField)

withdrawFirstTokenParams :: WithdrawTokenNameParams
withdrawFirstTokenParams = (toZp 0, toZp (-1))

data MixerInput = MixerInput
    {
        mtMixerInstance           :: MixerInstance,
        mtSigmaProtocolInput      :: SigmaProtocolInput,
        mtSigmaProtocolProof      :: SigmaProtocolProof,
        mtWithdrawTokenNameParams :: WithdrawTokenNameParams,
        mtWithdrawAddress         :: Address,
        mtNewLeaf                 :: BaseField        
    }
    deriving (Prelude.Eq, Show, Generic, FromJSON, ToJSON)

type WithdrawTokenRedeemer = (SigmaProtocolInput, SigmaProtocolProof, (BaseField, BaseField, Address), Bool)

toWithdrawTokenRedeemer :: MixerInput -> WithdrawTokenRedeemer
toWithdrawTokenRedeemer mt = (mtSigmaProtocolInput mt, mtSigmaProtocolProof mt, (prev, next, mtWithdrawAddress mt), True)
    where (prev, next) = mtWithdrawTokenNameParams mt

mixerInputVerify :: Set BaseField -> Set (BaseField, BaseField) -> MixerInput -> Bool
mixerInputVerify dKeys wKeys (MixerInput _ inp@(ls, cur, _, aVal) proof (prev, next) addr _) = cond1 && cond2 && cond3 && cond4 && cond5
    where
        cond1 = sigmaProtocolVerify testGens inp proof
        cond2 = prev < cur && cur < next
        cond3 = aVal == dataToZp addr
        cond4 = fromList ls `isSubsetOf` dKeys
        cond5 = (prev, next) `member` wKeys

mixerInputFilter :: Set BaseField -> Set (BaseField, BaseField) -> [MixerInput] -> [MixerInput]
mixerInputFilter dKeys wKeys = filter (mixerInputVerify dKeys wKeys)