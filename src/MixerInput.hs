{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module MixerInput where

import           Data.Set                         (Set, fromList, isSubsetOf, notMember)
import           Ledger                           hiding (singleton, validatorHash, unspentOutputs)
import           PlutusTx.Prelude

import           Crypto
import           Crypto.Conversions               (dataToZp)
import           MixerInstance                    (MixerInstance)
import           SigmaProtocol


type WithdrawTokenNameParams = (Fr, Fr)

withdrawFirstTokenParams :: WithdrawTokenNameParams
withdrawFirstTokenParams = (toZp 0, toZp (-1))

data MixerInput = MixerInput
    {
        mtMixerInstance           :: MixerInstance,
        mtSigmaProtocolInput      :: SigmaProtocolInput,
        mtSigmaProtocolProof      :: SigmaProtocolProof,
        mtWithdrawTokenNameParams :: WithdrawTokenNameParams,
        mtWithdrawAddress         :: Address,
        mtNewLeaf                 :: Fr        
    }

type WithdrawTokenRedeemer = (SigmaProtocolInput, SigmaProtocolProof, (Fr, Fr, Address), Bool)

toWithdrawTokenRedeemer :: MixerInput -> WithdrawTokenRedeemer
toWithdrawTokenRedeemer mt = (mtSigmaProtocolInput mt, mtSigmaProtocolProof mt, (prev, next, mtWithdrawAddress mt), True)
    where (prev, next) = mtWithdrawTokenNameParams mt

mixerInputVerify :: Set Fr -> Set Fr -> MixerInput -> Bool
mixerInputVerify leafs keys (MixerInput _ inp@(ls, cur, _, aVal) proof (prev, next) addr _) = cond1 && cond2 && cond3 && cond4 && cond5
    where
        cond1 = sigmaProtocolVerify inp proof
        cond2 = prev < cur && cur < next
        cond3 = aVal == dataToZp addr
        cond4 = fromList ls `isSubsetOf` leafs
        cond5 = cur `notMember` keys

mixerInputFilter :: Set Fr -> Set Fr -> [MixerInput] -> [MixerInput]
mixerInputFilter leafs keys = filter (mixerInputVerify leafs keys)
