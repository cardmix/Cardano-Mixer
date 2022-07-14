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
import           Utils.ByteString                 (ToBuiltinByteString(..))

---------------------------------- MixerInput type -----------------------------------------

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

------------------------------------ Sigma protocol ----------------------------------------------

-- leafs, key, addrExp, and addrValue
type SigmaProtocolInput = ([Fr], Fr, Fr, Fr)

-- triple of commitments
type SigmaProtocolCommit = ([Fr], [Fr], [Fr])

-- commitments, errors, and responses
type SigmaProtocolProof = (SigmaProtocolCommit, [Fr], [Fr])

g1 :: Fr
g1 = Zp 3

g2 :: Fr
g2 = Zp 5

g3 :: Fr
g3 = Zp 7

{-# INLINABLE sigmaProtocolChallenge #-}
sigmaProtocolChallenge :: SigmaProtocolProof -> Fr
sigmaProtocolChallenge ((as, bs, cs), _, xs) = dataToZp $ sha2_256 $ toBytes $ map fromZp (as ++ bs ++ cs ++ xs)

-- TODO: check lengths of the arrays
{-# INLINABLE sigmaProtocolVerify #-}
sigmaProtocolVerify :: SigmaProtocolInput -> SigmaProtocolProof -> Bool
sigmaProtocolVerify (leafs, key, addrExp, addr) proof@((as, bs, cs), es, xs) = eq1 && eq2 && eq3 && eq4
    where
        ys  = xs
        zs  = map (addr *) xs
        s   = sigmaProtocolChallenge proof
        eq1 = all (\(a, (e, (x, l))) -> pow g1 (fromZp x) == a * pow l (fromZp e)) $ zip as $ zip es $ zip xs leafs
        eq2 = all (\(b, (e, y)) -> pow g2 (fromZp y) == b * pow key (fromZp e)) $ zip bs $ zip es ys
        eq3 = all (\(c, (e, z)) -> pow g3 (fromZp z) == c * pow addrExp (fromZp e)) $ zip cs $ zip es zs
        eq4 = s == sum es