{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module SigmaProtocol where

import           PlutusTx.Prelude

import           Crypto
import           Crypto.Conversions               (dataToZp)
import           Utils.ByteString                 (ToBuiltinByteString(..))


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