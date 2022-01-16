module Crypto.BLS12381 where

import Prelude hiding (conj)

import Data.BigInt.Argonaut (BigInt, fromBase, fromInt)
import Data.Group (ginverse)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Utility (randomBigInt)



import Crypto.Curve (CurvePoint(..), T1, T2)
import Crypto.Extension (class IrreducibleMonic, Extension(..), conj, embed, embed2, frob, powUnitary)
import Crypto.Polynomial (toPoly)
import Crypto.Zp (class FiniteField, Zp, fieldPrime, toZp)

----------------------------------- BLS12381 --------------------------------

data R = R

instance Semigroup R where
  append _ _ = R

instance Monoid R where
  mempty = R

type Fr = Zp R

instance FiniteField R where
  fieldPrime = const $ fromMaybe zero $ fromBase 16 "73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001"

generateFr :: Effect Fr
generateFr = toZp <$> (randomBigInt (fieldPrime (mempty :: R) - one))

data E6 = E6

instance Semigroup E6 where
    append _ _ = E6

instance Monoid E6 where
    mempty = E6

instance IrreducibleMonic T2 E6 where
    poly = const $ toPoly (E (zero - 
      toPoly (toZp (fromMaybe zero $ fromBase 16 "d0088f51cbff34d258dd3db21a5d66bb23ba5c279c2895fb39869507b587b120f55ffff58a9ffffdcff7fffffffd556")
        : toZp (fromMaybe zero $ fromBase 16 "d0088f51cbff34d258dd3db21a5d66bb23ba5c279c2895fb39869507b587b120f55ffff58a9ffffdcff7fffffffd555")
        : Nil)) : zero : zero : one : Nil)

data E12 = E12

instance Semigroup E12 where
    append _ _ = E12

instance Monoid E12 where
    mempty = E12

instance IrreducibleMonic (Extension T2 E6) E12 where
    poly = const $ toPoly (E (toPoly (zero : (zero-one) : Nil)) : zero : one : Nil)

--------------------------------- Pairing BLS12381 -------------------------------------

type TT = Extension (Extension T2 E6) E12

pairing :: CurvePoint T1 -> CurvePoint T2 -> TT
pairing p1 p2 = finalExponentiationBLS12 parameterHex $ 
  millerAlgorithmBLS12 parameterBin p1 p2

-- | [Miller algorithm for Barreto-Lynn-Scott degree 12 curves]
-- (https://eprint.iacr.org/2016/130.pdf).
millerAlgorithmBLS12 :: List BigInt -> CurvePoint T1 -> CurvePoint T2 -> TT
millerAlgorithmBLS12 (x:xs) p q = snd $
    millerLoop p q xs (Tuple (if x > zero then q else ginverse q) mempty)
millerAlgorithmBLS12 _ _ _      = mempty

-- Miller loop, line 2 to line 10.
millerLoop :: CurvePoint T1 -> CurvePoint T2 -> List BigInt -> Tuple (CurvePoint T2) TT ->  Tuple (CurvePoint T2) TT
millerLoop p q = millerLoop'
  where
    millerLoop' Nil    tf = tf
    millerLoop' (x:xs) tf = case doublingStep p tf of
      tf2
        | x == zero -> millerLoop' xs tf2
        | x == one  -> millerLoop' xs $ additionStep p q tf2
        | otherwise -> millerLoop' xs $ additionStep p (ginverse q) tf2

-- Doubling step, line 4.
doublingStep :: CurvePoint T1 -> Tuple (CurvePoint T2) TT -> Tuple (CurvePoint T2) TT
doublingStep p (Tuple t f) = (<>) f <<< (<>) f <$> lineFunction p t t

-- Addition step, line 6 and line 8.
additionStep :: CurvePoint T1 -> CurvePoint T2 -> Tuple (CurvePoint T2) TT -> Tuple (CurvePoint T2) TT
additionStep p q (Tuple t f) = (<>) f <$> lineFunction p q t

-- | [Final exponentiation for Barreto-Lynn-Scott degree 12 curves]
-- (https://eprint.iacr.org/2016/130.pdf).
finalExponentiationBLS12 :: BigInt -> TT -> TT
finalExponentiationBLS12 u = hardPart <<< easyPart
  where
    easyPart = p2 <<< p6
      where
        p6 f = conj f * ginverse f           -- f^(p^6 - 1) 
        p2 f = f * (frob <<< frob) f         -- f^(p^2 + 1)
    hardPart f = p4
      where
        f2 = f * f                                        -- f^2
        y3 = powUnitary (powUnitary f u * conj f2) u * f  -- f^(lambda_3)
        y2 = powUnitary y3 u                              -- f^(lambda_2)
        y1 = powUnitary y2 u * conj y3                    -- f^(lambda_1)
        y0 = powUnitary y1 u * f2 * f                     -- f^(lambda_0)
        p4 = y0 * frob (y1 * frob (y2 * frob y3))         -- f^((p^4 - p^2 + 1) / r)

-- | BLS12381 curve parameter @s = t@ in signed binary.
parameterBin :: List BigInt
parameterBin = map fromInt (-1: -1:  0: -1: 0: 0: -1: 0: 0: 0: 0: 0: 0: 0: 0: -1: 0
                  :  0:  0:  0: 0: 0:  0: 0: 0: 0: 0: 0: 0: 0: 0:  0: 0
                  :  0:  0:  0: 0: 0:  0: 0: 0: 0: 0: 0: 0: 0: 0: -1: 0
                  :  0:  0:  0: 0: 0:  0: 0: 0: 0: 0: 0: 0: 0: 0:  0: Nil)

-- | BLS12381 curve parameter @t@ in hexadecimal.
parameterHex :: BigInt
parameterHex = - (fromMaybe zero $ fromBase 16 "d201000000010000")

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Line function evaluation @Line(T, Q, P)@.
--
-- Compute the line function between two points @T@ and @Q@ in @G2@,
-- evaluate the line function at a point @P@ in @G1@,
-- and embed the line function evaluation in @GT@.
lineFunction :: CurvePoint T1 -> CurvePoint T2 -> CurvePoint T2 -> Tuple (CurvePoint T2) TT -- ^ Points @T + Q@ and @Line(T, Q, P)@.
lineFunction (CP x y) (CP x1 y1) (CP x2 y2) =
    if x1 /= x2
        then Tuple (CP x3  y3)  (E (toPoly (embed2 (zero - y) : E (toPoly (embed x * l : (y1 - l  * x1) : Nil)) : Nil)))
    else if y1 + y2 == zero
        then Tuple O            (E (toPoly (embed2          x : embed (zero - x1) : Nil)))
    else Tuple (CP x3' y3') (E (toPoly (embed2 (zero - y) : E (toPoly (embed x * l' : (y1 - l' * x1) : Nil)) : Nil)))
  where
    l   = (y2 - y1) * ginverse (x2 - x1)
    x3  = l * l - x1 - x2
    y3  = l * (x1 - x3) - y1
    x12 = x1 * x1
    l'  = (x12 + x12 + x12) * ginverse (y1 + y1)
    x3' = l' * l' - x1 - x2
    y3' = l' * (x1 - x3') - y1
lineFunction _ _ _ = Tuple O mempty