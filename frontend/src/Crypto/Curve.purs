module Crypto.Curve where

import Prelude hiding (add)

import Data.BigInt.Argonaut (BigInt, fromBase)
import Data.Group (class Group, ginverse)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested (type (/\), Tuple3, (/\))

import Crypto.Extension (class IrreducibleMonic, Extension(..)) 
import Crypto.Polynomial (toPoly)
import Crypto.Zp (class FiniteField, Zp, fromZp, toZp)
import Utility (two)

class (Ring t, Group t, Eq t) <= EllipticCurve t where
    aCurveCoef :: t
    gen        :: CurvePoint t

data CurvePoint t = CP t t | O

instance EllipticCurve t => Semigroup (CurvePoint t) where
    append p1 p2
            | p1 == p2  = dbl p1
            | otherwise = add p1 p2

instance EllipticCurve t => Monoid (CurvePoint t) where
    mempty = O

instance EllipticCurve t => Group (CurvePoint t) where
    ginverse O        = O
    ginverse (CP x y) = CP x (zero-y)

instance EllipticCurve t => Eq (CurvePoint t) where
    eq (CP x1 y1) (CP x2 y2) = x1 == x2 && y1 == y2
    eq (CP _ _) O            = false
    eq O          (CP _ _)   = false
    eq O          O          = true

add :: forall t . EllipticCurve t => CurvePoint t -> CurvePoint t -> CurvePoint t
add p O       = p
add O q       = q
add (CP x1 y1) (CP x2 y2)
  | x1 == x2  = O
  | otherwise = CP x3 y3
  where
    l  = (y1 - y2) * ginverse (x1 - x2)
    x3 = l * l - x1 - x2
    y3 = l * (x1 - x3) - y1

dbl :: forall t . EllipticCurve t => CurvePoint t -> CurvePoint t
dbl O         = O
dbl (CP x y)
  | y == zero = O
  | otherwise = CP x' y'
  where
    xx = x * x
    l  = (xx + xx + xx + aCurveCoef) * ginverse (y + y)
    x' = l * l - x - x
    y' = l * (x - x') - y

mul :: forall t p . EllipticCurve t => CurvePoint t -> Zp p -> CurvePoint t
-- mul p n = mul' p (fromZp n)                  -- multiplication using affine coordinates
mul p n = fromJ $ mul'' (toJ p) (fromZp n)      -- multiplication using Jacobian coordinates

-- {-# INLINABLE mul' #-}
-- mul' :: EllipticCurve t => CurvePoint t -> Integer -> CurvePoint t
-- mul' p n
--   | n < 0     = inv $ mul' p (-n)
--   | n == 0    = O
--   | n == 1    = p
--   | even n    = p'
--   | otherwise = add p p'
--   where
--     p' = mul' (dbl p) (divide n 2)

--------------------- Jacobian coordinates -----------------------------

mul'' :: forall t . EllipticCurve t => Tuple3 t t t -> BigInt -> Tuple3 t t t
mul'' p n
  | n < zero          = invJ $ mul'' p (negate n)
  | n == zero         = one /\ one /\ zero /\ unit
  | n == one          = p
  | mod n two == zero = mul'' (dblJ p) (div n two)
  | otherwise         = addJ p (mul'' (dblJ p) (div n two))

invJ :: forall t . EllipticCurve t => Tuple3 t t t -> Tuple3 t t t
invJ (x /\ y /\ z /\ unit) = x /\ (zero-y) /\ z /\ unit

addJ :: forall t . EllipticCurve t => (t /\ t /\ t /\ Unit) -> (t /\ t /\ t /\ Unit) -> (t /\ t /\ t /\ Unit)
addJ (x1 /\ y1 /\ z1 /\ _) (x2 /\ y2 /\ z2 /\ _)
          | z1 == zero = one /\ one /\ zero /\ unit
          | z2 == zero = one /\ one /\ zero /\ unit
          | otherwise  = x3 /\ y3 /\ z3 /\ unit
  where
    z1z1 = z1 * z1
    z2z2 = z2 * z2
    z1z2 = z1 + z2
    u1   = x1 * z2z2
    u2   = x2 * z1z1
    s1   = y1 * z2 * z2z2
    s2   = y2 * z1 * z1z1
    h    = u2 - u1
    h2   = two * h
    i    = h2 * h2
    j    = h * i
    r    = two * (s2 - s1)
    v    = u1 * i
    x3   = r * r - j - two * v
    y3   = r * (v - x3) - two * s1 * j
    z3   = (z1z2 * z1z2 - z1z1 - z2z2) * h    

dblJ :: forall t . EllipticCurve t => Tuple3 t t t -> Tuple3 t t t
dblJ (x1 /\ y1 /\ z1 /\ _)
        | z1 == zero = (one /\ one /\ zero /\ unit)
        | otherwise  = (x3 /\ y3 /\ z3 /\ unit)
  where
    two = one + one 
    three = two + one
    eight = three + three + two

    a    = aCurveCoef
    xx   = x1 * x1
    yy   = y1 * y1
    yyyy = yy * yy
    zz   = z1 * z1
    xy   = x1 + yy
    yz   = y1 + z1
    s    = two * (xy * xy - xx - yyyy)
    m    = three * xx + a * zz * zz
    t    = m * m - two * s
    x3   = t
    y3   = m * (s - t) - eight * yyyy
    z3   = yz * yz - yy - zz

toJ :: forall t . EllipticCurve t => CurvePoint t -> Tuple3 t t t
toJ (CP x y) = (x /\ y /\ one /\ unit)
toJ O        = (one /\ one /\ zero /\ unit)

fromJ :: forall t . EllipticCurve t => Tuple3 t t t -> CurvePoint t
fromJ (x /\ y /\ z /\ _)
          | z == zero = O
          | otherwise = let zz = z * z in CP (x * ginverse zz) (y * ginverse (z * zz))

data Q = Q

instance Semigroup Q where
  append _ _ = Q

instance Monoid Q where 
  mempty = Q

instance FiniteField Q where
  fieldPrime = const $ fromMaybe zero $ fromBase 16 "0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab"

-- BLS12381
type T1 = Zp Q
instance EllipticCurve T1 where
    aCurveCoef = zero
    gen = CP
      (toZp $ fromMaybe zero $ fromBase 16 "17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb")
      (toZp $ fromMaybe zero $ fromBase 16 "8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1")

data E2 = E2

instance Semigroup E2 where
    append _ _ = E2

instance Monoid E2 where
    mempty = E2

instance IrreducibleMonic T1 E2 where
    poly = const $ toPoly $ (one : zero : one : Nil)

-- BLS12381T
type T2 = Extension T1 E2
instance EllipticCurve T2 where
    aCurveCoef = zero
    gen = CP
      (E (toPoly (toZp (fromMaybe zero $ fromBase 16 "24aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8")
          : toZp (fromMaybe zero $ fromBase 16 "13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e")
          : Nil)))
      (E (toPoly (toZp (fromMaybe zero $ fromBase 16 "ce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801")
          : toZp (fromMaybe zero $ fromBase 16 "606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be")
          : Nil)))