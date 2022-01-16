module Crypto.Extension where

import Prelude hiding (conj)

import Crypto.Polynomial (Polynomial, degPoly, fromConst, leading, qr, toPoly, unPoly)
import Crypto.Zp (class FiniteField, Zp, fieldPrime)
import Data.Group (class Group, ginverse)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)

import Data.BigInt.Argonaut (BigInt)
import Utility (error, quotRem, three, two)

class (Ring t, Group t, Eq t) <= IsExtension t where
    deg  :: t -> BigInt
    char :: t -> BigInt
    frob :: t -> t

instance FiniteField p => IsExtension (Zp p) where
    deg  = const one
    char = const (fieldPrime (mempty :: p))
    frob = identity

class Monoid e <= IrreducibleMonic t e where
    poly :: e -> Polynomial t

newtype Extension :: forall k. Type -> k -> Type
newtype Extension t e = E (Polynomial t)

instance (IsExtension t, IrreducibleMonic t e) => IsExtension (Extension t e) where
    deg _ = degPoly (poly (mempty :: e) :: Polynomial t) * deg (zero :: t)
    char = const $ char (zero :: t)
    frob x@(E p) = case frobenius (unPoly p) (unPoly $ poly (mempty :: e)) of
        Just q  -> E $ toPoly q
        Nothing -> pow x $ char x

frobenius :: forall t . (IsExtension t) => List t -> List t -> Maybe (List t)
frobenius Nil _     = Just Nil
frobenius (a:Nil) _ = Just (frob a : Nil)
frobenius (a:b:Nil) (x:_:_:Nil)
  | deg x == two  = Just (a : negate b : Nil)
  | char x == two = Just ((frob a - frob b * x) : Nil)
  | otherwise   = Just (frob a : frob b * nxq : Nil)
  where
    nxq = pow (negate x) (div (char x) two)
frobenius (a:b:Nil) (x:_:_:_:Nil) = if char x == three
                                  then Just ((frob a - frob b * x) : Nil)
                                else if r == one
                                  then Just (frob a : frob b * nxq : Nil)
                                else Just (frob a : zero : frob b * nxq : Nil)
  where
    Tuple q r = quotRem (char x) three
    nxq       = pow (negate x) q
frobenius (a:b:c:Nil) (x:_:_:_:Nil) = if char x == three
                                    then Just ((frob a - (frob b - frob c * x) * x) : Nil)
                                  else if r == one
                                    then Just (frob a : frob b * nxq : frob c * nxq * nxq : Nil)
                                  else Just (frob a : frob c * nx * nxq * nxq : frob b * nxq : Nil)
  where
    Tuple q r = quotRem (char x) three
    nx        = negate x
    nxq       = pow nx q
frobenius _ _   = Nothing

instance (IsExtension t, IrreducibleMonic t e) => Semiring (Extension t e) where
  add (E p1) (E p2) = E $ p1 + p2
  zero = E zero
  mul (E p1) (E p2) = E $ snd $ qr (p1 * p2) (poly (mempty :: e))
  one  = E one

instance (IsExtension t, IrreducibleMonic t e) => Ring (Extension t e) where
  sub (E p1) (E p2) = E $ p1 - p2

instance (IsExtension t, IrreducibleMonic t e) => CommutativeRing (Extension t e)

instance (IsExtension t, IrreducibleMonic t e) => EuclideanRing (Extension t e) where
  degree = const one
  div e1 e2 = e1 * ginverse e2
  mod = const $ const zero

instance (IsExtension t, IrreducibleMonic t e) => Semigroup (Extension t e) where
   append = (*)

instance (IsExtension t, IrreducibleMonic t e) => Monoid (Extension t e) where
  mempty = one

instance (IsExtension t, IrreducibleMonic t e) => Group (Extension t e) where
    ginverse (E a) = E (c * s)
      where
        f (Tuple x y) (Tuple x' y')
            | x' == zero = (Tuple x y)
            | otherwise  = f (Tuple x' y') (Tuple (x - q * x') (y - q * y'))
          where Tuple q _ = qr x x'
        Tuple g s = f (Tuple a one) (Tuple (poly (mempty :: e)) zero)
        c = fromConst $ ginverse $ leading g

instance (IsExtension t, IrreducibleMonic t e) => Eq (Extension t e) where
  eq (E a) (E b) = r == zero
    where Tuple _ r = qr (a - b) (poly (mempty :: e))

embed :: forall t e . IsExtension t => t -> Extension t e
embed x = E $ fromConst x

embed2 :: forall t e1 e2 . IsExtension t => IrreducibleMonic t e1 => t -> Extension (Extension t e1) e2
embed2 = embed <<< embed

conj :: forall t e . IsExtension t => Extension t e -> Extension t e
conj (E p) = case unPoly p of
    Nil     -> E $ toPoly Nil
    a:Nil   -> E $ toPoly (a : Nil)
    a:b:Nil -> E $ toPoly (a : (zero-b) : Nil)
    _       -> error "undefined"

pow :: forall t . IsExtension t => t -> BigInt -> t
pow a n
        | a == zero = zero
        | n == zero = one
        | n <  zero = pow (ginverse a) (negate n)
        | otherwise = if r == one then a * pow aa q else pow aa q
    where
        Tuple q r = quotRem n two
        aa        = a * a

-- | Unitary exponentiation @^@.
--
-- Exponentiation of a unitary element @x@ to an arbitrary integer @n@
-- in a specified cyclotomic subgroup.
{-# INLINE powUnitary #-}
powUnitary :: forall t e . IsExtension t => IrreducibleMonic t e => Extension t e -> BigInt -> Extension t e
powUnitary x n
            | n < zero  = pow (conj x) (negate n)
            | otherwise = pow x n

