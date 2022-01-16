module Crypto.Zp where

import Prelude

import Data.BigInt.Argonaut (BigInt, fromInt)
import Data.Group (class Group, ginverse)
import Data.Tuple (Tuple(..), snd)

class Monoid p <= FiniteField p where
    fieldPrime :: p -> BigInt

newtype Zp :: forall k. k -> Type
newtype Zp p = Zp BigInt

toZp :: forall p. FiniteField p => BigInt -> Zp p
toZp a = Zp $ mod a (fieldPrime (mempty :: p))

fromZp :: forall p. Zp p -> BigInt
fromZp (Zp a) = a

instance FiniteField p => Semiring (Zp p) where
  add (Zp a) (Zp b) = Zp $ mod (a + b) (fieldPrime (mempty :: p))
  zero = Zp $ fromInt 0
  mul (Zp a) (Zp b) = Zp $ mod (a * b) (fieldPrime (mempty :: p))
  one  = Zp $ fromInt 1

instance FiniteField p => Ring (Zp p) where
  sub (Zp a) (Zp b) = Zp $ mod (a - b) (fieldPrime (mempty :: p))

instance FiniteField p => CommutativeRing (Zp p)

instance FiniteField p => EuclideanRing (Zp p) where
  degree = const one
  div z1 z2 = z1 * ginverse z2
  mod = const $ const zero

instance FiniteField p => Semigroup (Zp p) where
   append = (*)

instance FiniteField p => Monoid (Zp p) where
  mempty = one

instance FiniteField p => Group (Zp p) where
  ginverse (Zp a) = Zp (mod (snd $ f (Tuple a one) (Tuple (fieldPrime (mempty :: p)) zero)) (fieldPrime (mempty :: p)))
      where
        f (Tuple x y) (Tuple x' y')
                    | x' == zero = Tuple x y
                    | otherwise  = f (Tuple x' y') (Tuple (x - q * x') (y - q * y'))
          where q = div x x'

instance FiniteField p => Eq (Zp p) where
  eq (Zp a) (Zp b) = zero == mod (a - b) (fieldPrime (mempty :: p))

instance FiniteField p => Ord (Zp p) where
  compare (Zp a) (Zp b) = compare (mod a (fieldPrime (mempty :: p))) (mod b (fieldPrime (mempty :: p)))
