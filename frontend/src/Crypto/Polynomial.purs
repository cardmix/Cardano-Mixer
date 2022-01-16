module Crypto.Polynomial where

import Prelude

import Control.Alternative (guard)
import Data.BigInt.Argonaut (BigInt)
import Data.Group (class Group, ginverse)
import Data.List (List(..), foldl, null, zipWith, (:))
import Data.Tuple (Tuple(..))

import Utility (init, last, length, replicate, zipWith0)
  
newtype Polynomial t = P (List t)

instance (Eq t, Ring t) => Semiring (Polynomial t) where
  add (P x) (P y) = removeZeroTerms $ P $ (zipWith0) (+) x y
  zero            = P Nil
  mul (P x) (P y) = P $ mulLst x' y' zero
        where       
            x' = addNumber x zero
            y' = addNumber y zero
            addNumber :: List t -> BigInt -> List (Tuple t BigInt)
            addNumber Nil    _ = Nil
            addNumber (a:as) n = Tuple a n : addNumber as (n+one)
  one             = P (one : Nil)

mulLst :: forall t . Ring t => List (Tuple t BigInt) -> List (Tuple t BigInt) -> BigInt -> List t
mulLst x y k = do
                Tuple xi ni <- x
                Tuple yi mi <- y
                guard $ ni + mi == k
                pure $ xi * yi

mulCoefs :: forall t . Ring t => List (Tuple t BigInt) -> List (Tuple t BigInt) -> BigInt -> List t
mulCoefs x y k = case mulLst x y k of
                    Nil -> Nil
                    _   -> (foldl (+) zero (mulLst x y k)) : mulCoefs x y (k + one)

instance (Eq t, Ring t) => Ring (Polynomial t) where
  sub (P x) (P y) = removeZeroTerms $ P $ zipWith0 (-) x y

instance (Eq t, Ring t) => CommutativeRing (Polynomial t)

instance (Eq t, Ring t) => Semigroup (Polynomial t) where
   append = (*)

instance (Eq t, Ring t) => Monoid (Polynomial t) where
  mempty = one

instance Eq t => Eq (Polynomial t) where
  eq (P x) (P y) = if length x == length y
                    then foldl (&&) true $ zipWith (==) x y
                    else false

degPoly :: forall t . Polynomial t -> BigInt
degPoly (P p)  = length p - one

leading :: forall t . Ring t => Polynomial t -> t
leading (P p)
    | null p    = zero
    | otherwise = last p

unPoly :: forall t . Polynomial t -> List t
unPoly (P p) = p

toPoly :: forall t . Eq t => Ring t => List t -> Polynomial t
toPoly = removeZeroTerms <<< P

fromConst :: forall t . Eq t => Ring t => t -> Polynomial t
fromConst a
        | a == zero = P Nil
        | otherwise = P (a:Nil)

qr :: forall t . Eq t => Group t => Ring t => Polynomial t -> Polynomial t -> Tuple (Polynomial t) (Polynomial t)
qr x y = quotientRemainder x y zero

quotientRemainder :: forall t . Eq t => Group t => Ring t => Polynomial t -> Polynomial t -> Polynomial t -> Tuple (Polynomial t) (Polynomial t)
quotientRemainder x y q = case degX < degY of
        true  -> Tuple q x
        false -> quotientRemainder x' y q'
    where
        a    = leading x * ginverse (leading y)
        n    = degX - degY
        y'   = monomialMultiply y (Tuple a n)
        x'   = x - y'
        q'   = q + monomialMultiply one (Tuple a n)
        degX = degPoly x
        degY = degPoly y

monomialMultiply :: forall t . Ring t => Eq t => Polynomial t -> Tuple t BigInt -> Polynomial t
monomialMultiply (P p) (Tuple a n)
                    | a == zero = zero
                    | otherwise = P $ replicate n zero <> map ((*) a) p

removeZeroTerms :: forall t . Eq t => Ring t => Polynomial t -> Polynomial t
removeZeroTerms q@(P p)
                    | q == zero      = P Nil
                    | last p == zero = removeZeroTerms (P $ init p)
                    | otherwise      = P p

