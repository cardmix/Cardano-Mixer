module Utility where

import Prelude

import Data.BigInt.Argonaut (BigInt, even, fromInt, odd, toBase)
import Data.List (List(..), foldl, fromFoldable, reverse, (..), (:), filter, zip, unzip)
import Data.List as List
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

head :: forall t . List t -> t
head (a:_) = a
head _     = error "undefined"

length :: forall t . List t -> BigInt
length (_:lst) = one + length lst
length Nil     = zero

randomBigInt :: BigInt -> Effect BigInt
randomBigInt n = do
    let l = List.length $ fromFoldable $ toCharArray $ toBase 2 n
    go l
  where 
        f :: Effect BigInt
        f = do
            i <- randomInt 0 1
            pure $ fromInt i
        go :: Int -> Effect BigInt
        go l = do
          lst <- traverse (\_ -> f) (one .. l)
          let m = foldl (\b a -> two * b + a) zero lst
          if m > n
            then go l else pure $ m

last :: forall t . List t -> t
last = head <<< reverse

init :: forall t . List t -> List t
init Nil     = Nil
init (_:Nil) = Nil
init (x:xs)  = x : init xs

zipWith0 :: forall a b c . Semiring a => Semiring b => (a -> b -> c) -> List a -> List b -> List c
zipWith0 _ Nil Nil       = Nil
zipWith0 f Nil (b:bs)    = f zero b : zipWith0 f Nil bs
zipWith0 f (a:as) Nil    = f a zero : zipWith0 f as Nil
zipWith0 f (a:as) (b:bs) = f a b    : zipWith0 f as bs

replicate :: forall t . BigInt -> t -> List t
replicate n x
            | n <= zero = Nil
            | otherwise = x : replicate (n-one) x

quotRem :: BigInt -> BigInt -> Tuple BigInt BigInt
quotRem x y = Tuple (div x y) (mod x y)

indexRange :: BigInt -> BigInt -> List BigInt
indexRange = indexRange' Nil 

indexRange' :: List BigInt -> BigInt -> BigInt -> List BigInt
indexRange' lst a b = if a <= b then indexRange' (lst <> (a:Nil)) (a + one) b else lst

getEvenOdd :: forall t . List t -> Tuple (List t) (List t)
getEvenOdd xs = Tuple es os
    where l         = length xs
          inds      = indexRange zero (l-one)
          xs_zipped = zip inds xs
          ys = filter (even <<< fst) xs_zipped
          zs = filter (odd  <<< fst) xs_zipped
          Tuple _  es = unzip ys
          Tuple _  os = unzip zs

two :: forall t . Ring t => t
two = one + one

three :: forall t . Ring t => t
three = two + one


