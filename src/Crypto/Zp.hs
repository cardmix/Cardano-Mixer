{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE NumericUnderscores         #-}

module Crypto.Zp (Zp(..), FiniteField(..), toZp, fromZp) where

import           Data.Aeson                        (FromJSON, ToJSON)
import           GHC.Generics                      (Generic)
import           PlutusTx                          (FromData(..), ToData(..), UnsafeFromData(..))
import           PlutusTx.Prelude
import           Prelude                           (Show)
import           Schema                            (ToSchema)
import           Test.QuickCheck.Arbitrary.Generic (Arbitrary(..), genericArbitrary)

------------------------- Finite Field -----------------------------

class (Monoid p) => FiniteField p where
    fieldPrime :: p -> Integer

newtype Zp p = Zp Integer
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

{-# INLINABLE toZp #-}
toZp :: forall p. FiniteField p => Integer -> Zp p
toZp a = Zp $ modulo a (fieldPrime (mempty :: p))

{-# INLINABLE fromZp #-}
fromZp ::Zp p -> Integer
fromZp (Zp a) = a

instance forall p. FiniteField p => Ord (Zp p) where
    {-# INLINABLE (<=) #-}
    (<=) (Zp a) (Zp b) = modulo a (fieldPrime (mempty :: p)) <= modulo b (fieldPrime (mempty :: p))

instance Arbitrary (Zp p) where
  {-# INLINABLE arbitrary #-}
  arbitrary = genericArbitrary

instance forall p. FiniteField p => AdditiveSemigroup (Zp p) where
    {-# INLINABLE (+) #-}
    (+) (Zp a) (Zp b) = Zp $ modulo (a + b) (fieldPrime (mempty :: p))

instance forall p. FiniteField p => AdditiveGroup (Zp p) where
    {-# INLINABLE (-) #-}
    (-) (Zp a) (Zp b) = Zp $ modulo (a-b) (fieldPrime (mempty :: p))

instance forall p. FiniteField p => AdditiveMonoid (Zp p) where
    {-# INLINABLE zero #-}
    zero = Zp 0

instance forall p. FiniteField p => MultiplicativeSemigroup (Zp p) where
    {-# INLINABLE (*) #-}
    (*) (Zp a) (Zp b) = Zp $ modulo (a * b) (fieldPrime (mempty :: p))

instance forall p. FiniteField p => Semigroup (Zp p) where
    {-# INLINABLE (<>) #-}
    (<>) = (*)

instance forall p. FiniteField p => MultiplicativeMonoid (Zp p) where
    {-# INLINABLE one #-}
    one = Zp 1

instance forall p. FiniteField p => Monoid (Zp p) where
    {-# INLINABLE mempty #-}
    mempty = one

instance forall p. FiniteField p => Group (Zp p) where
    {-# INLINABLE inv #-}
    inv (Zp a) = Zp (modulo (snd $ f (a, 1) (fieldPrime (mempty :: p), 0)) (fieldPrime (mempty :: p)))
      where
        f (x, y) (x', y')
                    | x' == zero = (x, y)
                    | otherwise  = f (x', y') (x - q * x', y - q * y')
          where q = divide x x'

instance forall p. FiniteField p => Eq (Zp p) where
    {-# INLINABLE (==) #-}
    (==) (Zp a) (Zp b) = 0 == modulo (a - b) (fieldPrime (mempty :: p))

instance ToData (Zp p) where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData (Zp a) = toBuiltinData a

instance FromData (Zp p) where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData i = Zp <$> fromBuiltinData i

instance UnsafeFromData (Zp p) where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData i = Zp $ unsafeFromBuiltinData i
