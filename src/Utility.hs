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

module Utility where

import           Data.List                         (partition, unzip)
import           Data.Text                         (Text, pack)
import           Ledger                            (PaymentPubKeyHash, Value)
import           Plutus.Contract                   (Contract, mapError)
import           Plutus.Contract.StateMachine      (SMContractError(..))
import           Ledger.Constraints                (mustPayToPubKey)
import           Ledger.Constraints.TxConstraints  (TxConstraints)
import           PlutusTx.Builtins                 (subtractInteger)
import           PlutusTx.Prelude
import           Prelude                           (Show(..), Char, String)

--------------------------------- Lists -------------------------------------

{-# INLINABLE init #-}
init :: [t] -> [t]
init []     = []
init [_]    = []
init (x:xs) = x : init xs

{-# INLINABLE last #-}
last :: [t] -> t
last = head . reverse

{-# INLINABLE drop #-}
drop :: Integer -> [a] -> [a]
drop n xs     | n <= 0 =  xs
drop _ []              =  []
drop n (_:xs)          =  drop (subtractInteger n 1) xs

{-# INLINABLE getEvenOdd #-}
getEvenOdd :: [t] -> ([t], [t])
getEvenOdd xs = (es, os)
    where (ys, zs) = partition (even . fst) (zip [0 :: Integer .. ] xs)
          (_,  es) = unzip ys
          (_,  os) = unzip zs

{-# INLINABLE replicate #-}
replicate :: Integer -> t -> [t]
replicate n x
            | n <= 0    = []
            | otherwise = x : replicate (n-1) x

{-# INLINABLE zipWith0 #-}
zipWith0 :: (AdditiveMonoid a, AdditiveMonoid b) => (a -> b -> c) -> [a] -> [b] -> [c]
zipWith0 _ [] []         = []
zipWith0 f [] (b:bs)     = f zero b : zipWith0 f [] bs
zipWith0 f (a:as) []     = f a zero : zipWith0 f as []
zipWith0 f (a:as) (b:bs) = f a b    : zipWith0 f as bs

{-# INLINABLE selectBatch #-}
selectBatch :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
selectBatch sz n1 n2 i = (n1 + sz*i, min (n1 + sz*(i+1) - 1) n2)

{-# INLINABLE numBatches #-}
numBatches :: Integer -> Integer -> Integer -> Integer
numBatches sz n1 n2
                | n1 > n2   = 0
                | otherwise = 1 + divide (n2-n1) sz

-------------------------------- ByteStrings --------------------------------

{-# INLINABLE buildByteString #-}
buildByteString :: String -> BuiltinByteString
buildByteString str = foldr (consByteString . g) emptyByteString (f str)
    where f s = if length s > 1 then take 2 s : f (drop 2 s) else []
          g s = charToHex (head s) * 16 + charToHex (s !! 1)

charToHex :: Char -> Integer
charToHex '0' = 0
charToHex '1' = 1
charToHex '2' = 2
charToHex '3' = 3
charToHex '4' = 4
charToHex '5' = 5
charToHex '6' = 6
charToHex '7' = 7
charToHex '8' = 8
charToHex '9' = 9
charToHex 'a' = 10
charToHex 'b' = 11
charToHex 'c' = 12
charToHex 'd' = 13
charToHex 'e' = 14
charToHex 'f' = 15
charToHex _   = error ()

--------------------------- Smart Contracts ---------------------------------

collateralConstraints :: PaymentPubKeyHash -> [Value] -> TxConstraints i o
collateralConstraints pkh vals = mconcat $ map (mustPayToPubKey pkh) vals

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show


