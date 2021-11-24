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
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE NumericUnderscores         #-}

module Crypto.R1CS (R1C(..), R1CS, Assignment, makeSub, solveR1CS, getR1CSPolynomials, loadR1CSFile, example) where

import           Data.Aeson                        (FromJSON, decode)
import           Data.ByteString.Lazy              (readFile)
import           Data.Map                          (Map, fromList, findWithDefault, toList, union, intersection, difference, singleton, empty)
import           GHC.Generics                      (Generic)
import           PlutusTx.Prelude                  hiding ((<$>), (<*>), toList, mconcat)
import           Prelude                           (Show (..), String, IO, read, unzip3, (<*>), (^), print, Num (negate))
import qualified Prelude                           (null)

import           Crypto.BLS12381                   (Fr)
import           Crypto.DFT                        (DFT(..), IDFT(..), toDFT, idft, nearestPowerOfTwo, unDFT, extendDFT, targetPolyDFT, unIDFT, extend, dft, toIDFT)
import           Crypto.Zp                         (toZp, Zp (Zp))
import           Utility                           (replicate, drop)

------------------------------ R1CS -----------------------------

data R1C = R1C
    {
        leftCoefs  :: Map Integer Fr,
        rightCoefs :: Map Integer Fr,
        outCoefs   :: Map Integer Fr
    } deriving Show

type R1CS = [R1C]

----------------------------- Assingments -----------------------

type Assignment = Map Integer Fr

{-# INLINABLE makeSub #-}
makeSub :: Assignment -> Assignment -> Fr
makeSub subs expr = sum [ v * findWithDefault zero k subs | (k, v) <- toList expr ]

{-# INLINABLE solveR1CS #-}
solveR1CS :: R1CS -> Assignment -> Assignment
solveR1CS r1cs subs = if Prelude.null (difference newSubs subs) then subs else solveR1CS r1cs newSubs
  where newSubs = foldl solveR1C subs r1cs

-- TODO: optimize the function, throw error when solving fails
{-# INLINABLE solveR1C #-}
solveR1C :: Assignment -> R1C -> Assignment
solveR1C subs r1c = subs `union` subNew
  where
    x   = [leftCoefs, rightCoefs, outCoefs] <*> [r1c] :: [Assignment]
    [a, b, c] = map (makeSub subs . flip intersection subs) x
    diff = map (toList . flip difference subs) x
    subNew = case diff of
      [[], [], []]       -> singleton 0 one
      [[(k, v)], [], []] -> singleton k ((c * inv b - a) * inv v)
      [[], [(k, v)], []] -> singleton k ((c * inv a - b) * inv v)
      [[], [], [(k, v)]] -> singleton k ((a * b - c) * inv v)
      _                  -> singleton 0 one

-- Get polynomials (u, v, w, h)
{-# INLINABLE getR1CSPolynomials #-}
getR1CSPolynomials :: R1CS -> Assignment -> DFT -> (IDFT, IDFT, IDFT, IDFT)
getR1CSPolynomials r1cs subs (DFT p) = (u, v, w, h)
  where
    f r1c = (makeSub subs (leftCoefs r1c), makeSub subs (rightCoefs r1c), makeSub subs (outCoefs r1c))
    (a, b, c) = unzip3 $ map f r1cs
    uDFT = toDFT a
    vDFT = toDFT b
    wDFT = toDFT c
    lDFT = uDFT * vDFT - extendDFT wDFT
    u = idft uDFT
    v = idft vDFT
    w = idft wDFT
    m = divide (length $ unDFT lDFT) 2
    h = toIDFT $ drop m (unIDFT $ idft lDFT)

example :: IO ()
example = do
    print lDFT
    print $ idft lDFT
    print h
    print $ zipWith (*) (unDFT $ dft $ extend $ toIDFT h) (unDFT p)
    -- print p
    -- print $ idft p
    -- print $ hDFT
    -- print $ idft hDFT
  where 
    uDFT = toDFT [Zp 1, Zp 6, Zp 7, Zp 3]
    vDFT = toDFT [Zp 5, Zp 6, Zp 8, Zp 9]
    wDFT = toDFT [Zp 5, Zp 36, Zp 56, Zp 27]
    lDFT = uDFT * vDFT - extendDFT wDFT
    p = targetPolyDFT 4
    m = divide (length $ unDFT lDFT) 2
    h = drop m (unIDFT $ idft lDFT)
    -- f v a = 

----------------------- File operations ----------------------

data R1CSFile = R1CSFile
            {
                n8           :: Integer,
                prime        :: String,
                nVars        :: Integer,
                nOutputs     :: Integer,
                nPubInputs   :: Integer,
                nPrvInputs   :: Integer,
                nLabels      :: Integer,
                nConstraints :: Integer,
                constraints  :: [[Map String String]],
                map_         :: [Integer]
            }
  deriving (Show, Generic, FromJSON)

{-# INLINABLE loadR1CSFile #-}
loadR1CSFile :: String -> IO R1CS
loadR1CSFile str = do
    input <- readFile str
    let maybeR1CSFile = decode input :: Maybe R1CSFile
        r1cs          = maybe [] decodeR1CS maybeR1CSFile
        n             = nearestPowerOfTwo $ length r1cs
    return $ addEmptyR1CS n r1cs

{-# INLINABLE addEmptyR1CS #-}
addEmptyR1CS :: Integer -> R1CS -> R1CS
addEmptyR1CS _ [] = []
addEmptyR1CS n r1cs
              | l > 2^n     = r1cs
              | otherwise = r1cs ++ replicate (2^n-l) (R1C empty empty empty)
  where l = length r1cs

{-# INLINABLE decodeR1CS #-}
decodeR1CS :: R1CSFile -> R1CS
decodeR1CS rf = if null r then [] else map f r
  where
      r = constraints rf
      f x = R1C {leftCoefs = g 0 x, rightCoefs = g 1 x, outCoefs = g 2 x}
      g ind x = fromList $ map h $ toList (x !! ind)
      h (key, val) = (read key :: Integer, toZp $ read val)
