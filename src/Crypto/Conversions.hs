{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Crypto.Conversions  where

import           Ledger.Address              (PaymentPubKeyHash (..))
import           Plutus.V1.Ledger.Api
import           PlutusTx.AssocMap           (Map, toList)
import           PlutusTx.Prelude            hiding (toList)

import           Crypto.MiMC                 (mimcHash)
import           Crypto.Zp                   (Zp(..), FiniteField(..), toZp)

----------------------------- DataToZp ------------------------------------------

class FiniteField p => DataToZp a p where
    dataToZp :: a -> Zp p

instance FiniteField p => DataToZp (Zp p) p where
    {-# INLINABLE dataToZp #-}
    dataToZp = id

instance FiniteField p => DataToZp Integer p where
    {-# INLINABLE dataToZp #-}
    dataToZp = integerToZp

instance FiniteField p => DataToZp BuiltinByteString p where
    {-# INLINABLE dataToZp #-}
    dataToZp = integerToZp . byteStringToInteger 0

instance FiniteField p => DataToZp PaymentPubKeyHash p where
    {-# INLINABLE dataToZp #-}
    dataToZp = dataToZp . getPubKeyHash . unPaymentPubKeyHash

instance FiniteField p => DataToZp CurrencySymbol p where
    {-# INLINABLE dataToZp #-}
    dataToZp = dataToZp . unCurrencySymbol

instance FiniteField p => DataToZp TokenName p where
    {-# INLINABLE dataToZp #-}
    dataToZp = dataToZp . unTokenName

instance (FiniteField p, DataToZp a p, DataToZp b p) => DataToZp (a, b) p where
    {-# INLINABLE dataToZp #-}
    dataToZp (x, y) = mimcHash (dataToZp x) (dataToZp y)

instance (FiniteField p, DataToZp a p, DataToZp b p) => DataToZp (Map a b) p where
    {-# INLINABLE dataToZp #-}
    dataToZp m = foldl (curry dataToZp) zero (toList m)

instance FiniteField p => DataToZp Value p where
    {-# INLINABLE dataToZp #-}
    dataToZp = dataToZp . getValue

-------------------------------- Auxiliary ----------------------------------------

{-# INLINABLE integerToZp #-}
integerToZp :: forall p. FiniteField p => Integer -> Zp p
integerToZp m
            | m < fieldPrime (mempty :: p) = toZp m
            | otherwise                    = mimcHash (toZp r) (integerToZp q)
                where (q, r) = quotRem m (fieldPrime (mempty :: p))

{-# INLINABLE byteStringToInteger #-}
byteStringToInteger :: Integer -> BuiltinByteString -> Integer
byteStringToInteger z bs
                    | bs == emptyByteString = z
                    | otherwise             = byteStringToInteger (256*z + a) bs'
                        where a   = indexByteString (takeByteString 1 bs) 0
                              bs' = dropByteString 1 bs