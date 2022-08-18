{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Crypto.Conversions  where

import           Ledger.Address              (PaymentPubKeyHash (..))
import           Plutus.V2.Ledger.Api        (Address (..), Credential(..), PubKeyHash (..), StakingCredential (..), ValidatorHash (..),
                                                CurrencySymbol (..), TokenName (..))
import           PlutusTx.Prelude            hiding (toList)

import           Crypto.Zp                   (Zp(..), FiniteField(..), toZp)
import           Utils.ByteString            (byteStringToInteger)

----------------------------- DataToZp ------------------------------------------

class FiniteField p => DataToZp a p where
    dataToZp :: a -> Zp p

instance FiniteField p => DataToZp (Zp p) p where
    {-# INLINABLE dataToZp #-}
    dataToZp = id

instance FiniteField p => DataToZp Integer p where
    {-# INLINABLE dataToZp #-}
    dataToZp = toZp

instance FiniteField p => DataToZp BuiltinByteString p where
    {-# INLINABLE dataToZp #-}
    dataToZp bs = toZp $ byteStringToInteger bs

instance FiniteField p => DataToZp PaymentPubKeyHash p where
    {-# INLINABLE dataToZp #-}
    dataToZp = dataToZp . getPubKeyHash . unPaymentPubKeyHash

instance FiniteField p => DataToZp Address p where
    {-# INLINABLE dataToZp #-}
    dataToZp (Address (PubKeyCredential (PubKeyHash pkh)) (Just (StakingHash (PubKeyCredential (PubKeyHash skh))))) =
        dataToZp $ pkh `appendByteString` skh
    dataToZp (Address (PubKeyCredential (PubKeyHash pkh)) _) = dataToZp pkh
    dataToZp (Address (ScriptCredential (ValidatorHash vh)) (Just (StakingHash (PubKeyCredential (PubKeyHash skh))))) =
        dataToZp $ vh `appendByteString` skh
    dataToZp (Address (ScriptCredential (ValidatorHash vh)) _) = dataToZp vh

instance FiniteField p => DataToZp CurrencySymbol p where
    {-# INLINABLE dataToZp #-}
    dataToZp = dataToZp . unCurrencySymbol

instance FiniteField p => DataToZp TokenName p where
    {-# INLINABLE dataToZp #-}
    dataToZp = dataToZp . unTokenName

-- instance (FiniteField p, DataToZp a p, DataToZp b p) => DataToZp (a, b) p where
--     {-# INLINABLE dataToZp #-}
--     dataToZp (x, y) = mimcHash (dataToZp x) (dataToZp y)

-- instance (FiniteField p, DataToZp a p, DataToZp b p) => DataToZp (Map a b) p where
--     {-# INLINABLE dataToZp #-}
--     dataToZp m = foldl (curry dataToZp) zero (toList m)

-- instance FiniteField p => DataToZp Value p where
--     {-# INLINABLE dataToZp #-}
--     dataToZp = dataToZp . getValue
