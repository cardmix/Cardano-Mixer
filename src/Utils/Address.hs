{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}


module Utils.Address where

import           Data.Either                     (fromRight)
import           Data.Text                       (pack)
import           Cardano.Api.Shelley             (AsType (..), StakeAddress(..), shelleyPayAddrToPlutusPubKHash, deserialiseFromBech32)
import qualified Cardano.Ledger.Alonzo.TxInfo    as Alonzo
import qualified Cardano.Ledger.Credential       as Shelley
import           Ledger.Address                  (PaymentPubKeyHash(..))
import           Plutus.V1.Ledger.Api            (PubKeyHash)
import           PlutusTx.Prelude
import           Prelude                         (String)

import           Configuration.PABConfig         (pabWalletPKH)


getPubKeyHashFromStakeAddress :: String -> PaymentPubKeyHash
getPubKeyHashFromStakeAddress str = maybe (error ()) PaymentPubKeyHash $ stakeAddrToPlutusPubKHash $
                        fromRight (error ()) $ deserialiseFromBech32 AsStakeAddress $ pack str

stakeAddrToPlutusPubKHash :: StakeAddress -> Maybe PubKeyHash
stakeAddrToPlutusPubKHash (StakeAddress _ payCred) =
  case payCred of
    Shelley.ScriptHashObj _ -> Nothing
    Shelley.KeyHashObj kHash -> Just $ Alonzo.transKeyHash kHash

strToPKH :: String -> PaymentPubKeyHash
strToPKH str = case str of
                "" -> pabWalletPKH
                _  -> maybe (error ()) PaymentPubKeyHash $ shelleyPayAddrToPlutusPubKHash $
                        fromRight (error ()) $ deserialiseFromBech32 AsShelleyAddress $ pack str