{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}


module Utils.Address where

import           Data.Either                     (fromRight)
import           Data.Text                       (Text)
import           Cardano.Api.Shelley             (AsType (..), StakeAddress(..), shelleyPayAddrToPlutusPubKHash, deserialiseFromBech32, fromShelleyStakeReference, StakeAddressReference (StakeAddressByValue), StakeCredential (StakeCredentialByKey), Hash (StakeKeyHash))
import qualified Cardano.Api.Shelley             as Shelley
import           Cardano.Ledger.Alonzo.TxInfo    (transKeyHash)
import qualified Cardano.Ledger.Alonzo.TxInfo    as Alonzo
import           Cardano.Ledger.Credential       (Credential(..))
import qualified Cardano.Ledger.Credential       as Shelley
import           Ledger.Address                  (PaymentPubKeyHash(..), StakePubKeyHash(..), Address, pubKeyHashAddress)
import           Plutus.V1.Ledger.Api            (PubKeyHash)
import           PlutusTx.Prelude

import           Configuration.PABConfig         (pabWalletPKH)


getPubKeyHashFromStakeAddress :: Text -> PaymentPubKeyHash
getPubKeyHashFromStakeAddress txt = maybe (error ()) PaymentPubKeyHash $ stakeAddrToPlutusPubKHash $
                        fromRight (error ()) $ deserialiseFromBech32 AsStakeAddress txt

stakeAddrToPlutusPubKHash :: StakeAddress -> Maybe PubKeyHash
stakeAddrToPlutusPubKHash (StakeAddress _ payCred) =
  case payCred of
    Shelley.ScriptHashObj _ -> Nothing
    Shelley.KeyHashObj kHash -> Just $ Alonzo.transKeyHash kHash

textToPKH :: Text -> PaymentPubKeyHash
textToPKH txt = case txt of
                "" -> pabWalletPKH
                _  -> maybe (error ()) PaymentPubKeyHash $ shelleyPayAddrToPlutusPubKHash $
                        fromRight (error ()) $ deserialiseFromBech32 AsShelleyAddress txt

textToAddress :: Text -> Address 
textToAddress txt = case txt of
                "" -> pubKeyHashAddress pabWalletPKH Nothing
                _  -> pubKeyHashAddress pkh (Just skh)
      where
        Shelley.ShelleyAddress _ payCred stakeRef  = fromRight (error ()) $ deserialiseFromBech32 AsShelleyAddress txt
        pkh = case payCred of
                KeyHashObj    h1 -> PaymentPubKeyHash $ transKeyHash h1
                ScriptHashObj _  -> error ()
        skh = case fromShelleyStakeReference stakeRef of
                StakeAddressByValue (StakeCredentialByKey (StakeKeyHash h2)) -> StakePubKeyHash $ transKeyHash h2
                _  -> error ()

textToKeys :: Text -> (PaymentPubKeyHash, StakePubKeyHash)
textToKeys txt = case txt of
                "" -> (pabWalletPKH, StakePubKeyHash $ unPaymentPubKeyHash pabWalletPKH)
                _  -> (pkh, skh)
      where
        Shelley.ShelleyAddress _ payCred stakeRef  = fromRight (error ()) $ deserialiseFromBech32 AsShelleyAddress txt
        pkh = case payCred of
                KeyHashObj    h1 -> PaymentPubKeyHash $ transKeyHash h1
                ScriptHashObj _  -> error ()
        skh = case fromShelleyStakeReference stakeRef of
                StakeAddressByValue (StakeCredentialByKey (StakeKeyHash h2)) -> StakePubKeyHash $ transKeyHash h2
                _  -> error ()