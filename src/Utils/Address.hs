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

import           Configuration.PABConfig         (pabWalletPKH, pabWalletSKH)


getPubKeyHashFromStakeAddress :: Text -> PaymentPubKeyHash
getPubKeyHashFromStakeAddress txt = maybe (error ()) PaymentPubKeyHash $ stakeAddrToPlutusPubKHash $
                        fromRight (error ()) $ deserialiseFromBech32 AsStakeAddress txt

stakeAddrToPlutusPubKHash :: StakeAddress -> Maybe PubKeyHash
stakeAddrToPlutusPubKHash (StakeAddress _ payCred) =
  case payCred of
    Shelley.ScriptHashObj _ -> Nothing
    Shelley.KeyHashObj kHash -> Just $ Alonzo.transKeyHash kHash

textToPKH :: Text -> Maybe PaymentPubKeyHash
textToPKH txt = case txt of
                "" -> Just pabWalletPKH
                _  -> do
                        addr <- either (const Nothing) Just $ deserialiseFromBech32 AsShelleyAddress txt
                        pkh  <- shelleyPayAddrToPlutusPubKHash addr
                        return $ PaymentPubKeyHash pkh
                        

textToAddress :: Text -> Maybe Address
textToAddress txt = case txt of
                "" -> Just $ pubKeyHashAddress pabWalletPKH (Just $ StakePubKeyHash pabWalletSKH)
                _  -> do
                        Shelley.ShelleyAddress _ payCred stakeRef <- either (const Nothing) Just $ deserialiseFromBech32 AsShelleyAddress txt
                        pkh <- case payCred of
                                KeyHashObj    h1 -> Just $ PaymentPubKeyHash $ transKeyHash h1
                                ScriptHashObj _  -> Nothing
                        skh <- case fromShelleyStakeReference stakeRef of
                                StakeAddressByValue (StakeCredentialByKey (StakeKeyHash h2)) -> Just $ StakePubKeyHash $ transKeyHash h2
                                _  -> Nothing
                        return $ pubKeyHashAddress pkh (Just skh)

textToKeys :: Text -> Maybe (PaymentPubKeyHash, StakePubKeyHash)
textToKeys txt = case txt of
                "" -> Just (pabWalletPKH, StakePubKeyHash pabWalletSKH)
                _  -> do
                        Shelley.ShelleyAddress _ payCred stakeRef  <- either (const Nothing) Just $ deserialiseFromBech32 AsShelleyAddress txt
                        pkh <- case payCred of
                                KeyHashObj    h1 -> Just $ PaymentPubKeyHash $ transKeyHash h1
                                ScriptHashObj _  -> Nothing
                        skh <- case fromShelleyStakeReference stakeRef of
                                StakeAddressByValue (StakeCredentialByKey (StakeKeyHash h2)) -> Just $ StakePubKeyHash $ transKeyHash h2
                                _  -> Nothing
                        return (pkh, skh)