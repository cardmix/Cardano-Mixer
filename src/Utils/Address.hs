{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}


module Utils.Address where

import           Data.Text                       (Text)
import           Cardano.Api.Shelley             (AsType (..), StakeAddress(..), StakeAddressReference (..), StakeCredential (..), Hash (..),
        deserialiseFromBech32, fromShelleyStakeReference)
import qualified Cardano.Api.Shelley             as Shelley
import           Cardano.Ledger.Alonzo.TxInfo    (transKeyHash)
import           Cardano.Ledger.Credential       (Credential(..))
import qualified Cardano.Ledger.Credential       as Shelley
import           Ledger.Address                  (PaymentPubKeyHash(..), StakePubKeyHash(..), Address, pubKeyHashAddress)
import           PlutusTx.Prelude

------------------------ Export/Import of addresses -------------------------

-- Convert bech32 Shelley address to a pair of hashes. Must contain StakePubKeyHash.
bech32ToKeyHashes :: Text -> Maybe (PaymentPubKeyHash, StakePubKeyHash)
bech32ToKeyHashes txt = do
        Shelley.ShelleyAddress _ payCred stakeRef  <- either (const Nothing) Just $ deserialiseFromBech32 AsShelleyAddress txt
        pkh <- case payCred of
                KeyHashObj    h1 -> Just $ PaymentPubKeyHash $ transKeyHash h1
                ScriptHashObj _  -> Nothing
        skh <- case fromShelleyStakeReference stakeRef of
                StakeAddressByValue (StakeCredentialByKey (StakeKeyHash h2)) -> Just $ StakePubKeyHash $ transKeyHash h2
                _  -> Nothing
        return (pkh, skh)

-- Convert bech32 Shelley address to a Plutus address. Must contain StakePubKeyHash.
bech32ToAddress :: Text -> Maybe Address
bech32ToAddress txt = do
        (pkh, skh) <- bech32ToKeyHashes txt
        return $ pubKeyHashAddress pkh (Just skh)

-- Convert bech32 Stake address to a Plutus StakePubKeyHash.
bech32ToStakePubKeyHash :: Text -> Maybe StakePubKeyHash
bech32ToStakePubKeyHash txt = do
                        StakeAddress _ payCred <- either (const Nothing) Just $ deserialiseFromBech32 AsStakeAddress txt
                        case payCred of
                                Shelley.ScriptHashObj _ -> Nothing
                                Shelley.KeyHashObj kHash -> Just $ StakePubKeyHash $ transKeyHash kHash
