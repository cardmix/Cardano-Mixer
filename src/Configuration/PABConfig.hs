{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Configuration.PABConfig where

import Cardano.Api.Shelley           (ProtocolParameters, NetworkId(..), NetworkMagic (..))
import Data.Aeson                    (decode)
import Data.ByteString.Lazy          (fromStrict)
import Data.Either                   (rights)
import Data.FileEmbed                (embedFile)
import Data.Maybe                    (fromJust)
import Data.Text                     (Text, pack)
import Ledger                        (PubKeyHash(..), TxOutRef (TxOutRef), TxId (..))
import Ledger.Address                (PaymentPubKeyHash(..), StakePubKeyHash (..))
import PlutusTx.Prelude              hiding (elem)
import Prelude                       (String, undefined)
import Wallet.Emulator.Wallet        (Wallet(..), fromBase16)

import Utils.Address                 (bech32ToKeyHashes)
import Utils.Network                 (NetworkConfig (..))

-- TODO: Replace PolicyId with TxOutRef

--------------------------------- Network-dependent -----------------------------------

pabWalletAddressText :: Text
pabWalletAddressText = "addr_test1qprx98hun3wa7xncxc85mfxu8peg02pr3ftj0nnaqtmtvk8n5s2dl4zqs6gnp36lmcqdz6j65wqlyq5lc3mye7g8kussq5skqf"

dispenserWalletAddressText :: Text
dispenserWalletAddressText = "addr_test1qrh8caw4kmlkwydwzdehpyw905dg8ayjv0vpe6vqmkkk5q3psddwydp9ea0gj3jawxyak3d238jpj9fxx3gnfhk7paxqnw2xmw"

-- TODO: define this
governanceBeaconTokenPolicyId :: BuiltinByteString
governanceBeaconTokenPolicyId = undefined

mixTokenTxOutRef :: TxOutRef
mixTokenTxOutRef = TxOutRef (TxId emptyByteString) 0 

mixerBeaconTxOutRef :: TxOutRef
mixerBeaconTxOutRef = TxOutRef (TxId emptyByteString) 0

mixProfitsBeaconTxOutRef :: TxOutRef
mixProfitsBeaconTxOutRef = TxOutRef (TxId emptyByteString) 0

networkId :: NetworkId
networkId = Testnet $ NetworkMagic 1097911063

protocolParams :: ProtocolParameters
protocolParams = fromJust $ decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")

------------------------------------------ Common ---------------------------------------------

networkConfig :: NetworkConfig
networkConfig = NetworkConfig networkId protocolParams

pabWalletIdString :: String
pabWalletIdString = "2c922c0b34abf5ab0d7f3f290b9f6c8874a4d300"

pabWallet :: Wallet
pabWallet = Wallet (Just "PAB Wallet") $ head $ rights [fromBase16 $ pack pabWalletIdString]

pabWalletPKH :: PaymentPubKeyHash
pabWalletPKH = fst $ fromJust $ bech32ToKeyHashes pabWalletAddressText

pabWalletSKH :: PubKeyHash
pabWalletSKH = let StakePubKeyHash kh = fromJust $ snd $ fromJust $ bech32ToKeyHashes pabWalletAddressText in kh

dispenserWalletIdString :: String
dispenserWalletIdString = "5cd8d83d3de9770ac2970f6238386e183e216854"

dispenserWallet :: Wallet
dispenserWallet = Wallet (Just "Dispenser Wallet") $ head $ rights [fromBase16 $ pack dispenserWalletIdString]

dispenserWalletPKH :: PaymentPubKeyHash
dispenserWalletPKH = fst $ fromJust $ bech32ToKeyHashes dispenserWalletAddressText
