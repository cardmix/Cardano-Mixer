{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Configuration.PABConfig where

import Data.Either                   (rights)
import Data.Text                     (pack)
import Ledger                        (PubKeyHash(..))
import Ledger.Address                (PaymentPubKeyHash(..))
import Plutus.V1.Ledger.Api          (ValidatorHash (..))
import PlutusTx.Prelude              hiding (elem)
import Prelude                       (String)
import Wallet.Emulator.Wallet        (Wallet(..), fromBase16)

data PABConfig = Mainnet | Testnet

pabConfig :: PABConfig
pabConfig = Testnet

--------------------------------- Mainnet --------------------------------------

--------------------------------- Testnet --------------------------------------

pabWalletIdStringTestnet :: String
pabWalletIdStringTestnet = "2c922c0b34abf5ab0d7f3f290b9f6c8874a4d300"

pabWalletPKHBytesTestnet :: [Integer]
pabWalletPKHBytesTestnet = [0x46, 0x62, 0x9e, 0xfc, 0x9c, 0x5d, 0xdf, 0x1a, 0x78, 0x36, 0x0f, 0x4d, 0xa4, 0xdc, 0x38,
     0x72, 0x87, 0xa8, 0x23, 0x8a, 0x57, 0x27, 0xce, 0x7d, 0x02, 0xf6, 0xb6, 0x58]

dispenserWalletIdStringTestnet :: String
dispenserWalletIdStringTestnet = "5cd8d83d3de9770ac2970f6238386e183e216854"

dispenserWalletPKHBytesTestnet :: [Integer]
dispenserWalletPKHBytesTestnet = [238,124,117,213,182,255,103,17,174,19,115,112,145,197,125,26,131,244,146,99,216,28,233,128,221,173,106,2]

-- TODO: add to config
adminTokenPolicyId :: [Integer]
adminTokenPolicyId = [0x14, 0xca, 0x69, 0xe5, 0x9f, 0x3e, 0xa7, 0xcb, 0x8d, 0x74,
     0x83, 0x0c, 0xc1, 0x8d, 0x52, 0x86, 0x60, 0xcd, 0xa0, 0xc4, 0x5e, 0xe7, 0x84, 0xdf, 0xb3, 0x2c, 0x95, 0xbf]

-- TODO: add to config
adminDecisionTokenPolicyId :: [Integer]
adminDecisionTokenPolicyId = [0x18, 0x03, 0x07, 0xc3, 0x48, 0xf6, 0x48, 0x28, 0xb5, 0x8b, 0xa1, 0x19, 0x45, 0x8f, 0x41, 0xd9,
     0x9b, 0xfe, 0xd8, 0x23, 0x72, 0xba, 0xfb, 0xc0, 0x82, 0x9e, 0x05, 0xc6]

-- TODO: add to config
mixTokenPolicyId :: [Integer]
mixTokenPolicyId = [242,31,23,221,138,119,42,218,30,173,38,40,35,162,36,241,236,157,175,173,101,220,105,57,207,60,72,72]

-- TODO: add to config
mixStakingTokenPolicyId :: [Integer]
mixStakingTokenPolicyId = [0x18, 0x03, 0x07, 0xc3, 0x48, 0xf6, 0x48, 0x28, 0xb5, 0x8b, 0xa1, 0x19, 0x45, 0x8f, 0x41, 0xd9,
     0x9b, 0xfe, 0xd8, 0x23, 0x72, 0xba, 0xfb, 0xc0, 0x82, 0x9e, 0x05, 0xc6]

----------------------------- Common -------------------------------

pabWalletIdString :: String
pabWalletIdString = case pabConfig of
     Mainnet -> pabWalletIdStringTestnet
     Testnet -> pabWalletIdStringTestnet

pabWallet :: Wallet
pabWallet = Wallet (Just "PAB Wallet") $ head $ rights [fromBase16 $ pack pabWalletIdString]

pabWalletPKHBytes :: [Integer]
pabWalletPKHBytes = case pabConfig of
     Mainnet -> pabWalletPKHBytesTestnet
     Testnet -> pabWalletPKHBytesTestnet

pabWalletPKH :: PaymentPubKeyHash
pabWalletPKH = PaymentPubKeyHash $ PubKeyHash $ foldr consByteString emptyByteString pabWalletPKHBytes

pabWalletSKHBytes :: [Integer]
pabWalletSKHBytes = [243,164,20,223,212,64,134,145,48,199,95,222,0,209,106,90,163,129,242,2,159,196,118,76,249,7,183,33]

pabWalletSKH :: PubKeyHash
pabWalletSKH = PubKeyHash $ foldr consByteString emptyByteString pabWalletSKHBytes

dispenserWalletIdString :: String
dispenserWalletIdString = case pabConfig of
     Mainnet -> dispenserWalletIdStringTestnet
     Testnet -> dispenserWalletIdStringTestnet

dispenserWallet :: Wallet
dispenserWallet = Wallet (Just "Dispenser Wallet") $ head $ rights [fromBase16 $ pack dispenserWalletIdString]

dispenserWalletPKHBytes :: [Integer]
dispenserWalletPKHBytes = case pabConfig of
     Mainnet -> dispenserWalletPKHBytesTestnet
     Testnet -> dispenserWalletPKHBytesTestnet

dispenserWalletPKH :: PaymentPubKeyHash
dispenserWalletPKH = PaymentPubKeyHash $ PubKeyHash $ foldr consByteString emptyByteString dispenserWalletPKHBytes

vestingHashBytes :: [Integer]
vestingHashBytes = [189,179,221,146,199,167,210,211,153,234,155,218,69,28,238,129,6,21,97,16,41,71,20,87,235,184,0,141]

vestingScriptPermanentHash :: ValidatorHash
vestingScriptPermanentHash = ValidatorHash $ foldr consByteString emptyByteString vestingHashBytes
