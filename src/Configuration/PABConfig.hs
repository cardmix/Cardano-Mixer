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

data PABConfig = Simulator | Testnet

pabConfig :: PABConfig
pabConfig = Testnet

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

---------------------------- Simulator / Emulator -----------------------------

pabWalletIdStringSimulator :: String
pabWalletIdStringSimulator = "1bc5f27d7b4e20083977418e839e429d00cc87f3"

pabWalletPKHBytesSimulator :: [Integer]
pabWalletPKHBytesSimulator = [0xc6, 0x05, 0x88, 0x8d, 0x3c, 0x40, 0x38, 0x6d, 0x7c, 0x32, 0x3a, 0x46, 0x79, 0xc7, 0x67, 0xe5,
     0xa0, 0xa7, 0xb6, 0x83, 0x60, 0x5c, 0x3e, 0x5d, 0xf9, 0xa7, 0x6a, 0xee]

-- TODO: correct bytes
dispenserWalletIdStringSimulator :: String
dispenserWalletIdStringSimulator = "5cd8d83d3de9770ac2970f6238386e183e216854"

-- TODO: correct bytes
dispenserWalletPKHBytesSimulator :: [Integer]
dispenserWalletPKHBytesSimulator = [238,124,117,213,182,255,103,17,174,19,115,112,145,197,125,26,131,244,146,99,216,28,233,128,221,173,106,2]

-- -- TODO: add to config
-- mixTokenPolicyId :: [Integer]
-- mixTokenPolicyId = [234,90,69,0,93,247,236,193,240,29,130,189,8,57,128,143,197,107,192,226,136,118,145,236,43,91,163,42]

----------------------------- Common -------------------------------

pabWalletIdString :: String
pabWalletIdString = case pabConfig of
     Simulator -> pabWalletIdStringSimulator
     Testnet   -> pabWalletIdStringTestnet

pabWallet :: Wallet
pabWallet = Wallet $ head $ rights [fromBase16 $ pack pabWalletIdString]

pabWalletPKHBytes :: [Integer]
pabWalletPKHBytes = case pabConfig of
     Simulator -> pabWalletPKHBytesSimulator
     Testnet   -> pabWalletPKHBytesTestnet

pabWalletPKH :: PaymentPubKeyHash
pabWalletPKH = PaymentPubKeyHash $ PubKeyHash $ foldr consByteString emptyByteString pabWalletPKHBytes

dispenserWalletIdString :: String
dispenserWalletIdString = case pabConfig of
     Simulator -> dispenserWalletIdStringSimulator
     Testnet   -> dispenserWalletIdStringTestnet

dispenserWallet :: Wallet
dispenserWallet = Wallet $ head $ rights [fromBase16 $ pack dispenserWalletIdString]

dispenserWalletPKHBytes :: [Integer]
dispenserWalletPKHBytes = case pabConfig of
     Simulator -> dispenserWalletPKHBytesSimulator
     Testnet   -> dispenserWalletPKHBytesTestnet

dispenserWalletPKH :: PaymentPubKeyHash
dispenserWalletPKH = PaymentPubKeyHash $ PubKeyHash $ foldr consByteString emptyByteString dispenserWalletPKHBytes

vestingHashBytes :: [Integer]
vestingHashBytes = [227,105,154,57,112,79,3,28,249,43,212,15,231,233,204,78,174,132,89,90,172,3,99,68,65,35,135,9]

vestingScriptPermanentHash :: ValidatorHash
vestingScriptPermanentHash = ValidatorHash $ foldr consByteString emptyByteString vestingHashBytes
