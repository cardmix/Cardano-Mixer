{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Configuration.PABConfig where

import Data.Either                   (rights)
import Data.Text                     (pack)
import Ledger                        (PubKeyHash(..))
import Ledger.Address                (PaymentPubKeyHash(..))
import Plutus.V1.Ledger.Api          (ValidatorHash (..))
import PlutusTx.Prelude
import Prelude                       (String)
import Wallet.Emulator.Wallet        (Wallet(..), fromBase16)


----------------------------- Testnet ------------------------------

-- pabWalletIdString :: String
-- pabWalletIdString = "2c922c0b34abf5ab0d7f3f290b9f6c8874a4d300"

-- pabWalletPKHBytes :: [Integer]
-- pabWalletPKHBytes = [0x46, 0x62, 0x9e, 0xfc, 0x9c, 0x5d, 0xdf, 0x1a, 0x78, 0x36, 0x0f, 0x4d, 0xa4, 0xdc, 0x38,
--      0x72, 0x87, 0xa8, 0x23, 0x8a, 0x57, 0x27, 0xce, 0x7d, 0x02, 0xf6, 0xb6, 0x58]

-- adminKeyPolicyId :: [Integer]
-- adminKeyPolicyId = [0x14, 0xca, 0x69, 0xe5, 0x9f, 0x3e, 0xa7, 0xcb, 0x8d, 0x74,
--      0x83, 0x0c, 0xc1, 0x8d, 0x52, 0x86, 0x60, 0xcd, 0xa0, 0xc4, 0x5e, 0xe7, 0x84, 0xdf, 0xb3, 0x2c, 0x95, 0xbf]

---------------------------- Simulator -----------------------------

-- pabWalletIdString :: String
-- pabWalletIdString = "1bc5f27d7b4e20083977418e839e429d00cc87f3"

-- pabWalletPKHBytes :: [Integer]
-- pabWalletPKHBytes = [0xc6, 0x05, 0x88, 0x8d, 0x3c, 0x40, 0x38, 0x6d, 0x7c, 0x32, 0x3a, 0x46, 0x79, 0xc7, 0x67, 0xe5,
--      0xa0, 0xa7, 0xb6, 0x83, 0x60, 0x5c, 0x3e, 0x5d, 0xf9, 0xa7, 0x6a, 0xee]

-- vestingHashBytes :: [Integer]
-- vestingHashBytes = [0x16, 0x9c, 0x6d, 0x32, 0x25, 0xdf, 0xaf, 0xc6, 0x9d, 0x86, 0xc9, 0x35, 0xaf, 0x89, 0x74, 0x48,
--      0xa8, 0xc9, 0x6d, 0x02, 0xd9, 0x2b, 0x11, 0x33, 0x48, 0xa7, 0x17, 0x6b]

-- adminKeyPolicyId :: [Integer]
-- adminKeyPolicyId = [0x31, 0xa4, 0x0b, 0xa2, 0x38, 0x86, 0xd3, 0x9e, 0xb9, 0x89, 0xc9, 0xc0, 0xf5, 0x20, 0xde, 0x5f,
--      0x4b, 0x2c, 0x58, 0xb2, 0x9e, 0x54, 0xbf, 0x52, 0x14, 0x6b, 0xea, 0x54]

---------------------------- Emulator -----------------------------

pabWalletIdString :: String
pabWalletIdString = "1bc5f27d7b4e20083977418e839e429d00cc87f3"

pabWalletPKHBytes :: [Integer]
pabWalletPKHBytes = [0xc6, 0x05, 0x88, 0x8d, 0x3c, 0x40, 0x38, 0x6d, 0x7c, 0x32, 0x3a, 0x46, 0x79, 0xc7, 0x67, 0xe5,
     0xa0, 0xa7, 0xb6, 0x83, 0x60, 0x5c, 0x3e, 0x5d, 0xf9, 0xa7, 0x6a, 0xee]

vestingHashBytes :: [Integer]
vestingHashBytes = [45,8,180,170,253,58,242,52,65,224,95,103,190,73,104,24,232,31,9,68,202,25,245,183,229,60,120,79]

adminTokenPolicyId :: [Integer]
adminTokenPolicyId = [0x18, 0x03, 0x07, 0xc3, 0x48, 0xf6, 0x48, 0x28, 0xb5, 0x8b, 0xa1, 0x19, 0x45, 0x8f, 0x41, 0xd9,
     0x9b, 0xfe, 0xd8, 0x23, 0x72, 0xba, 0xfb, 0xc0, 0x82, 0x9e, 0x05, 0xc6]

relayTokenPolicyId :: [Integer]
relayTokenPolicyId = [0x18, 0x03, 0x07, 0xc3, 0x48, 0xf6, 0x48, 0x28, 0xb5, 0x8b, 0xa1, 0x19, 0x45, 0x8f, 0x41, 0xd9,
     0x9b, 0xfe, 0xd8, 0x23, 0x72, 0xba, 0xfb, 0xc0, 0x82, 0x9e, 0x05, 0xc6]

collateralTokenPolicyId :: [Integer]
collateralTokenPolicyId = [0x18, 0x03, 0x07, 0xc3, 0x48, 0xf6, 0x48, 0x28, 0xb5, 0x8b, 0xa1, 0x19, 0x45, 0x8f, 0x41, 0xd9,
     0x9b, 0xfe, 0xd8, 0x23, 0x72, 0xba, 0xfb, 0xc0, 0x82, 0x9e, 0x05, 0xc6]

mixTokenPolicyId :: [Integer]
mixTokenPolicyId = [0x18, 0x03, 0x07, 0xc3, 0x48, 0xf6, 0x48, 0x28, 0xb5, 0x8b, 0xa1, 0x19, 0x45, 0x8f, 0x41, 0xd9,
     0x9b, 0xfe, 0xd8, 0x23, 0x72, 0xba, 0xfb, 0xc0, 0x82, 0x9e, 0x05, 0xc6]

mixStakingTokenPolicyId :: [Integer]
mixStakingTokenPolicyId = [0x18, 0x03, 0x07, 0xc3, 0x48, 0xf6, 0x48, 0x28, 0xb5, 0x8b, 0xa1, 0x19, 0x45, 0x8f, 0x41, 0xd9,
     0x9b, 0xfe, 0xd8, 0x23, 0x72, 0xba, 0xfb, 0xc0, 0x82, 0x9e, 0x05, 0xc6]

oracleTokenPolicyId :: [Integer]
oracleTokenPolicyId = [0x18, 0x03, 0x07, 0xc3, 0x48, 0xf6, 0x48, 0x28, 0xb5, 0x8b, 0xa1, 0x19, 0x45, 0x8f, 0x41, 0xd9,
     0x9b, 0xfe, 0xd8, 0x23, 0x72, 0xba, 0xfb, 0xc0, 0x82, 0x9e, 0x05, 0xc6]


----------------------------- Common -------------------------------

pabWallet :: Wallet
pabWallet = Wallet $ head $ rights [fromBase16 $ pack pabWalletIdString]

pabWalletPKH :: PaymentPubKeyHash
pabWalletPKH = PaymentPubKeyHash $ PubKeyHash $ foldr consByteString emptyByteString pabWalletPKHBytes

vestingScriptPermanentHash :: ValidatorHash
vestingScriptPermanentHash = ValidatorHash $ foldr consByteString emptyByteString vestingHashBytes
           
           
           
           
           





