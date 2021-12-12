
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Config where


import Data.Either                   (rights)
import Data.Text                     (pack)
import PlutusTx.Prelude
import Prelude                       (String)
import Wallet.Emulator.Wallet        (Wallet(..), fromBase16)


----------------------------- Testnet ------------------------------

-- pabWalletIdString :: String
-- pabWalletIdString = "2c922c0b34abf5ab0d7f3f290b9f6c8874a4d300"


-- adminKeyPolicyId :: [Integer]
-- adminKeyPolicyId = [0x14, 0xca, 0x69, 0xe5, 0x9f, 0x3e, 0xa7, 0xcb, 0x8d, 0x74,
--      0x83, 0x0c, 0xc1, 0x8d, 0x52, 0x86, 0x60, 0xcd, 0xa0, 0xc4, 0x5e, 0xe7, 0x84, 0xdf, 0xb3, 0x2c, 0x95, 0xbf]

---------------------------- Simulator -----------------------------

pabWalletIdString :: String
pabWalletIdString = "1bc5f27d7b4e20083977418e839e429d00cc87f3"


adminKeyPolicyId :: [Integer]
adminKeyPolicyId = [0x31, 0xa4, 0x0b, 0xa2, 0x38, 0x86, 0xd3, 0x9e, 0xb9, 0x89, 0xc9, 0xc0, 0xf5, 0x20, 0xde, 0x5f,
     0x4b, 0x2c, 0x58, 0xb2, 0x9e, 0x54, 0xbf, 0x52, 0x14, 0x6b, 0xea, 0x54]


---------------------------- Emulator -----------------------------

-- pabWalletIdString :: String
-- pabWalletIdString = "1bc5f27d7b4e20083977418e839e429d00cc87f3"


-- adminKeyPolicyId :: [Integer]
-- adminKeyPolicyId = [0x18, 0x03, 0x07, 0xc3, 0x48, 0xf6, 0x48, 0x28, 0xb5, 0x8b, 0xa1, 0x19, 0x45, 0x8f, 0x41, 0xd9,
--      0x9b, 0xfe, 0xd8, 0x23, 0x72, 0xba, 0xfb, 0xc0, 0x82, 0x9e, 0x05, 0xc6]


----------------------------- Common -------------------------------

pabWallet :: Wallet
pabWallet = Wallet $ head $ rights [fromBase16 $ pack pabWalletIdString]








