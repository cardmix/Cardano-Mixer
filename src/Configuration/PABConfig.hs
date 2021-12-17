{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Configuration.PABConfig where

import Data.Either                   (rights)
import Data.Text                     (pack)
import PlutusTx.Prelude
import Prelude                       (String)
import Wallet.Emulator.Wallet        (Wallet(..), fromBase16, mockWalletPaymentPubKeyHash)

import Crypto                        

----------------------------- Testnet ------------------------------

-- pabWalletIdString :: String
-- pabWalletIdString = "2c922c0b34abf5ab0d7f3f290b9f6c8874a4d300"


-- adminKeyPolicyId :: [Integer]
-- adminKeyPolicyId = [0x14, 0xca, 0x69, 0xe5, 0x9f, 0x3e, 0xa7, 0xcb, 0x8d, 0x74,
--      0x83, 0x0c, 0xc1, 0x8d, 0x52, 0x86, 0x60, 0xcd, 0xa0, 0xc4, 0x5e, 0xe7, 0x84, 0xdf, 0xb3, 0x2c, 0x95, 0xbf]

---------------------------- Simulator -----------------------------

-- pabWalletIdString :: String
-- pabWalletIdString = "1bc5f27d7b4e20083977418e839e429d00cc87f3"


-- adminKeyPolicyId :: [Integer]
-- adminKeyPolicyId = [0x31, 0xa4, 0x0b, 0xa2, 0x38, 0x86, 0xd3, 0x9e, 0xb9, 0x89, 0xc9, 0xc0, 0xf5, 0x20, 0xde, 0x5f,
--      0x4b, 0x2c, 0x58, 0xb2, 0x9e, 0x54, 0xbf, 0x52, 0x14, 0x6b, 0xea, 0x54]


---------------------------- Emulator -----------------------------

pabWalletIdString :: String
pabWalletIdString = "1bc5f27d7b4e20083977418e839e429d00cc87f3"


adminKeyPolicyId :: [Integer]
adminKeyPolicyId = [0x18, 0x03, 0x07, 0xc3, 0x48, 0xf6, 0x48, 0x28, 0xb5, 0x8b, 0xa1, 0x19, 0x45, 0x8f, 0x41, 0xd9,
     0x9b, 0xfe, 0xd8, 0x23, 0x72, 0xba, 0xfb, 0xc0, 0x82, 0x9e, 0x05, 0xc6]


----------------------------- Common -------------------------------

pabWallet :: Wallet
pabWallet = Wallet $ head $ rights [fromBase16 $ pack pabWalletIdString]


----------------------------- Test values --------------------------

pabTestValues :: (Fr, Proof, Fr, Fr, Fr, Fr)
pabTestValues = (leaf, proof, h, hA, oh, nh)
     where r1 = toZp 12451 :: Fr
           r2 = toZp 6788546 :: Fr
           h  = mimcHash (toZp 0) r1
           hA = mimcHash (dataToZp $ mockWalletPaymentPubKeyHash pabWallet) r2
           v1 = toZp 890523 :: Fr
           v2 = toZp 35656 :: Fr
           v3 = toZp 97346 :: Fr
           oh = mimcHash v1 v2
           nh = mimcHash v1 v3
           leaf  = Zp 6607553988888913206274753584799503904250064978416565150316268919259420287010
           proof = Proof (CP (Zp 147929808083696038139150660170093020905446402282007548334683308419830474875725291275993472069334540777065719493720)
               (Zp 1272925944532861791823362929194866059563351363474583526448565967885052280906152428139481135824152475979139570870016))
               (CP (E (P [Zp 403128792411863184882153809851131467866528989958253365879955937772370668055182312565552870625414765889643882627504,
               Zp 3362616684172831066620751147294760107294990039928694321338688645478221226761272036861248921864678085347329741440212]))
               (E (P [Zp 1096863617790010713464472462853176126098604053264746661185987657758386698634484273632555358363329144235118183871838,
               Zp 3650741862608979947230363819352517296784056154425141723178573322637161159193402792434651284255713327572074817837009])))
               (CP (Zp 2792671801056404575446809177230240319828877167871412108837951492033276057259620837122507303397047791758862274564120)
                (Zp 2656810241898813722596937260302802693984101647756483008909501977046159253684917605904727025758591882567696972916510))




