{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Configuration.PABConfig where

import Data.Either                   (rights)
import Data.Text                     (pack)
import Ledger                        (PubKeyHash(..))
import Ledger.Address                (PaymentPubKeyHash(..))
import PlutusTx.Prelude
import Prelude                       (String, IO)
import Wallet.Emulator.Wallet        (Wallet(..), fromBase16)

import Crypto
import Utility                       (replicate, last, init)
import MixerProofs                   (generateSimulatedWithdrawProof)


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

-- adminKeyPolicyId :: [Integer]
-- adminKeyPolicyId = [0x31, 0xa4, 0x0b, 0xa2, 0x38, 0x86, 0xd3, 0x9e, 0xb9, 0x89, 0xc9, 0xc0, 0xf5, 0x20, 0xde, 0x5f,
--      0x4b, 0x2c, 0x58, 0xb2, 0x9e, 0x54, 0xbf, 0x52, 0x14, 0x6b, 0xea, 0x54]


---------------------------- Emulator -----------------------------

pabWalletIdString :: String
pabWalletIdString = "1bc5f27d7b4e20083977418e839e429d00cc87f3"

pabWalletPKHBytes :: [Integer]
pabWalletPKHBytes = [0xc6, 0x05, 0x88, 0x8d, 0x3c, 0x40, 0x38, 0x6d, 0x7c, 0x32, 0x3a, 0x46, 0x79, 0xc7, 0x67, 0xe5,
     0xa0, 0xa7, 0xb6, 0x83, 0x60, 0x5c, 0x3e, 0x5d, 0xf9, 0xa7, 0x6a, 0xee]

adminKeyPolicyId :: [Integer]
adminKeyPolicyId = [0x18, 0x03, 0x07, 0xc3, 0x48, 0xf6, 0x48, 0x28, 0xb5, 0x8b, 0xa1, 0x19, 0x45, 0x8f, 0x41, 0xd9,
     0x9b, 0xfe, 0xd8, 0x23, 0x72, 0xba, 0xfb, 0xc0, 0x82, 0x9e, 0x05, 0xc6]


----------------------------- Common -------------------------------

pabWallet :: Wallet
pabWallet = Wallet $ head $ rights [fromBase16 $ pack pabWalletIdString]

pabWalletPKH :: PaymentPubKeyHash
pabWalletPKH = PaymentPubKeyHash $ PubKeyHash $ foldr consByteString emptyByteString pabWalletPKHBytes

----------------------------- Test values --------------------------

pabTestValues :: IO (Fr, Proof, Fr, Fr, Fr, Fr)
pabTestValues = do
          proof <- generateSimulatedWithdrawProof (root, a, h, hA, one, oh, nh, r1, r2, init cp, l, v1, v2, v3)
          return (leaf, proof, h, hA, oh, nh)
                    -- if isSubsequenceOf pabWalletIdString "1bc5f27d7b4e20083977418e839e429d00cc87f3"
                    -- then return (leaf, proofSim, h, hA, oh, nh)
                    -- else do
                    --      proof <- generateSimulatedWithdrawProof (root, a, h, hA, one, oh, nh, r1, r2, init cp, l, v1, v2, v3)
                    --      return (leaf, proofTest, h, hA, oh, nh)
     where r1 = toZp 12451 :: Fr
           r2 = toZp 6788546 :: Fr
           h  = mimcHash (toZp 0) r1
           hA = mimcHash (dataToZp pabWalletPKH) r2
           v1 = toZp 890523 :: Fr
           v2 = toZp 35656 :: Fr
           v3 = toZp 97346 :: Fr
           oh = mimcHash v1 v2
           nh = mimcHash v1 v3
           leaf  = Zp 6607553988888913206274753584799503904250064978416565150316268919259420287010

           d   = 10
           a   = dataToZp pabWalletPKH
           cp0 = replicate d zero
           c0  = 0
           k = mimcHash r1 r2
           cp  = addMerkleLeaf k (c0+1) cp0
           root = last cp
           l = replicate d zero :: [Fr]

          --  proofTest = Proof (CP (Zp 2048407857294394034536740515984414785037858144681451023485110397043269481291872973763725222403472202917853268851967)
          --      (Zp 1076178257968688651306528175502508605060323609365641523915943506464513593623884534786121919191570449218996663181525))
          --      (CP (E (P [Zp 2135258151344577700828490612463740044658695445771719396782378358775137765864989529193045783437079294090731620224699,
          --      Zp 2334206688100172622770476420665397923941837372135708797293855176291459368483128568647156659658114791166084658178771]))
          --      (E (P [Zp 3894706755604952774638474855752691457370716018087611624479782286679375988407352044812521466131744569245315801986410,
          --      Zp 2125766704098901788292185188277728156582217346694448892772672731523257693820834599543612668911922271793155980727927])))
          --      (CP (Zp 3364884976756393888483075101215917793719730940066818403122380881233485362835214035019932443826519915871758012777968)
          --      (Zp 1023743870972013919178029415052010871985184750188930388841054009257457708295807544797103927338176201546509306638276))

          --  proofSim = Proof (CP (Zp 147929808083696038139150660170093020905446402282007548334683308419830474875725291275993472069334540777065719493720)
          --      (Zp 1272925944532861791823362929194866059563351363474583526448565967885052280906152428139481135824152475979139570870016))
          --      (CP (E (P [Zp 403128792411863184882153809851131467866528989958253365879955937772370668055182312565552870625414765889643882627504,
          --      Zp 3362616684172831066620751147294760107294990039928694321338688645478221226761272036861248921864678085347329741440212]))
          --      (E (P [Zp 1096863617790010713464472462853176126098604053264746661185987657758386698634484273632555358363329144235118183871838,
          --      Zp 3650741862608979947230363819352517296784056154425141723178573322637161159193402792434651284255713327572074817837009])))
          --      (CP (Zp 2792671801056404575446809177230240319828877167871412108837951492033276057259620837122507303397047791758862274564120)
          --       (Zp 2656810241898813722596937260302802693984101647756483008909501977046159253684917605904727025758591882567696972916510))





