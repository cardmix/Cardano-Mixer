{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}

module Main
    (
        main
    ) where

import           Control.Concurrent                           (threadDelay)
import           Control.Monad.IO.Class                       (MonadIO (..))
import           Data.Aeson                                   (FromJSON(..), ToJSON(..), fromJSON, Result(..), decode)
import           Data.Proxy                                   (Proxy (..))
import           Data.Text                                    (Text, pack)
import           Data.UUID                                    (UUID)
import           GHC.Generics                                 (Generic)
import           Ledger.Ada                                   (lovelaceValueOf)
import           Ledger.Contexts                              (pubKeyHash, TxOutRef (TxOutRef))
import           Network.HTTP.Req
import           Plutus.Contracts.Currency                    (SimpleMPS(..), OneShotCurrency (OneShotCurrency), currencySymbol)
import           Plutus.PAB.Webserver.Types                   (ContractInstanceClientState(..), ContractActivationArgs (..))
import           Plutus.PAB.Events.ContractInstanceState      (PartiallyDecodedResponse(..))
import           Wallet.Emulator.Wallet                       (Wallet(..), walletPubKey)
import           Wallet.Types                                 (ContractInstanceId (..))

import           Crypto
import           Mixer                                        (DepositParams(..), WithdrawParams(..))
import           MixerFactory                                 (StartParams(..))
import AdminKey (adminKeySymbol, adminKeyTokenName)
import Data.Monoid (Last)
import Data.String
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Plutus.V1.Ledger.Bytes (LedgerBytes)
import Ledger (TxId(TxId), PubKeyHash)
import PlutusTx.Builtins (BuiltinByteString)
import Utility (buildByteString)
-- import PlutusTx.AssocMap (fromList)
import Crypto.MerkleTree (mimcHash)
import Crypto.ZKSNARK (ProveArguments(ProveArguments), emptyCRS)
import PlutusTx.Prelude (zero, one, fromMaybe, negate)
import qualified PlutusTx.Prelude ((*), (==), (-), inv, (+))
import           Data.ByteString.Lazy              (writeFile, readFile)
import Prelude hiding (readFile)
import Data.Map (fromList, elems, intersection)
import System.CPUTime
import MixerProofs (verifyWithdraw, generateWithdrawProof)
import System.Environment (getArgs)
import Crypto.DFT (extend)

data MixerContracts = MintAdminKey | Start | UseMixer
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

main :: IO ()
main = do
    f <- readFile (folderQAPs ++ fileQAPTarget)
    -- -- crs <- fromMaybe emptyCRS . decode <$> readFile (folderQAPs ++ folderCRS ++ fileCRS)
    r1cs <- loadR1CSFile "circuit-mixer.json"
    -- r1cs <- loadR1CSFile "circuit-test.json"
    let d = 10
        pkh = buildByteString "977efb35ab621d39dbeb7274ec7795a34708ff4d25a01a1df04c1f27"
        a   = dataToZp pkh
        cp0 = replicate d zero
        c0  = 0
        r1 = toZp 12451 :: Fr
        r2 = toZp 6788546 :: Fr
        r1' = toZp 890523 :: Fr
        r2' = toZp 35656 :: Fr
        h = mimcHash (toZp 0) r1
        k1 = mimcHash a r1
        k2 = mimcHash k1 r2
        cp  = addMerkleLeaf k2 (c0+1) cp0
        root = last cp

        l = replicate d zero :: [Fr]

        a' = toZp 1
        k3 = mimcHash zero a'
        k4 = mimcHash k3 r1'
        l' = mimcHash k4 r2'

        subsPub = [one, zero, zero, zero, zero, root, a, h, l', one] :: [Fr]
        subsPriv = fromList $ zip ([0] ++ [5..34])
         ([one, root, a, h, l', one, r1, r2] ++ init cp ++ l ++ [a', r1', r2'] :: [Fr])

        sol = solveR1CS r1cs subsPriv

    --     -- sa = SetupArguments 7 32 r1cs
    --     -- pa = ProveArguments sa crs sol        

        target = fromMaybe (toDFT []) (decode f)
    --     -- proof = prove (ZKProofSecret zero zero) pa target

    --     -- crsRed = reduceCRS crs
        (u, v, w, hh) = getR1CSPolynomials r1cs sol target
        -- t = idft target
        t = IDFT $ (PlutusTx.Prelude.negate one : replicate 32767 zero) ++ (one : replicate 32767 zero)
        aa = map (makeSub sol . leftCoefs) r1cs
        bb = map (makeSub sol . rightCoefs) r1cs
        cc = map (makeSub sol . outCoefs) r1cs
        bsol = zipWith (PlutusTx.Prelude.==) (zipWith (PlutusTx.Prelude.*) aa bb) cc
    print $ length $ filter (== False) bsol
    print $ length r1cs
    print $ cp
    print $ map ((elems sol) !! ) [1, 2, 3, 4]

    t1 <- getCPUTime
    proof <- generateWithdrawProof (root, a, h, l', one, r1, r2, init cp, l, a', r1', r2')
    print proof
    t2 <- getCPUTime
    print $ (fromIntegral (t2 - t1) :: Double) / (10^12)

    print $ verifyWithdraw (subsPub) (proof)
    t3 <- getCPUTime
    print $ (fromIntegral (t3 - t2) :: Double) / (10^12)

    let pkh1 = pubKeyHash $ walletPubKey $ Wallet 1
        pkh2 = pubKeyHash $ walletPubKey $ Wallet 2
    args <- getArgs
    case args of
        ["admin"]    -> mintAdminKeyProcedure -- for testing purposes
        ["start"]    -> startProcedure
        ["deposit"]  -> depositProcedure pkh1 pkh2
        ["withdraw"] -> withdrawProcedure pkh1 pkh2
        _            -> print ("Unknown command" :: String)

    -- example

    -- useProcedure

----------------------- Create mixer dApp logic ------------------------------

mintAdminKeyProcedure :: IO ()
mintAdminKeyProcedure = do
    cidAdmin <- activateRequest MintAdminKey (Wallet 1)
    endpointRequest "Create native token" cidAdmin (SimpleMPS adminKeyTokenName 1)
    threadDelay 1_000_000

----------------------------- Start mixer logic ------------------------------

startParams :: [StartParams]
startParams = map (\x -> StartParams x 10) lst
    where lst = [lovelaceValueOf 200_000_000, lovelaceValueOf 1000_000_000, lovelaceValueOf 10000_000_000]

startProcedure :: IO ()
startProcedure = do
    cidStart <- activateRequest Start (Wallet 1)
    mconcat $ map (startMixerProcedure cidStart) startParams
    print "Successfully started all mixers!"

startMixerProcedure :: UUID -> StartParams -> IO ()
startMixerProcedure uuid p = do
    endpointRequest "start" uuid p
    threadDelay 3_000_000

--------------------------------- Use mixer logic --------------------------------

depositProcedure :: PubKeyHash -> PubKeyHash -> IO ()
depositProcedure pkhFrom pkhTo = do
    cidUseMixer <- activateRequest UseMixer (Wallet 1)
    endpointRequest "deposit" cidUseMixer (DepositParams (lovelaceValueOf 200_000_000) (toZp 124))
    stopRequest cidUseMixer

withdrawProcedure :: PubKeyHash -> PubKeyHash -> IO ()
withdrawProcedure pkhFrom pkhTo = do
    cidUseMixer <- activateRequest UseMixer (Wallet 1)
    endpointRequest "withdraw" cidUseMixer (WithdrawParams (lovelaceValueOf 200_000_000) (pubKeyHash $ walletPubKey $ Wallet 2)
     (toZp 768) (toZp 844) (Proof O O O))
    stopRequest cidUseMixer

useProcedure :: IO ()
useProcedure = do
    cidUseMixer <- activateRequest UseMixer (Wallet 1)

    endpointRequest "deposit" cidUseMixer (DepositParams (lovelaceValueOf 10000_000_000) (toZp 124))
    threadDelay 2_000_000
    endpointRequest "deposit" cidUseMixer (DepositParams (lovelaceValueOf 10000_000_000) (toZp 3454))
    threadDelay 2_000_000
    endpointRequest "deposit" cidUseMixer (DepositParams (lovelaceValueOf 10000_000_000) (toZp 3463224))
    threadDelay 2_000_000

    endpointRequest "withdraw" cidUseMixer (WithdrawParams (lovelaceValueOf 10000_000_000) (pubKeyHash $ walletPubKey $ Wallet 2)
     (toZp 768) (toZp 844) (Proof O O O))

------------------------------- API Requests -------------------------------------

-- Activate a contract for a given wallet
activateRequest :: MixerContracts -> Wallet -> IO UUID
activateRequest x w = runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api" /: "contract" /: "activate")
        (ReqBodyJson $ ContractActivationArgs x w)
        (Proxy :: Proxy (JsonResponse ContractInstanceId))
        (port 9080)
    return $ unContractInstanceId $ responseBody v

-- Call an endpoint
endpointRequest :: (ToJSON p, Show p) => Text -> UUID -> p -> IO ()
endpointRequest endp uuid x = runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: endp)
        (ReqBodyJson x)
        (Proxy :: Proxy (JsonResponse ()))
        (port 9080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "Request processed: " ++ show x
        else "Error processing request!"

statusRequest :: (FromJSON p) => UUID -> IO (Maybe p)
statusRequest uuid = runReq defaultHttpConfig $ do
    v <- req
        GET
        (http "127.0.0.1" /: "api" /: "contract" /: "instance" /: pack (show uuid) /: "status")
        NoReqBody
        (Proxy :: Proxy (JsonResponse (ContractInstanceClientState MixerContracts)))
        (port 9080)
    let x = fromJSON $ observableState $ cicCurrentState $ responseBody v
    case x of
        Success tt -> return (Just tt)
        Error _    -> return Nothing

stopRequest :: UUID -> IO ()
stopRequest uuid = runReq defaultHttpConfig $ do
    v <- req
        PUT
        (http "127.0.0.1" /: "api" /: "contract" /: "instance" /: pack (show uuid) /: "stop")
        NoReqBody
        (Proxy :: Proxy (JsonResponse ()))
        (port 9080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "Request processed!"
        else "Error processing request!"