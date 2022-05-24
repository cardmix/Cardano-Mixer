{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}


module Main
    (
        main
    ) where

import           Data.Aeson                                   (encode, decode)
import           Data.ByteString                              (ByteString, writeFile, readFile)
import           Data.ByteString.Lazy                         (fromStrict, toStrict)
import           Data.Text                                    (Text, pack)
import           Ledger                                       (PaymentPubKeyHash, Value)
import           Plutus.V1.Ledger.Ada                         (lovelaceValueOf)
import           PlutusTx.Prelude                             hiding ((<$>))
import           Prelude                                      (IO, Show(..), String, FilePath, (<$>), print)
import           System.Directory                             
import           System.Environment                           (getArgs)
import           Wallet.Emulator.Wallet                       (Wallet)

import           Crypto                                       (Fr, PublicInputs(..), mimcHash, generateProofSecret)
import           MixerContractParams
import           MixerProofs                                  (generateSimulatedWithdrawProof)
import           MixerState                                   (MixerState)
import           MixerUserData
import           PABContracts                                 (PABContracts(..), MixerFrontendContracts (..))
import           Requests


pabIP :: Text
pabIP = "127.0.0.1"

mixVal :: Value
mixVal = lovelaceValueOf 20_000

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["deposit",  str] -> depositProcedure str
        ["submit"]        -> depositSubmitProcedure
        ["withdraw", str] -> withdrawProcedure str
        _                 -> print ("Unknown command" :: String)

--------------------------------- Use mixer logic --------------------------------

depositProcedure :: String -> IO ()
depositProcedure str = do
    (_, _, w) <- mixerConnectProcedure str

    ds   <- generateDepositSecret
    sas  <- generateShieldedAccountSecret
    logSecrets ds sas
    let leaf = mimcHash (getR1 ds) (getR2 ds)
    print $ mimcHash zero (getR1 ds)
    cidMixerUse <- activateRequest pabIP (FrontendContracts MixerUse) (Just w)
    endpointRequest pabIP "deposit" cidMixerUse (DepositParams (pack str) (lovelaceValueOf 20_000) leaf)
    bs <- awaitStatusUpdate pabIP cidMixerUse :: IO ByteString
    writeFile "tx.raw" bs

depositSubmitProcedure :: IO ()
depositSubmitProcedure = do
    (_, _, w) <- mixerConnectProcedure ""

    bs <- readFile "tx.signed"
    cidMixerUse <- activateRequest pabIP (FrontendContracts MixerUse) (Just w)
    endpointRequest pabIP "depositSubmit" cidMixerUse bs

withdrawProcedure :: String -> IO ()
withdrawProcedure str = do
    (_, a, w) <- mixerConnectProcedure str

    secret <- getSecrets
    case secret of
      Nothing        -> print @String "Nothing to withdraw."
      Just (ds, sas) -> do
        state <- mixerStateProcedure
        randomness <- generateProofSecret
        let (lastDeposit, subs, proof) = generateSimulatedWithdrawProof randomness a ds sas state
            params = WithdrawParams (pack str) mixVal lastDeposit subs proof
        cidMixerUse <- activateRequest pabIP (FrontendContracts MixerUse) (Just w)
        endpointRequest pabIP "withdraw" cidMixerUse params

------------------------------- Query mixer logic --------------------------------

mixerConnectProcedure :: String -> IO (PaymentPubKeyHash, Fr, Wallet)
mixerConnectProcedure str = do
    cidPAB <- activateRequest pabIP (FrontendContracts ConnectToPAB) (Nothing :: Maybe Wallet)
    endpointRequest pabIP "Connect to PAB" cidPAB str
    awaitStatusUpdate pabIP cidPAB

mixerStateProcedure :: IO MixerState
mixerStateProcedure = do
    cidQueryMixer <- activateRequest pabIP (FrontendContracts MixerStateQuery) (Nothing :: Maybe Wallet)
    endpointRequest pabIP "Get Mixer state" cidQueryMixer mixVal
    awaitStatusUpdate pabIP cidQueryMixer

------------------------------------ File logging ----------------------------------

fileDS :: FilePath
fileDS  = "testnet/Deposits/DepositSecret"

fileSAS :: FilePath
fileSAS = "testnet/Deposits/ShieldedAccountSecret"

logSecrets :: DepositSecret -> ShieldedAccountSecret -> IO ()
logSecrets ds sas = do
    i <- findLastFile
    writeFile (fileDS ++ show i) (toStrict $ encode ds)
    writeFile (fileSAS ++ show i) (toStrict $ encode sas)

getSecrets :: IO (Maybe (DepositSecret, ShieldedAccountSecret))
getSecrets = do
    i <- findLastFile
    if i > 0
        then do
            Just ds <- decode . fromStrict <$> readFile (fileDS ++ show (i-1))
            Just sas <- decode . fromStrict <$> readFile (fileSAS ++ show (i-1))
            removeFile (fileDS ++ show (i-1))
            removeFile (fileSAS ++ show (i-1))
            return $ Just (ds, sas)
        else return Nothing

findLastFile :: IO Integer
findLastFile = go 0
    where
        go i = do
            b <- doesFileExist $ fileDS ++ show i
            if b then go (i+1) else return i
