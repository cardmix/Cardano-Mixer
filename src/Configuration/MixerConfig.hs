{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}

module Configuration.MixerConfig where

import           Ledger                        (Value, TxOutRef)
import           Plutus.Contract               (Contract, ContractError (..), throwError)
import           Plutus.V1.Ledger.Ada          (lovelaceValueOf)
import           PlutusTx.Prelude              hiding (elem)

import           Mixer                         (Mixer, MixerInstance (..))
import           Scripts.ADAWithdrawScript     (adaWithdrawAddress, adaWithdrawValidatorHash)
import           Scripts.MixerScript           (mixerAddress, mixerValidatorHash)
import           Tokens.DepositToken           (depositTokenSymbol)
import           Tokens.MixerBeaconToken       (mixerBeaconCurrencySymbol, mixerBeaconTokenName)
import           Tokens.MIXToken               (mixToken)
import           Tokens.WithdrawToken          (withdrawTokenSymbol)

------------------------ Instantiating ---------------------------

toMixerInstance :: Mixer -> TxOutRef -> TxOutRef -> MixerInstance
toMixerInstance mixer beaconRef withdrawRef = 
          MixerInstance mixer beaconRef withdrawRef mixerBeaconTokenName bSymb dSymb wSymb adaWithdrawAddress mAddr adaWithdrawValidatorHash mVH
     where
          bSymb = mixerBeaconCurrencySymbol beaconRef
          dSymb = depositTokenSymbol (mixer, (bSymb, mixerBeaconTokenName))
          wSymb = withdrawTokenSymbol (mixer, dSymb, adaWithdrawAddress, withdrawRef)
          mAddr = mixerAddress wSymb
          mVH   = mixerValidatorHash wSymb

------------------------ Config options --------------------------

mixingValuesTable :: [[Value]]
mixingValuesTable = [ map lovelaceValueOf [20_000, 200_000, 2_000_000],
     map (\n -> lovelaceValueOf 4_000 + scale n mixToken) [10, 100, 1000]]

---------------------------- Interface ---------------------------

mixingValues :: Integer -> Integer -> Contract w s ContractError Value
mixingValues n m = if mixingValuesCorrectChoice n m
     then pure $ (mixingValuesTable !! (n-1)) !! (m-1)
     else throwError $ OtherContractError "Deposit call has incorrect mixing value!"

---------------------------- Utility -----------------------------

mixingValuesMaxAmount :: Integer -> Integer
mixingValuesMaxAmount n = nAmounts
     where nAssets  = length mixingValuesTable
           nAmounts = if n <= nAssets then length (mixingValuesTable !! (n-1)) else 0

mixingValuesCorrectChoice :: Integer -> Integer -> Bool
mixingValuesCorrectChoice n m = m > 0 && m <= mMax
     where mMax = mixingValuesMaxAmount n
