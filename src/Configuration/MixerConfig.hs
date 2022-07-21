{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}

module Configuration.MixerConfig where

import           Ledger                        (Value)
import           Ledger.Ada                    (lovelaceValueOf)
import           Plutus.Contract               (Contract, ContractError (..), throwError)
import           PlutusTx.Prelude              hiding (elem)

import           Configuration.PABConfig       (mixTokenTxOutRef)
import           Tokens.MIXToken               (mixToken)


------------------------ Config options --------------------------

mixingValuesTable :: [[Value]]
mixingValuesTable = [ map lovelaceValueOf [20_000, 200_000, 2_000_000],
     map (\n -> lovelaceValueOf 4_000 + scale n (mixToken mixTokenTxOutRef)) [10, 100, 1000]]

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
