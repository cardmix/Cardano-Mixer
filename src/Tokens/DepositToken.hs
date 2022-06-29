{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module Tokens.DepositToken where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           Plutus.V1.Ledger.Ada             (toValue)
import           Plutus.V1.Ledger.Value           (geq)
import           PlutusTx                         (compile, applyCode, liftCode)
import           PlutusTx.AssocMap                (singleton)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger, mempty)
import           Prelude                          (undefined)

import           Crypto
import           Scripts.Constraints              (tokensMinted, utxoReferenced, getUpperTimeEstimate, checkDatum, findUtxoProduced, utxoProduced)
import           Scripts.MixerScript
import           Scripts.VestingScript            ()
import           Tokens.RealDepositToken          (DepositTokenRedeemer, depositTokenName)
import           Types.TxConstructor              (TxConstructor(..))

--------------------------------------- On-Chain ------------------------------------------

type DepositTokenParams = (Mixer, Address)

-- The script must:
-- 0) have the token name that is a result of hashing (time, leaf, root, owner pkh, previous deposit token name)
-- three possible failures: wrong root or wrong previous deposit token or invalid previous deposit token
-- 1) mint the exact amount of the asset class as expected
-- 2) reference the corresponding mixer script input (check value and leaf)
-- 3) reference the input with the previous deposit token
-- 4) produce input in the vesting script with the deposit token there (check datum for correct time)
checkPolicy :: DepositTokenParams -> DepositTokenRedeemer -> ScriptContext -> Bool
checkPolicy (mixer, addr) red@(name0@(TokenName tn), Zp l, _) ctx@ScriptContext{scriptContextTxInfo=info}
    | tn == ""    =
    let name      = depositTokenName (TokenName "", zero, "")
        vDep      = token $ AssetClass (ownCurrencySymbol ctx, name)

        cond1     = tokensMinted ctx (singleton name 1)
        cond2     = utxoProduced info (== TxOut (mFailAddress  mixer) (toValue minAdaTxOut + vDep) Nothing)
    in cond1 && cond2
    | otherwise   = 
    let name      = depositTokenName red
        vMixer    = mixerDepositValue mixer
        vDep      = token $ AssetClass (ownCurrencySymbol ctx, name)
        vDepLast  = token $ AssetClass (ownCurrencySymbol ctx, name0)

        f1 o      = txOutAddress o == addr && txOutValue o == vMixer
        g1 leaf   = leaf == Zp l

        ct        = getUpperTimeEstimate info
        f2 o      = txOutAddress o == mixerDepositVestingAddress mixer && txOutValue o == mixerVestingValue vMixer + vDep
        g2 (t, _) = t == ct + vestingDuration

        cond1     = tokensMinted ctx (singleton name 1) -- we make restriction of one deposit token minted per mixer per transaction
        cond2     = checkDatum info (g1 :: Fr -> Bool) $ findUtxoProduced info f1
        cond3     = utxoReferenced info (\o -> txOutValue o `geq` vDepLast)
        cond4     = checkDatum info (g2 :: (POSIXTime, PaymentPubKeyHash) -> Bool) $ findUtxoProduced info f2
    in cond1 && cond2 && cond3 && cond4

curPolicy :: DepositTokenParams -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

-------------------------- Off-Chain -----------------------------

depositTokenSymbol :: DepositTokenParams -> CurrencySymbol
depositTokenSymbol par = scriptCurrencySymbol $ curPolicy par

depositTokenAssetClass :: DepositTokenParams -> DepositTokenRedeemer -> AssetClass
depositTokenAssetClass par red = AssetClass (depositTokenSymbol par, depositTokenName red)

depositToken :: DepositTokenParams -> DepositTokenRedeemer -> Value
depositToken par d = token $ depositTokenAssetClass par d

-- TxConstraints that Deposit Token is minted and sent by the transaction
depositTokenMintTx :: DepositTokenParams -> DepositTokenRedeemer -> TxConstructor a i o -> TxConstructor a i o
depositTokenMintTx _ _ _ = undefined
