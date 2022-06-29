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

module Tokens.RealWithdrawToken where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           Plutus.V1.Ledger.Api             (Credential(..))
import           PlutusTx                         (compile, applyCode, liftCode, unstableMakeIsData)
import           PlutusTx.AssocMap                (singleton)
import           PlutusTx.Prelude                 
import           Prelude                          (undefined)

import           Crypto
import           MixerProofs                      (verifyWithdraw, toWithdrawPublicSignals)
import           Scripts.Constraints              (tokensMinted)
import           Scripts.VestingScript            ()
import           Tokens.RealDepositToken          (RealDepositComputationData)
import           Types.TxConstructor              (TxConstructor)
import           Utils.ByteString                 (ToBuiltinByteString(..))
import           Utils.Common                     (ToIntegerData(..))

--------------------------- On-Chain -----------------------------

type RealWithdrawParams = ()

type RealWithdrawComputationData = ([Fr], Proof)

type RealWithdrawRedeemer = (RealWithdrawComputationData, Bool)

{-# INLINABLE realWithdrawTokenName #-}
realWithdrawTokenName :: RealWithdrawRedeemer -> TokenName
realWithdrawTokenName ((subs, proof), res) = TokenName $ sha2_256 $
    toBytes (map fromZp subs)
    `appendByteString` toBytes (toIntegerData proof)
    `appendByteString` toBytes res

-- The script must:
-- 0) have the token name that is a result of hashing (public signals, proof)
-- 1) mint the exact amount of the asset class as expected
-- 2) check the proof
checkPolicy :: RealWithdrawParams -> RealWithdrawRedeemer -> ScriptContext -> Bool
checkPolicy _ red@((subs, proof), res) ctx = cond1 && cond2
    where name  = realWithdrawTokenName red
          
          cond1 = tokensMinted ctx (singleton name 1) -- we make restriction of one token minted per mixer per transaction
          cond2 = res == verifyWithdraw (toWithdrawPublicSignals $ PublicInputs subs) proof

curPolicy :: RealWithdrawParams -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

type WithdrawComputationData = (Address, [Fr], Proof)

type WithdrawTokenRedeemer = (WithdrawComputationData, TokenName)

{-# INLINABLE withdrawTokenName #-}
withdrawTokenName :: WithdrawTokenRedeemer -> TokenName
withdrawTokenName ((Address (PubKeyCredential (PubKeyHash pkh)) _, subs, proof), TokenName dtName) =
    TokenName $ sha2_256 $ pkh
    `appendByteString` toBytes (map fromZp subs)
    `appendByteString` toBytes (toIntegerData proof)
    `appendByteString` dtName
withdrawTokenName _ = error ()

-- Type for InvalidWithdrawToken script
data InvalidWithdraw = InvalidWithdrawProof WithdrawTokenRedeemer
                     | InvalidWithdrawRoot (TokenName, Fr, RealDepositComputationData, WithdrawComputationData)
                     | InvalidWithdrawAddress WithdrawTokenRedeemer

PlutusTx.unstableMakeIsData ''InvalidWithdraw

-------------------------- Off-Chain -----------------------------

realWithdrawCurrencySymbol :: RealWithdrawParams -> CurrencySymbol
realWithdrawCurrencySymbol = scriptCurrencySymbol . curPolicy

realWithdrawAssetClass :: RealWithdrawParams -> RealWithdrawRedeemer -> AssetClass
realWithdrawAssetClass par red = AssetClass (realWithdrawCurrencySymbol par, realWithdrawTokenName red)

realWithdrawToken :: RealWithdrawParams -> RealWithdrawRedeemer -> Value
realWithdrawToken par = token . realWithdrawAssetClass par

realWithdrawTokenTx :: RealWithdrawParams -> RealWithdrawRedeemer -> TxConstructor a i o -> TxConstructor a i o
realWithdrawTokenTx _ _ _ = undefined
