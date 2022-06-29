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


module Tokens.WithdrawToken where

import qualified Data.Map
import           Data.Maybe                       (fromJust)
import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints.OffChain      (ScriptLookups)
import           Ledger.Constraints.TxConstraints (TxConstraints, mustPayToOtherScript)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           Plutus.V1.Ledger.Ada             (lovelaceValueOf)
import           Plutus.V1.Ledger.Api             (Credential(..))
import           PlutusTx                         (compile, toBuiltinData, applyCode, liftCode)
import           PlutusTx.AssocMap                (singleton)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger, mempty)
import           Prelude                          ((<>), mempty)

import           Crypto
import           Scripts.Constraints              (tokensMinted, utxoSpent, utxoProduced, tokensMintedTx, getUpperTimeEstimate, checkDatum, findUtxoProduced)
import           Scripts.MixerScript
import           Scripts.VestingScript            ()
import           Types.TxConstructor              (TxConstructor(..))
import           Utils.ByteString                 (ToBuiltinByteString(..))
import           Utils.Common                     (ToIntegerData(..))

------------------------------------ Deposit Token -----------------------------------------------

type WithdrawTokenParams = (Mixer, Address, CurrencySymbol, CurrencySymbol)

type WithdrawComputationData = (Address, [Fr], Proof)

type WithdrawTokenRedeemer = (WithdrawComputationData, TokenName, TokenName)

{-# INLINABLE withdrawTokenName #-}
withdrawTokenName :: WithdrawTokenRedeemer -> TokenName
withdrawTokenName ((Address (PubKeyCredential (PubKeyHash pkh)) _, subs, proof), TokenName ttName, TokenName dtName) =
    TokenName $ sha2_256 $ pkh
    `appendByteString` toBytes (map fromZp subs)
    `appendByteString` toBytes (toIntegerData proof)
    `appendByteString` ttName `appendByteString` dtName
withdrawTokenName _ = error ()

--------------------------- On-Chain -----------------------------

-- The script must:
-- 0) have the token name that is a result of hashing (public inputs and proof)
-- 1) mint the exact amount of the asset class as expected
-- 2) spend the corresponding mixer script output
-- 3) produce output in the vesting script and send the withdraw token there (check vesting time)
-- 4) produce output at the corresponding user address
-- 5) reference the deposit token with the correct root value
checkPolicy :: WithdrawTokenParams -> WithdrawTokenRedeemer -> ScriptContext -> Bool
checkPolicy (mixer, addr, ttCur, depCur) red@((uAddr, _, _), ttName, dtName) ctx@ScriptContext{scriptContextTxInfo=info} =
    cond1 && cond2 && cond3 && cond4
  where name      = withdrawTokenName red
        vMixer    = mixerDepositValue mixer
        vTime     = token $ AssetClass (ttCur, ttName)
        vDep      = token $ AssetClass (depCur, dtName)
        vWD       = token $ AssetClass (ownCurrencySymbol ctx, name)
        ct        = getUpperTimeEstimate info
        f o       = txOutAddress o == mixerWithdrawVestingAddress mixer && txOutValue o == mixerVestingValue vMixer + vTime + vDep + vWD
        g (t, _)  = t == ct + vestingDuration

        cond1     = tokensMinted ctx (singleton name 1) -- we make restriction of one deposit token minted per mixer per transaction
        cond2     = utxoSpent info (== TxOut addr (vMixer + vTime) Nothing)
        cond3     = checkDatum info (g :: (POSIXTime, PaymentPubKeyHash) -> Bool) $ findUtxoProduced info f
        cond4     = utxoProduced info (\o -> txOutValue o == mixerPureValue mixer && txOutAddress o == uAddr)

curPolicy :: WithdrawTokenParams -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

-------------------------- Off-Chain -----------------------------

withdrawTokenSymbol :: WithdrawTokenParams -> CurrencySymbol
withdrawTokenSymbol par = scriptCurrencySymbol $ curPolicy par

withdrawTokenAssetClass :: WithdrawTokenParams -> WithdrawTokenRedeemer -> AssetClass
withdrawTokenAssetClass par red = AssetClass (withdrawTokenSymbol par, withdrawTokenName red)

withdrawToken :: WithdrawTokenParams -> WithdrawTokenRedeemer -> Value
withdrawToken par red = token $ withdrawTokenAssetClass par red

-- TxConstraints that Relay Token is minted and sent by the transaction
withdrawTokenMintTx :: WithdrawTokenParams -> WithdrawTokenRedeemer -> (ScriptLookups a, TxConstraints i o)
withdrawTokenMintTx par@(mixer, _, _, _) red = (lookups, cons <>
      mustPayToOtherScript (mWithdrawVestingHash mixer) (Datum $ toBuiltinData ()) (withdrawToken par red + lovelaceValueOf 2_000_000))
  where
    (lookups, cons) = fromJust $ txConstructorResult constr
    constr = tokensMintedTx (curPolicy par) (Redeemer $ toBuiltinData red) (withdrawToken par red) $
        TxConstructor Data.Map.empty $ Just (mempty, mempty)
