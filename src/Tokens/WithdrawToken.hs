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
import           Ledger.Constraints.TxConstraints (TxConstraints, mustPayToOtherScript, mustValidateIn)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           Plutus.V1.Ledger.Ada             (lovelaceValueOf)
import           Plutus.V1.Ledger.Value           (geq)
import           PlutusTx                         (compile, toBuiltinData, applyCode, liftCode)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger, mempty)
import           Prelude                          ((<>), mempty)

import           Configuration.PABConfig          (vestingScriptPermanentHash)
import           Crypto
import           Scripts.VestingScript            ()
import           Scripts.Constraints              (tokensMintedTx)
import           Types.TxConstructor              (TxConstructor(..))

------------------------------------ Deposit Token -----------------------------------------------

withdrawTokenTargetValidatorHash :: ValidatorHash
withdrawTokenTargetValidatorHash = vestingScriptPermanentHash

withdrawTokenTargetAddress :: Address
withdrawTokenTargetAddress = scriptHashAddress withdrawTokenTargetValidatorHash

{-# INLINABLE withdrawTokenName #-}
withdrawTokenName :: (Integer, Integer) -> TokenName
withdrawTokenName (a, b) = TokenName $ withdrawTokenHash (a, b)

withdrawTokenSymbol :: (Address, Value) -> CurrencySymbol
withdrawTokenSymbol par = scriptCurrencySymbol $ curPolicy par

withdrawTokenAssetClass :: (Address, Value) -> (Fr, POSIXTime) -> AssetClass
withdrawTokenAssetClass par (Zp a, POSIXTime b) = AssetClass (withdrawTokenSymbol par, withdrawTokenName (a, b))

withdrawToken :: (Address, Value) -> (Fr, POSIXTime) -> Value
withdrawToken par d = token $ withdrawTokenAssetClass par d

--------------------------- On-Chain -----------------------------

integerToBuiltinByteString :: Integer -> BuiltinByteString
integerToBuiltinByteString n = consByteString r $ if q > 0 then integerToBuiltinByteString q else emptyByteString
  where (q, r) = divMod n 256

withdrawTokenHash :: (Integer, Integer) -> BuiltinByteString
withdrawTokenHash (a, b) = sha2_256 $ integerToBuiltinByteString a `appendByteString` integerToBuiltinByteString b

checkPolicy :: (Address, Value) -> (Fr, POSIXTime) -> ScriptContext -> Bool
checkPolicy (addr, val) (Zp a, dTime@(POSIXTime b)) ctx@ScriptContext{scriptContextTxInfo=txinfo} = mintOK && sentOK && txOK && timeOK
  where hash      = withdrawTokenHash (a, b)
        ownSymbol = ownCurrencySymbol ctx
        t         = token $ AssetClass (ownSymbol, TokenName hash)
        minted    = txInfoMint txinfo

        -- True if the pending transaction mints the amount of
        -- currency that we expect
        mintOK = minted == t

        outs   = txInfoOutputs txinfo

        outs'  = filter (\o -> txOutAddress o == withdrawTokenTargetAddress) outs
        sentOK = sum (map txOutValue outs') `geq` t

        outs'' = filter (\o -> txOutAddress o == addr) outs
        txOK = sum (map txOutValue outs'') `geq` val

        int    = txInfoValidRange txinfo
        int'   = interval (dTime-600_000) (dTime+600_000)
        timeOK = int' `contains` int

curPolicy :: (Address, Value) -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

-------------------------- Off-Chain -----------------------------

-- TxConstraints that Relay Token is minted and sent by the transaction
withdrawTokenMintTx :: (Address, Value) -> (Fr, POSIXTime) -> (ScriptLookups a, TxConstraints i o)
withdrawTokenMintTx par red@(_, ct) = (lookups, cons <> 
      mustPayToOtherScript withdrawTokenTargetValidatorHash (Datum $ toBuiltinData (par, red)) (withdrawToken par red + lovelaceValueOf 2_000_000) <>
      mustValidateIn (interval (ct-100_000) (ct+200_000)))
  where
    (lookups, cons) = fromJust $ txConstructorResult constr
    constr = tokensMintedTx (curPolicy par) (Redeemer $ toBuiltinData red) (withdrawToken par red) $
        TxConstructor Data.Map.empty $ Just (mempty, mempty)