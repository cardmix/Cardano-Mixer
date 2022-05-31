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

import qualified Data.Map
import           Data.Maybe                       (fromJust)
import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints.OffChain      (ScriptLookups)
import           Ledger.Constraints.TxConstraints (TxConstraints, mustPayToOtherScript, mustValidateIn)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           Plutus.V1.Ledger.Ada             (toValue)
import           Plutus.V1.Ledger.Value           (geq)
import           PlutusTx                         (compile, toBuiltinData, applyCode, liftCode)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger, mempty)
import           Prelude                          ((<>), mempty)

import           Crypto                           (Zp(..), Fr)
import           Scripts.MixerScript
import           Scripts.Constraints              (tokensMintedTx)
import           Types.TxConstructor              (TxConstructor(..))

------------------------------------ Deposit Token -----------------------------------------------

depositTokenTargetValidatorHash :: ValidatorHash
depositTokenTargetValidatorHash = ValidatorHash $ foldr consByteString emptyByteString
    [13,94,114,183,94,174,208,228,130,10,204,9,253,37,220,190,116,22,117,59,72,35,244,123,251,203,216,36]

depositTokenTargetAddress :: Address
depositTokenTargetAddress = scriptHashAddress depositTokenTargetValidatorHash

{-# INLINABLE depositTokenName #-}
depositTokenName :: (Integer, Integer) -> TokenName
depositTokenName (a, b) = TokenName $ depositTokenHash (a, b)

depositTokenSymbol :: (Address, Value) -> CurrencySymbol
depositTokenSymbol par = scriptCurrencySymbol $ curPolicy par

depositTokenAssetClass :: (Address, Value) -> (Fr, POSIXTime) -> AssetClass
depositTokenAssetClass par (Zp a, POSIXTime b) = AssetClass (depositTokenSymbol par, depositTokenName (a, b))

depositToken :: (Address, Value) -> (Fr, POSIXTime) -> Value
depositToken par d = token $ depositTokenAssetClass par d

--------------------------- On-Chain -----------------------------

integerToBuiltinByteString :: Integer -> BuiltinByteString
integerToBuiltinByteString n = consByteString r $ if q > 0 then integerToBuiltinByteString q else emptyByteString
  where (q, r) = divMod n 256

depositTokenHash :: (Integer, Integer) -> BuiltinByteString
depositTokenHash (a, b) = sha2_256 $ integerToBuiltinByteString a `appendByteString` integerToBuiltinByteString b

-- TODO: fix possible exploits here
checkPolicy :: (Address, Value) -> (Fr, POSIXTime) -> ScriptContext -> Bool
checkPolicy (addr, val) (Zp a, dTime@(POSIXTime b)) ctx@ScriptContext{scriptContextTxInfo=txinfo} = mintOK && sentOK && txOK && timeOK
  where hash      = depositTokenHash (a, b)
        ownSymbol = ownCurrencySymbol ctx
        t         = token $ AssetClass (ownSymbol, TokenName hash)
        minted    = txInfoMint txinfo

        -- True if the pending transaction mints the amount of
        -- currency that we expect
        mintOK = minted == t

        outs   = txInfoOutputs txinfo

        outs'  = filter (\o -> txOutAddress o == depositTokenTargetAddress) outs
        sentOK = sum (map txOutValue outs') `geq` t

        outs'' = filter (\o -> txOutAddress o == addr) outs
        mixer  = makeMixerFromFees val
        txOK = sum (map txOutValue outs'') `geq` (mValue mixer + mTotalFees mixer)

        int    = txInfoValidRange txinfo
        int'   = interval (dTime-600_000) (dTime+600_000)
        timeOK = int' `contains` int

curPolicy :: (Address, Value) -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

-------------------------- Off-Chain -----------------------------

-- TxConstraints that Deposit Token is minted and sent by the transaction
depositTokenMintTx :: (Address, Value) -> (Fr, POSIXTime) -> (ScriptLookups a, TxConstraints i o)
depositTokenMintTx par red@(_, ct) = (lookups, cons <> 
      mustPayToOtherScript depositTokenTargetValidatorHash (Datum $ toBuiltinData (par, red)) (depositToken par red + toValue minAdaTxOut) <>
      mustValidateIn (interval (ct-100_000) (ct+200_000)))
  where
    (lookups, cons) = fromJust $ txConstructorResult constr
    constr = tokensMintedTx (curPolicy par) (Redeemer $ toBuiltinData red) (depositToken par red) $
        TxConstructor Data.Map.empty $ Just (mempty, mempty)