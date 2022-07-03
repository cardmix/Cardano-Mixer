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
import           Ledger.Value                     (AssetClass(..), TokenName (..), geq)
import           PlutusTx                         (compile, applyCode, liftCode)
import           PlutusTx.AssocMap                (singleton)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger, mempty)

import           Crypto
import           Mixer
import           Scripts.Constraints              (tokensMinted, utxoProduced, findUtxoReferenced, tokensMintedTx, utxoProducedScriptTx, utxoReferencedTx)
import           Scripts.VestingScript            ()
import           Types.TxConstructor              (TxConstructor(..))
-- import           Utils.ByteString                 (ToBuiltinByteString(..))

--------------------------------------- On-Chain ------------------------------------------

type DepositTokenParams = (Mixer, (CurrencySymbol, TokenName))

type DepositTokenRedeemer = Fr

-- TODO: check if we need to use modulo here
{-# INLINABLE depositTokenName #-}
depositTokenName :: DepositTokenRedeemer -> TokenName
depositTokenName (Zp l) = TokenName $ toBytes l
    where
        -- using ToBuiltinByteString(..) causes compilation errors
        toBytes n = consByteString r $ if q > 0 then toBytes q else emptyByteString
            where (q, r) = divMod n 256

checkPolicy :: DepositTokenParams -> DepositTokenRedeemer -> ScriptContext -> Bool
checkPolicy (mixer, (beaconSymb, beaconName)) red ctx@ScriptContext{scriptContextTxInfo=info} = cond1 && cond2
    where
        name       = depositTokenName red
        vBeacon    = token $ AssetClass (beaconSymb, beaconName)
        beaconUtxo = findUtxoReferenced info (\o -> txOutValue o `geq` vBeacon)
        mixerAddr  = txOutAddress $ fromMaybe (error ()) beaconUtxo

        cond1      = tokensMinted ctx (singleton name 1)
        cond2      = utxoProduced info (\o -> txOutValue o `geq` mixerValueAfterDeposit mixer &&
            txOutAddress o == mixerAddr && isNothing (txOutDatumHash o))

curPolicy :: DepositTokenParams -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

------------------------------------------ Off-Chain --------------------------------------

depositTokenSymbol :: DepositTokenParams -> CurrencySymbol
depositTokenSymbol par = scriptCurrencySymbol $ curPolicy par

depositTokenAssetClass :: DepositTokenParams -> DepositTokenRedeemer -> AssetClass
depositTokenAssetClass par red = AssetClass (depositTokenSymbol par, depositTokenName red)

depositToken :: DepositTokenParams -> DepositTokenRedeemer -> Value
depositToken par d = token $ depositTokenAssetClass par d

-- Constraints that Deposit Token is minted in the transaction
depositTokenMintTx :: MixerInstance -> DepositTokenRedeemer -> TxConstructor a i o -> TxConstructor a i o
depositTokenMintTx mi red =
        tokensMintedTx (curPolicy par) red (depositToken par red) .
        utxoProducedScriptTx mixerVH Nothing (mixerValueAfterDeposit mixer) () .
        utxoReferencedTx False (\_ o -> _ciTxOutValue o `geq` beaconToken)
    where
        mixer       = miMixer mi
        beaconSymb  = miMixerBeaconCurrencySymbol mi
        beaconName  = miMixerBeaconTokenName mi
        par         = (mixer, (beaconSymb, beaconName))
        beaconToken = token (AssetClass $ snd par)
        mixerVH     = miMixerValidatorHash mi