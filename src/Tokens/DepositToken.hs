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

import           Control.Monad.State              (State)
import           Data.Map                         (Map)
import           Data.Tuple.Extra                 (snd3)
import           Ledger                           hiding (singleton, unspentOutputs, lookup)
import           Ledger.Typed.Scripts             (mkUntypedMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..), geq, Value (..))
import           Plutus.ChainIndex                (ChainIndexTx)
import           PlutusTx                         (compile, applyCode, liftCode)
import           PlutusTx.AssocMap                (singleton, lookup, keys, empty)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger, mempty, concatMap)
import           Prelude                          (concatMap)

import           Crypto
import           MixerProofs.SigmaProtocol        (BaseField)
import           Scripts.ADAWithdrawScript        (payToADAWithdrawScriptTx)
import           Scripts.Constraints              (tokensMinted, utxoProduced, findUtxoReferenced, tokensMintedTx, utxoProducedScriptTx, utxoReferencedTx)
import           Scripts.VestingScript            ()
import           Types.Mixer
import           Types.MixerInstance              (MixerInstance(..))
import           Types.TxConstructor              (TxConstructor(..))
import           Utils.ByteString                 (byteStringToInteger)

-- import           Utils.ByteString                 (ToBuiltinByteString(..))

--------------------------------------- On-Chain ------------------------------------------

type DepositTokenParams = (Mixer, (CurrencySymbol, TokenName), Address)

toDepositTokenParams :: MixerInstance -> DepositTokenParams
toDepositTokenParams mi = (mixer, (beaconSymb, beaconName), aAddr)
    where mixer         = miMixer mi
          aAddr         = miADAWithdrawAddress mi
          beaconSymb    = miMixerBeaconCurrencySymbol mi
          beaconName    = miMixerBeaconTokenName mi

type DepositTokenRedeemer = BaseField

-- TODO: check if we need to use modulo here
{-# INLINABLE depositTokenName #-}
depositTokenName :: DepositTokenRedeemer -> TokenName
depositTokenName (Zp l) = TokenName $ toBytes l
    where
        -- using ToBuiltinByteString(..) causes compilation errors
        toBytes n = consByteString r $ if q > 0 then toBytes q else emptyByteString
            where (q, r) = divMod n 256

checkPolicy :: DepositTokenParams -> DepositTokenRedeemer -> ScriptContext -> Bool
checkPolicy (mixer, (beaconSymb, beaconName), adaWithdrawAddr) red ctx@ScriptContext{scriptContextTxInfo=info} = cond1 && cond2 && cond3
    where
        name       = depositTokenName red
        vBeacon    = token $ AssetClass (beaconSymb, beaconName)
        beaconUtxo = findUtxoReferenced info (\o -> txOutValue o `geq` vBeacon)
        mixerAddr  = txOutAddress $ fromMaybe (error ()) beaconUtxo
        vDep       = token $ AssetClass (ownCurrencySymbol ctx, name)

        cond1      = tokensMinted ctx (singleton name 1)
        cond2      = utxoProduced info (\o -> txOutValue o `geq` mixerValueAfterDeposit mixer &&
            txOutAddress o == mixerAddr && isNothing (txOutDatumHash o))
        cond3      = utxoProduced info (\o -> txOutAddress o == adaWithdrawAddr && txOutValue o `geq` vDep)

curPolicy :: DepositTokenParams -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkUntypedMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

------------------------------------------ Off-Chain --------------------------------------

depositTokenSymbol :: DepositTokenParams -> CurrencySymbol
depositTokenSymbol par = scriptCurrencySymbol $ curPolicy par

depositTokenAssetClass :: DepositTokenParams -> DepositTokenRedeemer -> AssetClass
depositTokenAssetClass par red = AssetClass (depositTokenSymbol par, depositTokenName red)

depositToken :: DepositTokenParams -> DepositTokenRedeemer -> Value
depositToken par d = token $ depositTokenAssetClass par d

depositTokenMintTx :: MixerInstance -> DepositTokenRedeemer -> State (TxConstructor d a i o) ()
depositTokenMintTx mi red = do
        tokensMintedTx (curPolicy par) red (depositToken par red)
        utxoProducedScriptTx vh Nothing (mixerValueAfterDeposit mixer) ()
        utxoReferencedTx (\_ o -> _ciTxOutValue o `geq` beaconToken && _ciTxOutAddress o == mixerAddr)
        payToADAWithdrawScriptTx (depositToken par red)
    where
        mixer       = miMixer mi
        vh          = miMixerValidatorHash mi
        mixerAddr   = miMixerAddress mi
        par         = toDepositTokenParams mi
        beaconToken = token (AssetClass $ snd3 par)

depositKeys :: MixerInstance -> Map TxOutRef (ChainIndexTxOut, ChainIndexTx) -> [BaseField]
depositKeys mi = concatMap f
    where symb = depositTokenSymbol $ toDepositTokenParams mi
          f    = map (Zp . byteStringToInteger . unTokenName) . keys . fromMaybe empty . lookup symb . getValue . _ciTxOutValue . fst

