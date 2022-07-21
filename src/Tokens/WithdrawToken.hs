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

import           Control.Monad.Extra              (mconcatMapM)
import           Control.Monad.State              (State)
import           Data.Functor                     (($>))
import           Ledger                           hiding (singleton, unspentOutputs, lookup)
import           Ledger.Typed.Scripts             (mkUntypedMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..), geq, Value (..))
import           Plutus.ChainIndex                (ChainIndexTx)
import           Plutus.V1.Ledger.Api             (Credential(..))
import           PlutusTx                         (compile, applyCode, liftCode)
import           PlutusTx.AssocMap                (fromList, Map, keys, lookup, empty)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger, mempty)

import           Crypto
import           Crypto.Conversions               (dataToZp)
import           Scripts.ADAWithdrawScript        (payToADAWithdrawScriptTx)
import           Scripts.Constraints              (tokensMinted, utxoProduced, utxoReferenced, tokensBurnedTx, utxoProducedScriptTx,
                                                    tokensMintedTx, utxoReferencedTx, utxoProducedPublicKeyTx, utxoSpentPublicKeyTx)
import           Scripts.MixerDepositScript       (MixerDepositDatum)
import           Scripts.VestingScript            ()
import           SigmaProtocol                    (sigmaProtocolVerify)
import           Tokens.DepositToken              (depositTokenName)
import           Types.Mixer
import           Types.MixerInput                 (MixerInput, WithdrawTokenNameParams, WithdrawTokenRedeemer, withdrawFirstTokenParams)
import           Types.MixerInstance              (MixerInstance (..))
import           Types.TxConstructor              (TxConstructor (..))
import           Utils.ByteString                 (ToBuiltinByteString(..), byteStringToInteger)

--------------------------- On-Chain -----------------------------

-- Mixer, Deposit token symbol, ADAWithdraw script address, and TxOutRef of initial mint
type WithdrawTokenParams = (Mixer, CurrencySymbol, Address, TxOutRef)

toWithdrawTokenParams :: MixerInstance -> WithdrawTokenParams
toWithdrawTokenParams mi = (mixer, dSymb, aAddr, ref)
    where mixer          = miMixer mi
          dSymb          = miDepositCurrencySymbol mi
          aAddr          = miADAWithdrawAddress mi
          ref            = miWithdrawTxOutRef mi

{-# INLINABLE withdrawTokenName #-}
withdrawTokenName :: WithdrawTokenNameParams -> TokenName
withdrawTokenName (n@(Zp cur), Zp next) = TokenName $ toBytes $ cur * char n  + next

checkPolicy :: WithdrawTokenParams -> WithdrawTokenRedeemer -> ScriptContext -> Bool
checkPolicy (mixer, dSymb, adaWithdrawAddr, _) (sigmaInput@(leafs, cur, _, aVal), sigmaProof, (prev, next, addr), False)
    ctx@ScriptContext{scriptContextTxInfo=info} = cond1 && cond2 && cond3 && cond4 && cond5 && cond6 && cond7
  where
      wSymb    = ownCurrencySymbol ctx
      nameBurn = withdrawTokenName (prev, next)
      namePrev = withdrawTokenName (prev, cur)
      nameCur  = withdrawTokenName (cur, next)
      vPrev    = token $ AssetClass (wSymb, namePrev)
      vCur     = token $ AssetClass (wSymb, nameCur)
      f leaf   = utxoReferenced info (\o -> txOutValue o `geq` token (AssetClass (dSymb, depositTokenName leaf)))

      -- tokens are minted and burned correctly
      cond1 = tokensMinted ctx $ fromList [(nameBurn, -1), (namePrev, 1), (nameCur, 1)]
      -- the newly minted tokens are sent to the correct script address
      cond2 = utxoProduced info (\o -> txOutAddress o == adaWithdrawAddr && txOutValue o `geq` (vPrev + vCur))
      -- the correct value us payed to the correct address
      cond3 = utxoProduced info (\o -> txOutAddress o == addr && txOutValue o `geq` mixerValueAfterWithdraw mixer)
      -- sigma protocol proof is correct
      cond4 = sigmaProtocolVerify sigmaInput sigmaProof
      -- the address is converted to a number correctly
      cond5 = aVal == dataToZp addr
      -- all leafs used in the protocol are presented
      cond6 = all f leafs
      -- the key was not previously used
      cond7 = prev < cur && cur < next
checkPolicy (_, _, adaWithdrawAddr, ref) (_, _, _, True)
    ctx@ScriptContext{scriptContextTxInfo=info} = cond1 && cond2 && cond3
  where
      nameCur  = withdrawTokenName withdrawFirstTokenParams
      vCur     = token $ AssetClass (ownCurrencySymbol ctx, nameCur)

      cond1 = tokensMinted ctx $ fromList [(nameCur, 1)]
      cond2 = utxoProduced info (\o -> txOutAddress o == adaWithdrawAddr && txOutValue o `geq` vCur)
      cond3 = isJust $ findTxInByTxOutRef ref info -- TODO: check that this finds only utxos that are spent

curPolicy :: WithdrawTokenParams -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkUntypedMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

-------------------------- Off-Chain -----------------------------

withdrawTokenSymbol :: WithdrawTokenParams -> CurrencySymbol
withdrawTokenSymbol par = scriptCurrencySymbol $ curPolicy par

withdrawTokenAssetClass :: WithdrawTokenParams -> WithdrawTokenNameParams -> AssetClass
withdrawTokenAssetClass par nParams = AssetClass (withdrawTokenSymbol par, withdrawTokenName nParams)

withdrawToken :: WithdrawTokenParams -> WithdrawTokenNameParams -> Value
withdrawToken par nParams = token $ withdrawTokenAssetClass par nParams

-- Constraints that Withdraw Token is minted in the transaction
withdrawTokenMintTx :: MixerInstance -> WithdrawTokenRedeemer -> MixerDepositDatum -> State (TxConstructor [MixerInput] a i o) ()
withdrawTokenMintTx mi red@((leafs, cur, _, _), _, (prev, next, addr), b) leaf =
    if b
    then do
        tokensMintedTx (curPolicy par) red (withdrawToken par withdrawFirstTokenParams)
        payToADAWithdrawScriptTx (withdrawToken par withdrawFirstTokenParams)
        utxoSpentPublicKeyTx (\r _ -> r == wRef) $> ()
    else do
        tokensMintedTx (curPolicy par) red (withdrawToken par (prev, cur) + withdrawToken par (cur, next))
        tokensBurnedTx (curPolicy par) red (withdrawToken par (prev, next))
        payToADAWithdrawScriptTx (withdrawToken par (prev, cur) + withdrawToken par (cur, next))
        utxoProducedWithdraw
        mconcatMapM f leafs
    where
        mixer = miMixer mi
        dSymb = miDepositCurrencySymbol mi
        aAddr = miADAWithdrawAddress mi
        wRef  = miWithdrawTxOutRef mi
        par   = (mixer, dSymb, aAddr, wRef)

        -- Different condition depending on the type of withdrawal address
        utxoProducedWithdraw = case addr of
            Address (PubKeyCredential pkh) _ ->
                utxoProducedPublicKeyTx (PaymentPubKeyHash pkh) Nothing (mixerValueAfterWithdraw mixer) ()
            Address (ScriptCredential valHash) _ ->
                utxoProducedScriptTx valHash Nothing (mixerValueAfterWithdraw mixer) leaf

        g l _ o = _ciTxOutValue o `geq` token (AssetClass (dSymb, depositTokenName l))
        f l     = utxoReferencedTx (g l)

withdrawTokenFirstMintTx :: MixerInstance -> State (TxConstructor [MixerInput] a i o) ()
withdrawTokenFirstMintTx mi = withdrawTokenMintTx mi wRed (toZp 0)
    where
        wRed = (([], toZp 0, toZp 0, toZp 0), (([], [], []), [], []),
            (fst withdrawFirstTokenParams, snd withdrawFirstTokenParams, miMixerAddress mi), True)

withdrawKeys :: MixerInstance -> Map TxOutRef (ChainIndexTxOut, ChainIndexTx) -> [(Fr, Fr)]
withdrawKeys mi = concatMap f
    where symb = withdrawTokenSymbol $ toWithdrawTokenParams mi
          g x  = (Zp $ divide x (char (zero :: Fr)), Zp $ modulo x (char (zero :: Fr)))
          f    = map (g . byteStringToInteger . unTokenName) . keys . fromMaybe empty . lookup symb . getValue . _ciTxOutValue . fst
