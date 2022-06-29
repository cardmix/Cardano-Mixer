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

module Tokens.InvalidDepositToken where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), geq)
import           PlutusTx                         (compile, applyCode, liftCode)
import           PlutusTx.AssocMap                (singleton)
import           PlutusTx.Prelude
import           Prelude                          (undefined)

import           Scripts.Constraints              (tokensMinted, utxoReferenced)
import           Scripts.VestingScript            ()
import           Tokens.RealDepositToken          (InvalidDeposit(..), realDepositTokenName, hashRealDepositComputationData, depositTokenName)
import           Types.TxConstructor              (TxConstructor)

--------------------------- On-Chain -----------------------------

type InvalidDepositParams = CurrencySymbol

type InvalidDepositRedeemer = InvalidDeposit

{-# INLINABLE invalidDepositTokenName #-}
invalidDepositTokenName :: InvalidDepositRedeemer -> TokenName
invalidDepositTokenName (InvalidDeposit ((tn0, leaf0, cd0), leaf, _, bs)) =
    let tnPrev = depositTokenName (tn0, leaf0, hashRealDepositComputationData cd0) -- previous DepositToken TokenName
    in depositTokenName (tnPrev, leaf, bs)
invalidDepositTokenName (InvalidPreviousDeposit red) = depositTokenName red

checkPolicy :: InvalidDepositParams -> InvalidDepositRedeemer -> ScriptContext -> Bool
-- The script must:
-- 1) mint the exact amount of the asset class as expected
-- 2) reference the real deposit token with the corresponding TokenName
-- 3) check that the hash of the RealDepositComputationData is not correct
checkPolicy curSymb red@(InvalidDeposit ((_, _, realDCD0), leaf, realDCD, bs)) ctx@ScriptContext{scriptContextTxInfo=info} =
        cond1 && cond2 && cond3
    where tnRealDT = realDepositTokenName (realDCD0, leaf, realDCD)
          vRealDT  = token $ AssetClass (curSymb, tnRealDT)
          name     = invalidDepositTokenName red

          cond1 = tokensMinted ctx (singleton name 1) -- we make restriction of one token minted per mixer per transaction
          cond2 = utxoReferenced info (\o -> txOutValue o `geq` vRealDT)
          cond3 = bs /= hashRealDepositComputationData realDCD
-- The script must:
-- 1) mint the exact amount of the asset class as expected
-- 2) reference the invalid deposit token constructed for the previous deposit token
checkPolicy _ (InvalidPreviousDeposit red@(name0, _, _)) ctx@ScriptContext{scriptContextTxInfo=info} = cond1 && cond2
    where vPrevIDT = token $ AssetClass (ownCurrencySymbol ctx, name0)
          name     = depositTokenName red

          cond1 = tokensMinted ctx (singleton name 1) -- we make restriction of one token minted per mixer per transaction
          cond2 = utxoReferenced info (\o -> txOutValue o `geq` vPrevIDT)

curPolicy :: InvalidDepositParams -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

-------------------------- Off-Chain -----------------------------

invalidDepositCurrencySymbol :: InvalidDepositParams -> CurrencySymbol
invalidDepositCurrencySymbol = scriptCurrencySymbol . curPolicy

invalidDepositAssetClass :: InvalidDepositParams -> InvalidDepositRedeemer -> AssetClass
invalidDepositAssetClass par red = AssetClass (invalidDepositCurrencySymbol par, invalidDepositTokenName red)

invalidDepositToken :: InvalidDepositParams -> InvalidDepositRedeemer -> Value
invalidDepositToken par = token . invalidDepositAssetClass par

invalidDepositTokenTx :: InvalidDepositParams -> InvalidDepositRedeemer -> TxConstructor a i o -> TxConstructor a i o
invalidDepositTokenTx _ _ _ = undefined
