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

module Tokens.InvalidWithdrawToken where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints               (TxConstraints)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), geq)
import           PlutusTx                         (compile, applyCode, liftCode)
import           PlutusTx.AssocMap                (singleton)
import           PlutusTx.Prelude                 
import           Prelude                          (undefined)

import           Crypto
import           Crypto.Conversions               (dataToZp)
import           Scripts.Constraints              (tokensMinted, utxoReferenced)
import           Tokens.RealDepositToken          (depositTokenName, hashRealDepositComputationData)
import           Tokens.RealWithdrawToken         (InvalidWithdraw (..), realWithdrawTokenName, withdrawTokenName)
import           Utils.Prelude                    (last)

--------------------------- On-Chain -----------------------------

-- Real Withdraw Token
type InvalidWithdrawParams = CurrencySymbol

type InvalidWithdrawRedeemer = InvalidWithdraw

{-# INLINABLE invalidWithdrawTokenName #-}
invalidWithdrawTokenName :: InvalidWithdrawRedeemer -> TokenName
invalidWithdrawTokenName (InvalidWithdrawProof red) = withdrawTokenName red
invalidWithdrawTokenName (InvalidWithdrawRoot (tn, leaf, (k, cp), red)) = 
    withdrawTokenName (red, depositTokenName (tn, leaf, hashRealDepositComputationData (k, cp)))
invalidWithdrawTokenName (InvalidWithdrawAddress red) = withdrawTokenName red

checkPolicy :: InvalidWithdrawParams -> InvalidWithdrawRedeemer -> ScriptContext -> Bool
checkPolicy curSymb red@(InvalidWithdrawProof ((_, subs, proof), _)) ctx@ScriptContext{scriptContextTxInfo=info} =
        cond1 && cond2
    where tnRealWT = realWithdrawTokenName ((subs, proof), False)
          vRealWT  = token $ AssetClass (curSymb, tnRealWT)
          name     = invalidWithdrawTokenName red

          cond1 = tokensMinted ctx (singleton name 1) -- we make restriction of one token minted per mixer per transaction
          cond2 = utxoReferenced info (\o -> txOutValue o `geq` vRealWT)
checkPolicy curSymb red@(InvalidWithdrawRoot (_, _, (k, cp), (_, subs, proof))) ctx@ScriptContext{scriptContextTxInfo=info} =
        cond1 && cond2 && cond3
    where tnRealWT = realWithdrawTokenName ((subs, proof), True)
          vRealWT  = token $ AssetClass (curSymb, tnRealWT)
          name     = invalidWithdrawTokenName red

          cond1 = tokensMinted ctx (singleton name 1) -- we make restriction of one token minted per mixer per transaction
          cond2 = utxoReferenced info (\o -> txOutValue o `geq` vRealWT)
          cond3 = (k /= fromZp (subs !! 4)) || (last cp /= head subs)
checkPolicy curSymb red@(InvalidWithdrawAddress ((addr, subs, proof), _)) ctx@ScriptContext{scriptContextTxInfo=info} =
    cond1 && cond2 && cond3
    where tnRealWT = realWithdrawTokenName ((subs, proof), True)
          vRealWT  = token $ AssetClass (curSymb, tnRealWT)
          name     = invalidWithdrawTokenName red

          cond1 = tokensMinted ctx (singleton name 1) -- we make restriction of one token minted per mixer per transaction
          cond2 = utxoReferenced info (\o -> txOutValue o `geq` vRealWT)
          cond3 = dataToZp addr /= (subs !! 1)

curPolicy :: InvalidWithdrawParams -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

-------------------------- Off-Chain -----------------------------

invalidWithdrawCurrencySymbol :: InvalidWithdrawParams -> CurrencySymbol
invalidWithdrawCurrencySymbol = scriptCurrencySymbol . curPolicy

invalidWithdrawAssetClass :: InvalidWithdrawParams -> InvalidWithdrawRedeemer -> AssetClass
invalidWithdrawAssetClass par red = AssetClass (invalidWithdrawCurrencySymbol par, invalidWithdrawTokenName red)

invalidWithdrawToken :: InvalidWithdrawParams -> InvalidWithdrawRedeemer -> Value
invalidWithdrawToken par = token . invalidWithdrawAssetClass par

invalidWithdrawTokenTx :: InvalidWithdrawParams -> InvalidWithdrawRedeemer -> TxConstraints i o
invalidWithdrawTokenTx _ _ = undefined
