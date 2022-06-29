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

module Tokens.RealDepositToken where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..))
import           PlutusTx                         (compile, applyCode, liftCode, unstableMakeIsData)
import           PlutusTx.AssocMap                (singleton)
import           PlutusTx.Prelude
import           Prelude                          (undefined)

import           Crypto
import           Scripts.Constraints              (tokensMinted)
import           Scripts.VestingScript            ()
import           Types.TxConstructor              (TxConstructor)
import           Utils.ByteString                 (ToBuiltinByteString(..))
import           Utils.Prelude                    (init)

--------------------------- On-Chain -----------------------------

type RealDepositParams = ()

-- current index and co-path
type RealDepositComputationData = (Integer, [Fr])

-- previous position, current leaf, and current position
type RealDepositRedeemer = (RealDepositComputationData, Fr, RealDepositComputationData)

{-# INLINABLE realDepositTokenName #-}
realDepositTokenName :: RealDepositRedeemer -> TokenName
realDepositTokenName ((k, cp), Zp l, (k', cp')) = TokenName $ sha2_256 $ toBytes $ [k, k', l] ++ map fromZp (init cp) ++ map fromZp cp'

-- The script must:
-- 0) have the token name that is a result of hashing (position, root, leaf)
-- 1) mint the exact amount of the asset class as expected
-- 2) check the correctness of the new Merkle path
checkPolicy :: RealDepositParams -> RealDepositRedeemer -> ScriptContext -> Bool
checkPolicy _ red@((k, cp), leaf, (k', cp')) ctx = cond1 && cond2
    where name  = realDepositTokenName red

          cond1 = tokensMinted ctx (singleton name 1) -- we make restriction of one token minted per mixer per transaction
          cond2 = (cp' == addMerkleLeaf leaf k (init cp)) && (k' == k + 1)

curPolicy :: RealDepositParams -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

type DepositTokenRedeemer = (TokenName, Fr, BuiltinByteString)

{-# INLINABLE depositTokenName #-}
depositTokenName :: DepositTokenRedeemer -> TokenName
depositTokenName (TokenName tn, Zp l, bs) = TokenName $ sha2_256 $ tn `appendByteString` toBytes l `appendByteString` bs

-- Type for InvalidDepositToken script
data InvalidDeposit = InvalidDeposit ((TokenName, Fr, RealDepositComputationData), Fr, RealDepositComputationData, BuiltinByteString)
                    | InvalidPreviousDeposit DepositTokenRedeemer

PlutusTx.unstableMakeIsData ''InvalidDeposit

{-# INLINABLE hashRealDepositComputationData #-}
hashRealDepositComputationData :: RealDepositComputationData -> BuiltinByteString
hashRealDepositComputationData (k, cp) = sha2_256 (toBytes $ k : map fromZp cp)

-------------------------- Off-Chain -----------------------------

realDepositCurrencySymbol :: RealDepositParams -> CurrencySymbol
realDepositCurrencySymbol = scriptCurrencySymbol . curPolicy

realDepositAssetClass :: RealDepositParams -> RealDepositRedeemer -> AssetClass
realDepositAssetClass par red = AssetClass (realDepositCurrencySymbol par, realDepositTokenName red)

realDepositToken :: RealDepositParams -> RealDepositRedeemer -> Value
realDepositToken par = token . realDepositAssetClass par

realDepositTokenTx :: RealDepositParams -> RealDepositRedeemer -> TxConstructor a i o -> TxConstructor a i o
realDepositTokenTx _ _ _ = undefined
