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
{-# LANGUAGE TypeSynonymInstances       #-}



module AdminKey (
    adminKey,
    adminKeySymbol,
    adminKeyTokenName,
    adminKeyRequired,
    adminKeyTx,
    mkStateMachineClientAdmin
) where

import           Data.List                        (unzip)
import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints               (mustPayToPubKey)
import           Ledger.Constraints.OffChain      (ScriptLookups, unspentOutputs)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..), CurrencySymbol (..), geq)
import           Plutus.ChainIndex.Tx             (txOutsWithRef)
import           Plutus.Contract                  (Contract, logInfo)
import           Plutus.Contract.Request          (ownPaymentPubKeyHash, utxosAt)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)
import           Prelude                          (String, show)
import           Wallet.Types                     (AsContractError)

import           Config                           (adminKeyPolicyId)
import           Contracts.StateMachine


------------------------------------ Admin Key ---------------------------------------------------

{-# INLINABLE adminKeyTokenName #-}
adminKeyTokenName :: TokenName
adminKeyTokenName = TokenName "Cardano Mixer Admin Key"

{-# INLINABLE adminKeySymbol #-}
adminKeySymbol :: CurrencySymbol
adminKeySymbol = CurrencySymbol $ foldr consByteString emptyByteString adminKeyPolicyId

{-# INLINABLE adminKeyAssetClass #-}
adminKeyAssetClass :: AssetClass
adminKeyAssetClass = AssetClass (adminKeySymbol, adminKeyTokenName)

{-# INLINABLE adminKey #-}
adminKey :: Value
adminKey = token adminKeyAssetClass

-------------------------- Admin Key is required to run the Script -----------------------------

{-# INLINABLE adminKeyRequired #-}
adminKeyRequired :: state -> input -> ScriptContext -> Bool
adminKeyRequired _ _ ctx = val `geq` adminKey
    where info = scriptContextTxInfo ctx
          outs = txInfoOutputs info
          val  = sum (map txOutValue outs)

------------------- Admin Key transaction Lookups and Constraints ------------------------------

-- TxConstraints that Admin Key is spent by transaction
adminKeyTx :: (AsContractError e) => Contract w s e (ScriptLookups a, TxConstraints i o)
adminKeyTx = do
    pkh <- ownPaymentPubKeyHash
    utxos <- utxosAt (pubKeyHashAddress pkh Nothing)
    logInfo @String (show adminKey)
    return (unspentOutputs utxos, mustPayToPubKey pkh adminKey)

----------------------------------- State Machine support ------------------------------------

-- Admin Key in utxos
adminChooser :: forall state input . [OnChainState state input] -> Either SMContractError (OnChainState state input)
adminChooser states =
        case filter hasAdminKey states of
            [x] -> Right x
            _   -> Left (ChooserError "State not found")
    where hasAdminKey s = sum (map txOutValue $ fst $ unzip $ txOutsWithRef $ ocsTx s) `geq` adminKey

-- State Machine that can only be created and modified by Admin
mkStateMachineClientAdmin :: forall state input . StateMachineInstance state input -> StateMachineClient state input
mkStateMachineClientAdmin inst = StateMachineClient inst adminChooser