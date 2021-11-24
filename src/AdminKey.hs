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
import           Ledger.Value                     (AssetClass(..), geq, TokenName (TokenName), CurrencySymbol (CurrencySymbol))
import           Plutus.ChainIndex.Tx             (txOutsWithRef)
import           Plutus.Contract                  (Contract, logInfo)
import           Plutus.Contract.Request          (ownPubKey, utxosAt)
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Currency        (OneShotCurrency(..), currencySymbol)
import           PlutusTx.AssocMap                (fromList)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)
import           Prelude                          (String, show)
import           Wallet.Types                     (AsContractError)


------------------------------------ Admin Key ---------------------------------------------------

adminKeyTxId :: TxId
adminKeyTxId = TxId $ foldr consByteString emptyByteString [0xb1, 0x35, 0x80, 0x61, 0x42, 0xf3, 0xd0, 0x20, 0xfc, 0x68, 0xe9, 0xe7, 0x2f, 0xa6,
    0xe8, 0xe7, 0xc8, 0xfc, 0xe3, 0x4b, 0x2a, 0x0f, 0x17, 0xfd, 0x23, 0xb6, 0x33, 0x64, 0x1f, 0x58, 0x94, 0x4a]
    --buildByteString "b135806142f3d020fc68e9e72fa6e8e7c8fce34b2a0f17fd23b633641f58944a"

adminKeyTxNum :: Integer
adminKeyTxNum = 6

adminKeyTokenName :: TokenName
adminKeyTokenName = TokenName "Cardano Mixer Admin Key"

adminKeyCurrency :: OneShotCurrency
adminKeyCurrency = OneShotCurrency (adminKeyTxId, adminKeyTxNum) (fromList [(adminKeyTokenName, 1)])

{-# INLINABLE adminKeySymbol #-}
adminKeySymbol :: CurrencySymbol
-- adminKeySymbol = currencySymbol adminKeyCurrency
adminKeySymbol = CurrencySymbol $ foldr consByteString emptyByteString [0x93, 0x95, 0xf4, 0x8b, 0x7f, 0x17, 0xe7, 0x35, 0x90, 0x63, 0x34, 0xbb,
    0xca, 0x5f, 0x67, 0x69, 0x63, 0x24, 0x6d, 0x36, 0xc6, 0xc5, 0xc8, 0x57, 0x21, 0x1c, 0xb6, 0x0f]

adminKeyAssetClass :: AssetClass
adminKeyAssetClass = AssetClass (adminKeySymbol, adminKeyTokenName)

adminKey :: Value
adminKey = token adminKeyAssetClass

-------------------------- Admin Key is required to run the Script -----------------------------

adminKeyRequired :: state -> input -> ScriptContext -> Bool
adminKeyRequired _ _ ctx = val `geq` adminKey
    where info = scriptContextTxInfo ctx
          outs = txInfoOutputs info
          val  = sum (map txOutValue outs)

------------------- Admin Key transaction Lookups and Constraints ------------------------------

-- TxConstraints that Admin Key is spent by transaction
adminKeyTx :: (AsContractError e) => Contract w s e (ScriptLookups a, TxConstraints i o)
adminKeyTx = do
    pk <- ownPubKey
    let pkh = pubKeyHash pk
    utxos <- utxosAt (pubKeyHashAddress pkh)
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