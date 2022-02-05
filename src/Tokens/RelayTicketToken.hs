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


module Tokens.RelayTicketToken where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints.OffChain      (ScriptLookups)
import           Ledger.Constraints.TxConstraints (TxConstraints, mustPayToOtherScript)
import           Ledger.Typed.Scripts             (wrapMintingPolicy, ValidatorTypes(..))
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..), CurrencySymbol (..))
import           Plutus.Contract                  (Contract)
import           Plutus.V1.Ledger.Ada             (lovelaceValueOf)
import           Plutus.V1.Ledger.Value           (geq)
import           PlutusTx                         (compile, toBuiltinData)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)
import           Prelude                          ((<>))
import           Wallet.Types                     (AsContractError)


import           Configuration.PABConfig          (vestingScriptPermanentHash)
import           Tokens.Common


------------------------------------ Relay Ticket Token -----------------------------------------------

mixerRewardsAddress :: Address
mixerRewardsAddress = scriptHashAddress vestingScriptPermanentHash

mixerFee :: Value
mixerFee = lovelaceValueOf 200_000

{-# INLINABLE relayTicketTokenName #-}
relayTicketTokenName :: TokenName
relayTicketTokenName = TokenName emptyByteString

relayTicketTokenSymbol :: CurrencySymbol
relayTicketTokenSymbol = scriptCurrencySymbol curPolicy

{-# INLINABLE relayTicketTokenSymbolPermanent #-}
relayTicketTokenSymbolPermanent :: CurrencySymbol
relayTicketTokenSymbolPermanent = CurrencySymbol $ foldr consByteString emptyByteString
    [142,94,114,183,94,174,208,228,130,76,204,9,253,37,220,190,116,22,117,59,72,35,244,123,251,203,216,36]

{-# INLINABLE relayTicketTokenAssetClass #-}
relayTicketTokenAssetClass :: AssetClass
relayTicketTokenAssetClass = AssetClass (relayTicketTokenSymbolPermanent, relayTicketTokenName)

{-# INLINABLE relayTicketToken #-}
relayTicketToken :: Value
relayTicketToken = token relayTicketTokenAssetClass

type RelayTicketDatum = ()

type RelayTicketRedeemer = Integer

data RelayTicketing
instance ValidatorTypes RelayTicketing where
  type instance DatumType RelayTicketing = RelayTicketDatum
  type instance RedeemerType RelayTicketing = RelayTicketRedeemer

--------------------------- On-Chain -----------------------------

checkPolicy :: Integer -> ScriptContext -> Bool
checkPolicy n ctx@ScriptContext{scriptContextTxInfo=txinfo}
        | n > 0 = let
                    -- True if the pending transaction mints the amount of
                    -- currency that we expect
                    mintOK = minted == scale n t

                    outs  = filter (\o -> txOutAddress o == mixerRewardsAddress) $ txInfoOutputs txinfo
                    feeOK = sum (map txOutValue outs) `geq` scale n mixerFee
                  in mintOK && feeOK
    | otherwise = minted == scale (-1) t
  where ownSymbol = ownCurrencySymbol ctx
        t         = token $ AssetClass (ownSymbol, relayTicketTokenName)
        minted    = txInfoMint txinfo

curPolicy :: MintingPolicy
curPolicy = mkMintingPolicyScript $$(compile [|| wrapMintingPolicy checkPolicy ||])

{-# INLINABLE relayTicketTokenRequired #-}
relayTicketTokenRequired :: ScriptContext -> Bool
relayTicketTokenRequired = tokensBurned relayTicketToken

-------------------------- Off-Chain -----------------------------

-- TxConstraints that Relay Token is consumed by transaction
relayTicketTokenMintTx :: (AsContractError e) => Integer -> Contract w s e (ScriptLookups a, TxConstraints i o)
relayTicketTokenMintTx n = do
    (lookups, cons) <- tokensMintTx curPolicy (Redeemer $ toBuiltinData (n :: Integer)) relayTicketTokenName n
    return (lookups, cons <> mustPayToOtherScript vestingScriptPermanentHash (Datum $ toBuiltinData ()) (scale n mixerFee))

-- TxConstraints that Relay Token is consumed by transaction
relayTicketTokenBurnTx :: (AsContractError e) => Contract w s e (ScriptLookups a, TxConstraints i o)
relayTicketTokenBurnTx = tokensMintTx curPolicy (Redeemer $ toBuiltinData (0 :: Integer)) relayTicketTokenName (-1)
