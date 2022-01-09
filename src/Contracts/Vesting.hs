{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module Contracts.Vesting where

import           Control.Lens             (makeClassyPrisms, review)
import           Control.Monad            (void)
import           Data.Aeson               (FromJSON, ToJSON)
import qualified Data.Map
import           GHC.Generics             (Generic)
import           Ledger                   (Address, POSIXTime, PaymentPubKeyHash (..), ValidatorHash)
import           Ledger.Constraints       (ScriptLookups(..), TxConstraints(..), mustBeSignedBy, mustValidateIn, mustPayToOtherScript,
                                 unspentOutputs, otherScript, typedValidatorLookups)
import           Ledger.Contexts          (ScriptContext (..), TxInfo (..), txSignedBy)
import qualified Ledger.Interval          as Interval
import           Ledger.Scripts           (Datum(..), DatumHash)
import           Ledger.Tx                (ChainIndexTxOut(..))
import           Ledger.Typed.Scripts     
import           Ledger.Value             (Value)
import           Prelude                  (Semigroup (..), Eq, Show)
import           Plutus.Contract
import           PlutusTx
import           PlutusTx.Prelude         hiding ((<>), Eq, Semigroup, fold, mempty)
import           Schema                   (ToSchema)

import           AdminKey                 (adminKeyRequired)


{- |
    A simple vesting scheme. Money is locked by a contract and may only be
    retrieved after some time has passed.

    This is our first example of a contract that covers multiple transactions,
    with a contract state that changes over time.

    In our vesting scheme the money will be released in two _tranches_ (parts):
    A smaller part will be available after an initial number of time has
    passed, and the entire amount will be released at the end. The owner of the
    vesting scheme does not have to take out all the money at once: They can
    take out any amount up to the total that has been released so far. The
    remaining funds stay locked and can be retrieved later.

    Let's start with the data types.

-}

-- | A vesting scheme: vesting tranche and the owner.
data VestingParams = VestingParams
    {
        vestingDate   :: POSIXTime,
        vestingOwner  :: PaymentPubKeyHash,
        vestingHashes :: (ValidatorHash, DatumHash)
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''VestingParams

data Vesting
instance ValidatorTypes Vesting where
    type instance RedeemerType Vesting = ()
    type instance DatumType Vesting = VestingParams

{-# INLINABLE validate #-}
validate :: VestingParams -> () -> ScriptContext -> Bool
validate (VestingParams d o _) () ctx@ScriptContext{scriptContextTxInfo=txInfo@TxInfo{txInfoValidRange}} =
    (isUnlocked && isSignedByOwner) || adminKeyRequired () () ctx
  where
      validRange      = Interval.from d
      isUnlocked      = validRange `Interval.contains` txInfoValidRange
      isSignedByOwner = txSignedBy txInfo (unPaymentPubKeyHash o)

typedValidator :: TypedValidator Vesting
typedValidator = mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = wrapValidator

vestingScript :: Validator
vestingScript = validatorScript typedValidator

vestingScriptHash :: ValidatorHash
vestingScriptHash = validatorHash typedValidator

vestingScriptAddress :: Address
vestingScriptAddress = validatorAddress typedValidator

data VestingError =
    VContractError ContractError
    | InsufficientFundsError Value Value Value
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''VestingError

instance AsContractError VestingError where
    _ContractError = _VContractError

timelockTx :: (AsVestingError e) => VestingParams -> Value -> Contract w s e (ScriptLookups a, TxConstraints i o)
timelockTx p v = mapError (review _VestingError) $ do
    utxos <- utxosAt vestingScriptAddress
    let lookups = otherScript vestingScript <> unspentOutputs utxos
        cons    = mustPayToOtherScript vestingScriptHash (Datum $ toBuiltinData p) v
    return (lookups, cons)

retrieveFunds :: (AsVestingError e) => Contract w s e ()
retrieveFunds = mapError (review _VestingError) $ do
    utxos <- utxosAt vestingScriptAddress
    pkh   <- ownPaymentPubKeyHash
    ct    <- currentTime
    let utxos' = Data.Map.filter (\txout -> f txout ct pkh) utxos
    if Data.Map.null utxos'
        then return ()
        else do
            let lookups = typedValidatorLookups typedValidator <> unspentOutputs utxos'
                cons    = collectFromScript utxos' () <> mustValidateIn (Interval.from ct) <> mustBeSignedBy pkh
            void $ submitTxConstraintsWith lookups cons
  where f o t h = case _ciTxOutDatum o of
          Left  _ -> False
          Right r -> let p = unsafeFromBuiltinData $ getDatum r
                     in  vestingDate p <= t && vestingOwner p == h


type VestingSchema =
        Endpoint "vest funds" (VestingParams, Value)
        .\/ Endpoint "retrieve funds" ()

vestingContract :: Contract () VestingSchema VestingError ()
vestingContract = selectList [vest, retrieve]
  where
    vest = endpoint @"vest funds" $ \(p, v) -> do
        (lookups, cons) <- timelockTx p v
        void $ submitTxConstraintsWith (lookups <> typedValidatorLookups typedValidator) cons
    retrieve = endpoint @"retrieve funds" $ \() -> retrieveFunds
