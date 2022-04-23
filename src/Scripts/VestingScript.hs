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

{-# OPTIONS_GHC -Wno-orphans               #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}


module Scripts.VestingScript where

import           Control.Lens             (makeClassyPrisms, review)
import           Data.Aeson               (FromJSON, ToJSON)
import           GHC.Generics             (Generic)
import           Ledger                   (Address, POSIXTime, PaymentPubKeyHash (..), ValidatorHash, TxOutRef)
import           Ledger.Constraints       (ScriptLookups(..), TxConstraints(..), mustPayToOtherScript,
                                 unspentOutputs, otherScript)
import           Ledger.Contexts          (ScriptContext (..), TxInfo (..), txSignedBy)
import qualified Ledger.Interval          as Interval
import           Ledger.Scripts           (Datum(..))
import           Ledger.Typed.Scripts     
import           Ledger.Value             (Value)
import           Prelude                  (Semigroup (..), Eq, Show)
import           Plutus.Contract
import           PlutusTx
import           PlutusTx.Prelude         hiding ((<>), Eq, Semigroup, fold, mempty)

import           Crypto
import           Tokens.OracleToken       (oracleTokenRequired)


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

data VestingData = VestingData Address (Integer, Integer) [Fr] Proof
    deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''VestingData

PlutusTx.unstableMakeIsData ''Zp
PlutusTx.unstableMakeIsData ''R
PlutusTx.unstableMakeIsData ''Q
PlutusTx.unstableMakeIsData ''Proof

-- | A vesting scheme: vesting tranche and the owner.
data VestingParams = VestingParams
    {
        vestingDate      :: POSIXTime,
        vestingOwner     :: PaymentPubKeyHash,
        vestingTx        :: TxOutRef,
        vestingWHash     :: Fr
    } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''VestingParams

data Vesting
instance ValidatorTypes Vesting where
    type instance RedeemerType Vesting = ()
    type instance DatumType Vesting = VestingParams

{-# INLINABLE validate #-}
validate :: VestingParams -> () -> ScriptContext -> Bool
validate (VestingParams d o _ _) () ctx@ScriptContext{scriptContextTxInfo=txInfo@TxInfo{txInfoValidRange}} =
    (isUnlocked && isSignedByOwner) || oracleTokenRequired ctx
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

---------------------------- For PlutusTx ------------------------------

instance ToData t => ToData (Extension t e) where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData (E (P a)) = toBuiltinData a

instance FromData t => FromData (Extension t e) where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData i = E . P <$> fromBuiltinData i

instance UnsafeFromData t => UnsafeFromData (Extension t e) where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData i = E $ P $ unsafeFromBuiltinData i

instance (ToData t) => ToData (Polynomial t) where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData (P a) = toBuiltinData a

instance (FromData t) => FromData (Polynomial t) where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData i = P <$> fromBuiltinData i

instance (UnsafeFromData t) => UnsafeFromData (Polynomial t) where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData i = P $ unsafeFromBuiltinData i

instance (ToData t, Ring t) => ToData (CurvePoint t) where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData O        = toBuiltinData (False, (zero :: t, zero :: t))
    toBuiltinData (CP x y) = toBuiltinData (True,  (x,    y))

instance FromData t => FromData (CurvePoint t) where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData i = case fromBuiltinData i of
      Just (b, (x, y)) -> if b then Just $ CP x y else Just O
      Nothing          -> Nothing

instance UnsafeFromData t => UnsafeFromData (CurvePoint t) where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData i = if b then CP x y else O
      where (b, (x, y)) = unsafeFromBuiltinData i