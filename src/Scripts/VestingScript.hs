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

import           Control.Monad.State            (State, gets)
import           Ledger                         (Address, POSIXTime, PaymentPubKeyHash (..), ValidatorHash, Datum (..), ChainIndexTxOut (..), TxOutRef, ScriptContext (..), TxInfo (..), txSignedBy)
import qualified Ledger.Interval                as Interval
import           Ledger.Typed.Scripts
import           Ledger.Value                   (Value)
import           PlutusTx
import           PlutusTx.Prelude               hiding ((<>), Eq, Semigroup, fold, mempty)

import           Crypto
import           MixerProofs.SigmaProtocol      (FBase, FExp)
import           Scripts.Constraints            (utxoProducedScriptTx, utxoSpentScriptTx)
import           Types.TxConstructor            (TxConstructor (..))

--------------------------------------- On-Chain ------------------------------------------

type VestingDatum = (POSIXTime, PaymentPubKeyHash)

type VestingRedeemer = ()

data Vesting
instance ValidatorTypes Vesting where
    type instance RedeemerType Vesting = VestingRedeemer
    type instance DatumType Vesting = VestingDatum

{-# INLINABLE mkVestingValidator #-}
mkVestingValidator :: VestingDatum -> VestingRedeemer -> ScriptContext -> Bool
mkVestingValidator (d, o) _ ScriptContext{scriptContextTxInfo=info@TxInfo{txInfoValidRange}} =
    cond1 && cond2
  where
      cond1 = Interval.from d `Interval.contains` txInfoValidRange
      cond2 = txSignedBy info (unPaymentPubKeyHash o)

vestingTypedValidator :: TypedValidator Vesting
vestingTypedValidator = mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkVestingValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = mkUntypedValidator @VestingDatum @VestingRedeemer

------------------------------------------ Off-Chain --------------------------------------

vestingValidator :: Validator
vestingValidator = validatorScript vestingTypedValidator

vestingValidatorHash :: ValidatorHash
vestingValidatorHash = validatorHash vestingTypedValidator

vestingValidatorAddress :: Address
vestingValidatorAddress = validatorAddress vestingTypedValidator

payToVestingScriptTx :: Value -> POSIXTime -> State (TxConstructor d a i o) ()
payToVestingScriptTx v vestTime = do
    creator <- gets txCreator
    utxoProducedScriptTx vestingValidatorHash Nothing v (vestTime, fst creator)

withdrawFromVestingScriptTx :: State (TxConstructor d a i o) (Maybe (TxOutRef, ChainIndexTxOut))
withdrawFromVestingScriptTx = do
    creator <- gets txCreator
    time    <- gets txCurrentTime
    let f _ o = either (const False) g (_ciTxOutDatum o)
        g d   = fromMaybe False $ do
              (t, pkh) <- fromBuiltinData $ getDatum d
              return $ t <= time && pkh == fst creator
    utxoSpentScriptTx f (const . const vestingValidator) (const . const ())

---------------------------- For PlutusTx ------------------------------

PlutusTx.unstableMakeIsData ''Zp
PlutusTx.unstableMakeIsData ''R
PlutusTx.unstableMakeIsData ''Q
PlutusTx.unstableMakeIsData ''FBase
PlutusTx.unstableMakeIsData ''FExp
PlutusTx.unstableMakeIsData ''Proof

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
