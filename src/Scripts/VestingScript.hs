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

import           Ledger                         (Address, POSIXTime, PaymentPubKeyHash (..), ValidatorHash, TokenName, findDatumHash, Datum (..))
import           Ledger.Contexts                (ScriptContext (..), TxInfo (..), txSignedBy, TxOut (..))
import qualified Ledger.Interval                as Interval
import           Ledger.Typed.Scripts
import           Ledger.Value                   (Value (getValue), CurrencySymbol, geq, AssetClass (..), flattenValue)
import           PlutusTx
import           PlutusTx.Prelude               hiding ((<>), Eq, Semigroup, fold, mempty)

import           Crypto
import Scripts.Constraints (utxoReferenced, checkOwnInput, utxoProduced, utxoSpent)
import Ledger.Tokens (token)
import Data.Tuple.Extra (snd3)
import PlutusTx.AssocMap (lookup)

-- InvalidAsset, beacon with the required token currency symbol, beacon with the address to pay, the value to pay to the address
type VestingParams = (CurrencySymbol, CurrencySymbol, CurrencySymbol, Value)

type VestingDatum = (POSIXTime, PaymentPubKeyHash, Fr)

data VestingRedeemer = VestingRedeemerOK
                     | VestingRedeemerInvalidAsset (CurrencySymbol, TokenName, Address)
                     | VestingRedeemerMissingAsset (CurrencySymbol, Address)
                     | VestingRedeemerDuplicateAsset (CurrencySymbol, TokenName, TokenName, Address)

PlutusTx.unstableMakeIsData ''VestingRedeemer

data Vesting
instance ValidatorTypes Vesting where
    type instance RedeemerType Vesting = VestingRedeemer
    type instance DatumType Vesting = VestingDatum

-- The tx must:
-- 1) validate after 'd'
-- 2) be signed by 'o'
-- OR (invalid asset)
-- 1) reference an input with invalidAsset
-- 2) check that invalidAsset has the same TokenName as some asset in the validated utxo
-- 3) must pay to the second beacon token the correct amount (the parameter of the script)
-- OR (asset missing)
-- 1) reference an input with the first beacon token (read the correct currency symbol from datum)
-- 2) the correct currency smybol must not be present in the validated utxo
-- 3) must pay to the second beacon token the correct amount (the parameter of the script)
-- OR (duplicate asset)
-- 1) reference an input with the first beacon token (read the correct currency symbol from datum)
-- 2) reference an input with the duplicate asset
-- 2) the duplicate assset must be present in the validated utxo
-- 3) must pay to the second beacon token the correct amount (the parameter of the script)
{-# INLINABLE mkVestingValidator #-}
mkVestingValidator :: VestingParams -> VestingDatum -> VestingRedeemer -> ScriptContext -> Bool
mkVestingValidator _ (d, o, _) VestingRedeemerOK ScriptContext{scriptContextTxInfo=info@TxInfo{txInfoValidRange}} =
    cond1 && cond2
  where
      cond1 = Interval.from d `Interval.contains` txInfoValidRange
      cond2 = txSignedBy info (unPaymentPubKeyHash o)
mkVestingValidator (invalidAssetCurSymb, _, paymentBeconSymb, paymentValue) (_, _, leaf) (VestingRedeemerInvalidAsset (cs, tn, addr))
        ctx@ScriptContext{scriptContextTxInfo=info} =
    cond1 && cond2 && cond3 && cond4 && isJust dhPay
  where
      vInvalidAsset = token $ AssetClass (invalidAssetCurSymb, tn)
      vAsset        = token $ AssetClass (cs, tn)
      vPayBeacon    = token $ AssetClass (paymentBeconSymb, "")
      dhPay         = findDatumHash (Datum $ toBuiltinData leaf) info

      cond1 = utxoReferenced info (\o -> txOutValue o `geq` vInvalidAsset)
      cond2 = checkOwnInput ctx (\o -> txOutValue o `geq` vAsset)
      cond3 = utxoReferenced info (\o -> txOutValue o `geq` vPayBeacon && txOutAddress o == addr)
      cond4 = utxoProduced info (== TxOut addr paymentValue dhPay)
mkVestingValidator (_, symbBeaconSymb, paymentBeconSymb, paymentValue) (_, _, leaf) (VestingRedeemerMissingAsset (cs, addr))
        ctx@ScriptContext{scriptContextTxInfo=info} =
    cond1 && cond2 && cond3 && cond4 && isJust dhSymb && isJust dhPay
  where
      vSymbBeacon  = token $ AssetClass (symbBeaconSymb, "")
      dhSymb       = findDatumHash (Datum $ toBuiltinData cs) info
      vPayBeacon   = token $ AssetClass (paymentBeconSymb, "")
      dhPay        = findDatumHash (Datum $ toBuiltinData leaf) info

      cond1 = utxoReferenced info (\o -> txOutValue o `geq` vSymbBeacon && txOutDatumHash o == dhSymb)
      cond2 = checkOwnInput ctx (isNothing . lookup cs . getValue . txOutValue)
      cond3 = utxoReferenced info (\o -> txOutValue o `geq` vPayBeacon && txOutAddress o == addr)
      cond4 = utxoProduced info (== TxOut addr paymentValue dhPay)
mkVestingValidator (invalidAssetCurSymb, symbBeaconSymb, paymentBeconSymb, paymentValue) (_, _, leaf)
    (VestingRedeemerDuplicateAsset (cs, tnPrev, tnCur, addr)) ctx@ScriptContext{scriptContextTxInfo=info} =
    cond1 && cond2 && cond3 && cond4 && cond5
  where
      vSymbBeacon   = token $ AssetClass (symbBeaconSymb, "")
      dhSymb        = findDatumHash (Datum $ toBuiltinData cs) info
      vAssetPrev    = token $ AssetClass (cs, tnPrev)
      vAsset        = token $ AssetClass (cs, tnCur)
      vPayBeacon    = token $ AssetClass (paymentBeconSymb, "")
      dhPay         = findDatumHash (Datum $ toBuiltinData leaf) info

      cond1 = utxoReferenced info (\o -> txOutValue o `geq` vSymbBeacon && txOutDatumHash o == dhSymb)
      cond2 = utxoReferenced info (\o -> txOutValue o `geq` vAssetPrev)
      cond3 = utxoSpent info (\o -> txOutValue o `geq` vAsset)
      cond4 = utxoReferenced info (\o -> txOutValue o `geq` vPayBeacon && txOutAddress o == addr)
      cond5 = utxoProduced info (== TxOut addr paymentValue dhPay)

vestingTypedValidator :: VestingParams -> TypedValidator Vesting
vestingTypedValidator par = mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkVestingValidator par||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = wrapValidator

vestingValidator :: VestingParams -> Validator
vestingValidator = validatorScript . vestingTypedValidator

vestingValidatorHash :: VestingParams -> ValidatorHash
vestingValidatorHash = validatorHash . vestingTypedValidator

vestingValidatorAddress :: VestingParams -> Address
vestingValidatorAddress = validatorAddress . vestingTypedValidator

---------------------------- For PlutusTx ------------------------------

PlutusTx.unstableMakeIsData ''Zp
PlutusTx.unstableMakeIsData ''R
PlutusTx.unstableMakeIsData ''Q
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
