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

module Scripts.DepositVestingScript where

import           Ledger                         (Address, POSIXTime, PaymentPubKeyHash (..), ValidatorHash)
import           Ledger.Contexts                (ScriptContext (..), TxInfo (..), txSignedBy)
import qualified Ledger.Interval                as Interval
import           Ledger.Typed.Scripts
import           Ledger.Value                   (Value)
import           PlutusTx
import           PlutusTx.Prelude               hiding ((<>), Eq, Semigroup, fold, mempty)

type VestingParams = ()

type VestingDatum = (POSIXTime, PaymentPubKeyHash)

data Vesting
instance ValidatorTypes Vesting where
    type instance RedeemerType Vesting = ()
    type instance DatumType Vesting = VestingDatum

-- cases: redeem, invalid deposit token

-- The tx must:
-- 1) validate after 'd'
-- 2) be signed by 'o'
-- OR
-- 1) reference an input with the first beacon token (read the correct currency symbol from datum)
-- 2) the correct currency smybol must not be present in the validated utxo
-- 3) must pay to the second beacon token the correct amount (the parameter of the script)
-- OR
-- 1) reference an input with invalidAsset
-- 2) check that invalidAsset has the same TokenName as some asset in the validated utxo
-- 3) must pay to the second beacon token the correct amount (the parameter of the script)
{-# INLINABLE mkVestingValidator #-}
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator (d, o) () ScriptContext{scriptContextTxInfo=txInfo@TxInfo{txInfoValidRange}} =
    cond1 && cond2 --   || governanceDecisionTokenRequired txInfo
  where
      cond1 = Interval.from d `Interval.contains` txInfoValidRange
      cond2 = txSignedBy txInfo (unPaymentPubKeyHash o)

vestingTypedValidator :: TypedValidator Vesting
vestingTypedValidator = mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkVestingValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = wrapValidator

vestingValidator :: Validator
vestingValidator = validatorScript vestingTypedValidator

vestingValidatorHash :: ValidatorHash
vestingValidatorHash = validatorHash vestingTypedValidator

vestingValidatorAddress :: Address
vestingValidatorAddress = validatorAddress vestingTypedValidator
