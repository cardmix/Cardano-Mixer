{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.MixerInput where

import           Cardano.Api                      (FromJSON, ToJSON)
import           Data.Aeson                       (decodeFileStrict)
import           Data.Map                         (Map)
import           Data.Set                         (Set, fromList, isSubsetOf, notMember)
import           GHC.Generics                     (Generic)
import           Ledger                           hiding (singleton, validatorHash, unspentOutputs)
import           Plutus.ChainIndex.Tx             (ChainIndexTx)
import           PlutusTx.Prelude                 hiding ((<$>))
import           Prelude                          (undefined, Show, IO, (<$>))
import qualified Prelude

import           Crypto
import           Crypto.Conversions               (dataToZp)
import           MixerProofs.SigmaProtocol
import           Types.MixerInstance              (MixerInstance)


type WithdrawTokenNameParams = (BaseField, BaseField)

withdrawFirstTokenParams :: WithdrawTokenNameParams
withdrawFirstTokenParams = (toZp 0, toZp (-1))

data MixerInput = MixerInput
    {
        mtMixerInstance           :: MixerInstance,
        mtSigmaProtocolInput      :: SigmaProtocolInput,
        mtSigmaProtocolProof      :: SigmaProtocolProof,
        mtWithdrawTokenNameParams :: WithdrawTokenNameParams,
        mtWithdrawAddress         :: Address,
        mtNewLeaf                 :: BaseField        
    }
    deriving (Prelude.Eq, Show, Generic, FromJSON, ToJSON)

type WithdrawTokenRedeemer = (SigmaProtocolInput, SigmaProtocolProof, (BaseField, BaseField, Address), Bool)

toWithdrawTokenRedeemer :: MixerInput -> WithdrawTokenRedeemer
toWithdrawTokenRedeemer mt = (mtSigmaProtocolInput mt, mtSigmaProtocolProof mt, (prev, next, mtWithdrawAddress mt), True)
    where (prev, next) = mtWithdrawTokenNameParams mt

mixerInputVerify :: Set BaseField -> Set BaseField -> MixerInput -> Bool
mixerInputVerify leafs keys (MixerInput _ inp@(ls, cur, _, aVal) proof (prev, next) addr _) = cond1 && cond2 && cond3 && cond4 && cond5
    where
        cond1 = sigmaProtocolVerify testGens inp proof
        cond2 = prev < cur && cur < next
        cond3 = aVal == dataToZp addr
        cond4 = fromList ls `isSubsetOf` leafs
        cond5 = cur `notMember` keys

mixerInputFilter :: Set BaseField -> Set BaseField -> [MixerInput] -> [MixerInput]
mixerInputFilter leafs keys = filter (mixerInputVerify leafs keys)

updateMixerInputs :: [MixerInput] -> IO [MixerInput]
updateMixerInputs ins = do
    let file   = "data/inputs.json"
    fromMaybe ins <$> decodeFileStrict file

fromWithdrawRequest :: [MixerInstance] -> Map TxOutRef (ChainIndexTxOut, ChainIndexTx) -> WithdrawRequest -> Maybe MixerInput
fromWithdrawRequest = undefined