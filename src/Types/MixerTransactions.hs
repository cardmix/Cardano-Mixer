{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Types.MixerTransactions where

import           Control.Monad.State                      (State, gets, execState)
import           Data.Functor                             (($>))
import           Ledger.Typed.Scripts                     (ValidatorTypes(..))
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

import           Scripts.Constraints                      (failTx)
import           Scripts.MixerDepositScript               (withdrawFromMixerDepositScriptTx)
import           Scripts.MixerScript                      (Mixing, withdrawFromMixerScriptTx)
import           Tokens.DepositToken                      (depositTokenMintTx)
import           Tokens.MixerBeaconToken                  (mixerBeaconMintTx, mixerBeaconSendTx)
import           Tokens.WithdrawToken                     (withdrawTokenMintTx, withdrawTokenFirstMintTx)
import           Types.MixerInput                         (MixerInput (..), toWithdrawTokenRedeemer)
import           Types.MixerInstance                      (MixerInstance (..))
import           Types.TxConstructor                      (TxConstructor (..), selectTxConstructor)


type MixerTransaction = TxConstructor [MixerInput] Mixing (RedeemerType Mixing) (DatumType Mixing)
type MixerTransactionBuilder = State MixerTransaction ()

execTxs :: [MixerTransactionBuilder] -> MixerTransaction -> Maybe MixerTransaction
execTxs txs s = selectTxConstructor $ map (`execState` s) txs

mixerDepositTx :: MixerInstance -> MixerTransactionBuilder
mixerDepositTx mi = do
    leaf <- withdrawFromMixerDepositScriptTx mi
    mapM_ (depositTokenMintTx mi) leaf

mixerWithdrawTx :: MixerInstance -> MixerTransactionBuilder
mixerWithdrawTx mi = do
    mts <- gets (filter (\mt -> mtMixerInstance mt == mi) . txInputData)
    case mts of
        mt:_ -> do
            withdrawTokenMintTx mi (toWithdrawTokenRedeemer mt) (mtNewLeaf mt)
            withdrawFromMixerScriptTx mi (mtWithdrawTokenNameParams mt) $> ()
        _    -> failTx Nothing $> ()

-- Relaying
mixerTxs :: [MixerInstance] -> [MixerTransactionBuilder]
mixerTxs = foldl f []
    where
        f acc mi = acc ++ map ($ mi) [mixerDepositTx, mixerWithdrawTx]

-- Setting up a new mixer
mixerMakeTxs :: [MixerInstance] -> [MixerTransactionBuilder]
mixerMakeTxs = foldl f []
    where
        f acc mi = acc ++ map ($ mi) [mixerBeaconMintTx, mixerBeaconSendTx, withdrawTokenFirstMintTx]
