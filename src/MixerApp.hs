{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module MixerApp where

import           Control.Monad.State                      (State, gets, execState)
import           Data.Functor                             (($>))
import           Data.Map                                 (empty)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)

import           Contracts.ChainIndex                     (ChainIndexCache (..))
import           Mixer                                    (Mixer)
import           MixerInput                               (MixerInput (..), toWithdrawTokenRedeemer)
import           MixerInstance                            (MixerInstance (..))
import           Scripts.ADAWithdrawScript                (adaWithdrawAddress, adaWithdrawValidatorHash)
import           Scripts.Constraints                      (failTx)
import           Scripts.MixerDepositScript               (mixerDepositAddress, withdrawFromMixerDepositScriptTx)
import           Scripts.MixerScript                      (mixerAddress, mixerValidatorHash, withdrawFromMixerScriptTx)
import           Tokens.DepositToken                      (depositTokenSymbol, depositTokenMintTx)
import           Tokens.MixerBeaconToken                  (mixerBeaconCurrencySymbol, mixerBeaconTokenName, mixerBeaconMintTx, mixerBeaconSendTx)
import           Tokens.WithdrawToken                     (withdrawTokenSymbol, withdrawTokenMintTx, withdrawTokenFirstMintTx)
import           Types.TxConstructor                      (TxConstructor (..), selectTxConstructor)
import GHC.Base (undefined)


toMixerInstance :: Mixer -> TxOutRef -> TxOutRef -> MixerInstance
toMixerInstance mixer beaconRef withdrawRef =
     MixerInstance
     {
        miMixer                      = mixer,
        miMixerBeaconTxOutRef        = beaconRef,
        miWithdrawTxOutRef           = withdrawRef,
        miMixerBeaconTokenName       = mixerBeaconTokenName,
        miMixerBeaconCurrencySymbol  = bSymb,
        miDepositCurrencySymbol      = dSymb,
        miWithdrawCurrencySymbol     = wSymb,
        miMixerDepositAddress        = mixerDepositAddress dSymb,
        miMixerAddress               = mixerAddress wSymb,
        miADAWithdrawAddress         = adaWithdrawAddress,
        miMixerValidatorHash         = mixerValidatorHash wSymb,
        miADAWithdrawValidatorHash   = adaWithdrawValidatorHash
     }
     where
          bSymb = mixerBeaconCurrencySymbol beaconRef
          dSymb = depositTokenSymbol (mixer, (bSymb, mixerBeaconTokenName))
          wSymb = withdrawTokenSymbol (mixer, dSymb, adaWithdrawAddress, withdrawRef)

mkMixerChainIndexCache :: MixerInstance -> ChainIndexCache
mkMixerChainIndexCache mi = ChainIndexCache addrs curs empty zero
    where
        addrs = map ($ mi) [miMixerDepositAddress, miMixerAddress, miADAWithdrawAddress]
        curs  = map ($ mi) [miMixerBeaconCurrencySymbol, miDepositCurrencySymbol, miWithdrawCurrencySymbol]

------------------------------------------- Mixer Transactions ---------------------------------------------

mixerDepositTx :: MixerInstance -> State (TxConstructor d a i o) ()
mixerDepositTx mi = do
    leaf <- withdrawFromMixerDepositScriptTx mi
    mapM_ (depositTokenMintTx mi) leaf

mixerWithdrawTx :: MixerInstance -> State (TxConstructor [MixerInput] a i o) ()
mixerWithdrawTx mi = do
    mts <- gets (filter (\mt -> mtMixerInstance mt == mi) . txInputData)
    case mts of
        mt:_ -> do
            withdrawTokenMintTx mi (toWithdrawTokenRedeemer mt) (mtNewLeaf mt)
            withdrawFromMixerScriptTx mi (mtWithdrawTokenNameParams mt) $> ()
        _    -> failTx Nothing $> ()

mixerTxs :: [State (TxConstructor [MixerInput] a i o) ()]
mixerTxs = foldl f [] mixerInstances
    where
        f acc mi = acc ++ map ($ mi) [mixerBeaconMintTx, mixerBeaconSendTx, withdrawTokenFirstMintTx, mixerDepositTx, mixerWithdrawTx]

execMixerTx :: TxConstructor [MixerInput] a i o -> Maybe (TxConstructor [MixerInput] a i o)
execMixerTx s = selectTxConstructor $ map (`execState` s) mixerTxs

--------------------------------------------- MixerInstances -----------------------------------------------------

-- TODO: move this to some config file
mixerInstances :: [MixerInstance]
mixerInstances = undefined