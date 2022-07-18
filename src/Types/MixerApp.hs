{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Types.MixerApp where

import           Control.Monad.State                      (State, gets, execState)
import           Data.Functor                             (($>))
import           Data.Map                                 (empty)
import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Typed.Scripts                     (ValidatorTypes(..))
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (undefined)

import           IO.ChainIndex                            (ChainIndexCache (..))
import           Scripts.ADAWithdrawScript                (adaWithdrawAddress)
import           Scripts.Constraints                      (failTx)
import           Scripts.MixerDepositScript               (mixerDepositAddress, withdrawFromMixerDepositScriptTx)
import           Scripts.MixerScript                      (mixerAddress, mixerValidatorHash, withdrawFromMixerScriptTx, Mixing)
import           Tokens.DepositToken                      (depositTokenSymbol, depositTokenMintTx)
import           Tokens.MixerBeaconToken                  (mixerBeaconCurrencySymbol, mixerBeaconTokenName, mixerBeaconMintTx, mixerBeaconSendTx)
import           Tokens.WithdrawToken                     (withdrawTokenSymbol, withdrawTokenMintTx, withdrawTokenFirstMintTx)
import           Types.Mixer                              (Mixer, mixerFromProtocol)
import           Types.MixerInput                         (MixerInput (..), toWithdrawTokenRedeemer)
import           Types.MixerInstance                      (MixerInstance (..))
import           Types.TxConstructor                      (TxConstructor (..), selectTxConstructor)
import           Utils.Prelude                            (uncurry4)


type MixerTxConstructor = TxConstructor [MixerInput] Mixing (RedeemerType Mixing) (DatumType Mixing)

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

-------------------------------------------- ChainIndexCache -----------------------------------------------------

mkMixerChainIndexCache :: ChainIndexCache
mkMixerChainIndexCache = ChainIndexCache addrs empty zero
    where addrsMI acc mi = acc ++ map ($ mi) [miMixerDepositAddress, miMixerAddress, miADAWithdrawAddress]
          addrs          = foldl addrsMI [] mixerInstances

--------------------------------------------- MixerInstances -----------------------------------------------------

-- TODO: move this to some config file
mixerInstances :: [MixerInstance]
mixerInstances = map (uncurry4 mkMixerInstance) undefined

mkMixerInstance :: Value -> Integer -> TxOutRef -> TxOutRef -> MixerInstance
mkMixerInstance v r = toMixerInstance (mixerFromProtocol v r)

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
        miMixerValidatorHash         = mixerValidatorHash wSymb
     }
     where
          bSymb = mixerBeaconCurrencySymbol beaconRef
          dSymb = depositTokenSymbol (mixer, (bSymb, mixerBeaconTokenName), adaWithdrawAddress)
          wSymb = withdrawTokenSymbol (mixer, dSymb, adaWithdrawAddress, withdrawRef)
