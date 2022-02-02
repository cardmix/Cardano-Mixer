{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module ConnectToPABContract where

import           Data.Semigroup            (Last (..))
import           Ledger                    (PaymentPubKeyHash)
import           Plutus.Contract           (Promise, ContractError, Endpoint, endpoint, tell)
import           PlutusTx.Prelude          hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                   (String)

import           Configuration.PABConfig   (pabWallet)
import           Crypto                    (Fr)
import           Crypto.Conversions        (dataToZp)
import           Utils.Address             (strToPKH)

import Wallet.Emulator.Types (Wallet)


---------------------------------------------------------------------
--------------------------- Off-Chain -------------------------------
---------------------------------------------------------------------

type ConnectToPABSchema = Endpoint "Connect to PAB" String

connectToPABPromise :: Promise (Maybe (Last (PaymentPubKeyHash, Fr, Wallet))) ConnectToPABSchema ContractError ()
connectToPABPromise = endpoint @"Connect to PAB" @String $ \str ->
    let pkh = strToPKH str
        a   = dataToZp pkh
        w   = pabWallet
    in tell $ Just $ Last (pkh, a, w)
