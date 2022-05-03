{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Contracts.ConnectToPABContract where

import           Data.Semigroup            (Last (..))
import           Data.Text                 (Text)
import           Ledger                    (PaymentPubKeyHash)
import           Plutus.Contract           (Promise, ContractError, Endpoint, endpoint, tell)
import           PlutusTx.Prelude          hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Wallet.Emulator.Types     (Wallet)

import           Configuration.PABConfig   (pabWallet, pabWalletPKH)
import           Crypto                    (Fr)
import           Crypto.Conversions        (dataToZp)
import           Utils.Address             (textToPKH)

---------------------------------------------------------------------
--------------------------- Off-Chain -------------------------------
---------------------------------------------------------------------

type ConnectToPABSchema = Endpoint "connect-to-pab" Text

connectToPABPromise :: Promise (Maybe (Last (PaymentPubKeyHash, Fr, Wallet))) ConnectToPABSchema ContractError ()
connectToPABPromise = endpoint @"connect-to-pab" @Text $ \txt -> do
    let pkh = fromMaybe pabWalletPKH $ textToPKH txt
        a   = dataToZp pkh
        w   = pabWallet
    tell $ Just $ Last (pkh, a, w)
