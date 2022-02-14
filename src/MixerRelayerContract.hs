{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}


module MixerRelayerContract (
    MixerRelayerSchema,
    mixerRelayerProgram
) where

import           Ledger                                   hiding (singleton, validatorHash, unspentOutputs)
import           Ledger.Constraints.OffChain              (typedValidatorLookups)
import           Ledger.Constraints.TxConstraints
import           Plutus.Contract                          (Promise, ContractError, Endpoint, Contract,
                                                            endpoint, selectList, mkTxConstraints, submitTxConfirmed, logInfo)
import           Plutus.V1.Ledger.Ada                     (lovelaceValueOf)
import           PlutusTx.Prelude                         hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  ((<>))

import           MixerScript
import           Tokens.RelayTicketToken                  (relayTicketTokenMintTx)


-- "deposit" endpoint implementation
mintRelayTickets :: Promise () MixerRelayerSchema ContractError ()
mintRelayTickets = endpoint @"Mint Relay Tickets" @(Value, Integer) $ \(v, n) -> do
    (lookups, cons) <- relayTicketTokenMintTx n
    let mixer = makeMixerFromFees v
        lookups' = lookups <> typedValidatorLookups (mixerInst mixer)
        cons'    = cons <> mustPayToTheScript () (lovelaceValueOf 2_000_000)
    utx <- mkTxConstraints lookups' cons'
    logInfo utx
    submitTxConfirmed utx

type MixerRelayerSchema = Endpoint "Mint Relay Tickets" (Value, Integer)

mixerRelayerProgram :: Contract () MixerRelayerSchema ContractError ()
mixerRelayerProgram =
    selectList [mintRelayTickets] >> mixerRelayerProgram

