{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module RelayRequest where

import           PlutusTx.Prelude   hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude            (Show)

import           Crypto
import           Crypto.Conversions (dataToZp)
import           MixerContractTypes (WithdrawParams(..))
import           MixerProofs        (verifyWithdraw)
import           MixerState         (MixerState, MerkleTree(..), getMerkleTree)
import           Utils.Common       (last)

data RelayRequest = RelayRequestAccepted
    | WrongRootValue                     -- the root for the current deposit was calculated incorrectly (in particular, when the current deposit does not exist)
    | WrongWithdrawalAddress             -- the hash of the address was calculated incorrectly
    | WrongProof                         -- proof verification fails
  deriving Show

checkRelayRequest :: MixerState -> WithdrawParams -> RelayRequest
checkRelayRequest state params
  | checkWrongRootValue state params         = WrongRootValue
  | checkWrongWithdrawalAddress state params = WrongWithdrawalAddress
  | checkWrongProof state params             = WrongProof
  | otherwise                                = RelayRequestAccepted

checkWrongRootValue :: MixerState -> WithdrawParams -> Bool
checkWrongRootValue state (WithdrawParams _ pos@(k, m) _ subs _)
  | null subs                           = True
  | isNothing $ getMerkleTree state pos = True
  | otherwise                           = head subs /= root
    where MerkleTree _ leafs = state !! k
          coPath             = getMerkleCoPath leafs m
          root               = last coPath

checkWrongWithdrawalAddress :: MixerState -> WithdrawParams -> Bool
checkWrongWithdrawalAddress _ (WithdrawParams _ _ pkh subs _)
  | length subs < 2           = True
  | dataToZp pkh /= subs !! 1 = True
  | otherwise                 = False

checkWrongProof :: MixerState -> WithdrawParams -> Bool
checkWrongProof _ (WithdrawParams _ _ _ subs proof) = not $ verifyWithdraw pubParams proof
    where pubParams = [one, zero, zero, zero, zero, zero] ++ subs