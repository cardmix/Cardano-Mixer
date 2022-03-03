{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module RelayRequest where

import           PlutusTx.Prelude            hiding (Semigroup, (<$>), (<>), mempty, unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                     (Show)

import           Contracts.MixerKeysContract (MixerKeys)
import           Crypto
import           Crypto.Conversions          (dataToZp)
import           Types.MixerContractTypes    (WithdrawParams(..))

import           MixerProofs                 (verifyWithdraw)
import           MixerState                  (MixerState, MerkleTree(..), getMerkleTree)
import           Utils.Common                (last)


data RelayRequest = RelayRequestAccepted
    | WrongRootValue                     -- the root for the current deposit was calculated incorrectly (in particular, when the current deposit does not exist)
    | WrongWithdrawalAddress             -- the hash of the address was calculated incorrectly
    | WrongProof                         -- proof verification fails
    | DuplicateKey                       -- the key was reused
  deriving Show

checkRelayRequest :: MixerState -> MixerKeys -> WithdrawParams -> RelayRequest
checkRelayRequest state keys params
  | checkWrongRootValue state params   = WrongRootValue
  | checkWrongWithdrawalAddress params = WrongWithdrawalAddress
  | checkWrongProof params             = WrongProof
  | checkDuplicateKey keys params      = DuplicateKey
  | otherwise                          = RelayRequestAccepted

checkWrongRootValue :: MixerState -> WithdrawParams -> Bool
checkWrongRootValue state (WithdrawParams _ pos@(k, m) _ subs _)
  | null subs                           = True
  | isNothing $ getMerkleTree state pos = True
  | otherwise                           = head subs /= root
    where MerkleTree _ leafs = state !! k
          coPath             = getMerkleCoPath leafs m
          root               = last coPath

checkWrongWithdrawalAddress :: WithdrawParams -> Bool
checkWrongWithdrawalAddress (WithdrawParams _ _ pkh subs _)
  | length subs < 2           = True
  | dataToZp pkh /= subs !! 1 = True
  | otherwise                 = False

checkWrongProof :: WithdrawParams -> Bool
checkWrongProof (WithdrawParams _ _ _ subs proof) = not $ verifyWithdraw pubParams proof
    where pubParams = [one, zero, zero, zero, zero, zero] ++ subs

checkDuplicateKey :: MixerKeys -> WithdrawParams -> Bool
checkDuplicateKey keys (WithdrawParams _ _ _ subs _)
  | length subs < 3                   = True
  | null $ filter (== subs !! 2) keys = False
  | otherwise                         = True