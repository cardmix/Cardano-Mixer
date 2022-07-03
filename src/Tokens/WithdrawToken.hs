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
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module Tokens.WithdrawToken where

import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Typed.Scripts             (wrapMintingPolicy)
import           Ledger.Tokens                    (token)
import           Ledger.Value                     (AssetClass(..), TokenName (..), geq)
import           Plutus.V1.Ledger.Api             (Credential(..))
import           PlutusTx                         (compile, applyCode, liftCode)
import           PlutusTx.AssocMap                (fromList)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger, mempty)

import           Crypto
import           Crypto.Conversions (dataToZp)
import           Mixer
import           Scripts.Constraints              (tokensMinted, utxoProduced, utxoReferenced, tokensBurnedTx, utxoProducedScriptTx,
                                                    tokensMintedTx, utxoReferencedTx, utxoProducedPublicKeyTx, utxoSpentPublicKeyTx)
import           Scripts.MixerDepositScript       (MixerDepositDatum)
import           Scripts.VestingScript            ()
import           Tokens.DepositToken              (depositTokenName)
import           Types.TxConstructor              (TxConstructor (..))
import           Utils.ByteString                 (ToBuiltinByteString(..))

------------------------------------ Sigma protocol ----------------------------------------------

-- leafs, key, addrExp, and addrValue
type SigmaProtocolInput = ([Fr], Fr, Fr, Fr)

-- triple of commitments, errors, and responses
type SigmaProtocolCommit = ([Fr], [Fr], [Fr])

type SigmaProtocolProof = (SigmaProtocolCommit, [Fr], [Fr])

g1 :: Fr
g1 = Zp 3

g2 :: Fr
g2 = Zp 5

g3 :: Fr
g3 = Zp 7

{-# INLINABLE sigmaProtocolChallenge #-}
sigmaProtocolChallenge :: SigmaProtocolProof -> Fr
sigmaProtocolChallenge ((as, bs, cs), _, xs) = dataToZp $ sha2_256 $ toBytes $ map fromZp (as ++ bs ++ cs ++ xs)

-- TODO: check lengths of the arrays
{-# INLINABLE sigmaProtocolVerify #-}
sigmaProtocolVerify :: SigmaProtocolInput -> SigmaProtocolProof -> Bool
sigmaProtocolVerify (leafs, key, addrExp, addr) proof@((as, bs, cs), es, xs) = eq1 && eq2 && eq3 && eq4
    where
        ys  = xs
        zs  = map (addr *) xs
        s   = sigmaProtocolChallenge proof
        eq1 = all (\(a, (e, (x, l))) -> pow g1 (fromZp x) == a * pow l (fromZp e)) $ zip as $ zip es $ zip xs leafs
        eq2 = all (\(b, (e, y)) -> pow g2 (fromZp y) == b * pow key (fromZp e)) $ zip bs $ zip es ys
        eq3 = all (\(c, (e, z)) -> pow g3 (fromZp z) == c * pow addrExp (fromZp e)) $ zip cs $ zip es zs
        eq4 = s == sum es


--------------------------- On-Chain -----------------------------

-- Deposit token symbol, Withdraw token symbol, ADAWithdraw script address, and TxOutRef of initial mint
type WithdrawTokenParams = (Mixer, CurrencySymbol, Address, TxOutRef)

type WithdrawTokenRedeemer = (SigmaProtocolInput, SigmaProtocolProof, (Fr, Fr, Address), Bool)

type WithdrawTokenNameParams = (Fr, Fr)

{-# INLINABLE withdrawTokenName #-}
withdrawTokenName :: WithdrawTokenNameParams -> TokenName
withdrawTokenName (Zp cur, Zp next) = TokenName $ toBytes cur `appendByteString` toBytes next

checkPolicy :: WithdrawTokenParams -> WithdrawTokenRedeemer -> ScriptContext -> Bool
checkPolicy (mixer, dSymb, adaWithdrawAddr, _) (sigmaInput@(leafs, cur, _, aVal), sigmaProof, (prev, next, addr), False)
    ctx@ScriptContext{scriptContextTxInfo=info} = cond1 && cond2 && cond3 && cond4 && cond5 && cond6
  where
      wSymb    = ownCurrencySymbol ctx
      nameBurn = withdrawTokenName (prev, next)
      namePrev = withdrawTokenName (prev, cur)
      nameCur  = withdrawTokenName (cur, next)
      vPrev    = token $ AssetClass (wSymb, namePrev)
      vCur     = token $ AssetClass (wSymb, nameCur)
      f leaf   = utxoReferenced info (\o -> txOutValue o `geq` token (AssetClass (dSymb, depositTokenName leaf)))

      -- tokens are minted and burned correctly
      cond1 = tokensMinted ctx $ fromList [(nameBurn, -1), (namePrev, 1), (nameCur, 1)]
      -- the newly minted tokens are sent to the correct script address
      cond2 = utxoProduced info (\o -> txOutAddress o == adaWithdrawAddr && txOutValue o `geq` (vPrev + vCur))
      -- the correct value us payed to the correct address
      cond3 = utxoProduced info (\o -> txOutAddress o == addr && txOutValue o `geq` mixerValueAfterWithdraw mixer)
      -- sigma protocol proof is correct
      cond4 = sigmaProtocolVerify sigmaInput sigmaProof
      -- the address is converted to a number correctly
      cond5 = aVal == dataToZp addr
      -- all leafs used in the protocol are presented
      cond6 = all f leafs
checkPolicy (_, _, adaWithdrawAddr, ref) (_, _, _, True)
    ctx@ScriptContext{scriptContextTxInfo=info} = cond1 && cond2 && cond3
  where
      nameCur  = withdrawTokenName (Zp 0, toZp (-1))
      vCur     = token $ AssetClass (ownCurrencySymbol ctx, nameCur)

      cond1 = tokensMinted ctx $ fromList [(nameCur, 1)]
      cond2 = utxoProduced info (\o -> txOutAddress o == adaWithdrawAddr && txOutValue o `geq` vCur)
      cond3 = isJust $ findTxInByTxOutRef ref info -- TODO: check that this finds only utxos that are spent

curPolicy :: WithdrawTokenParams -> MintingPolicy
curPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrapMintingPolicy . checkPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

-------------------------- Off-Chain -----------------------------

withdrawTokenSymbol :: WithdrawTokenParams -> CurrencySymbol
withdrawTokenSymbol par = scriptCurrencySymbol $ curPolicy par

withdrawTokenAssetClass :: WithdrawTokenParams -> WithdrawTokenNameParams -> AssetClass
withdrawTokenAssetClass par nParams = AssetClass (withdrawTokenSymbol par, withdrawTokenName nParams)

withdrawToken :: WithdrawTokenParams -> WithdrawTokenNameParams -> Value
withdrawToken par nParams = token $ withdrawTokenAssetClass par nParams

-- Constraints that Withdraw Token is minted in the transaction
withdrawTokenMintTx :: WithdrawTokenParams -> WithdrawTokenRedeemer -> MixerDepositDatum -> TxConstructor a i o -> TxConstructor a i o
withdrawTokenMintTx par@(mixer, dSymb, adaWithdrawAddr, _)
    red@((leafs, cur, _, _), _, (prev, next, addr), False) leaf constr =
    case res of
        Just vh ->
            tokensMintedTx (curPolicy par) red (withdrawToken par (prev, cur) + withdrawToken par (cur, next)) $
            tokensBurnedTx (curPolicy par) red (withdrawToken par (prev, next)) $
            utxoProducedScriptTx vh Nothing (withdrawToken par (prev, cur) + withdrawToken par (cur, next)) () $
            case addr of
                Address (PubKeyCredential pkh) _ ->
                    utxoProducedPublicKeyTx (PaymentPubKeyHash pkh) Nothing (mixerValueAfterWithdraw mixer) ()
                -- If the address is a script address, we assume that it is a MixerDepositScript address
                Address (ScriptCredential valHash) _ ->
                    utxoProducedScriptTx valHash Nothing (mixerValueAfterWithdraw mixer) leaf
            $ foldr ((.) . (\l -> utxoReferencedTx (\o -> txOutValue o `geq` token (AssetClass (dSymb, depositTokenName l))))) id leafs constr
        Nothing -> constr { txConstructorResult = Nothing }
    where
        res = case adaWithdrawAddr of
            Address (ScriptCredential vh) _ -> Just vh
            _                               -> Nothing
withdrawTokenMintTx par@(_, _, adaWithdrawAddr, ref)
    red@((_, cur, _, _), _, (prev, next, _), True) _ constr = 
    case res of
        Just vh ->
            tokensMintedTx (curPolicy par) red (withdrawToken par (prev, cur) + withdrawToken par (cur, next)) $
            utxoProducedScriptTx vh Nothing (withdrawToken par (prev, cur) + withdrawToken par (cur, next)) () $
            -- TODO: add spent TxOutRef function
            utxoSpentPublicKeyTx (\r _ -> r == ref) constr
        Nothing -> constr { txConstructorResult = Nothing }
    where
        res = case adaWithdrawAddr of
            Address (ScriptCredential vh) _ -> Just vh
            _                               -> Nothing