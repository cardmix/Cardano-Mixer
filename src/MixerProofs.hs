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
{-# LANGUAGE TypeSynonymInstances       #-}

module MixerProofs (generateWithdrawProof, verifyWithdraw) where

import           Data.Aeson                       (decode)
import           Data.ByteString.Lazy             (readFile)
import           Data.Map                         (fromList, elems)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)
import           Prelude                          (String, IO, (<$>), print)

import           Crypto

------------------------------------ Withdraw Proof ---------------------------------------------------

fileWithdrawR1CS :: String
fileWithdrawR1CS = "circuit-mixer.json"

generateWithdrawProof :: (Fr, Fr, Fr, Fr, Fr, Fr, Fr, [Fr], [Fr], Fr, Fr, Fr) -> IO Proof
generateWithdrawProof (root, a, key, leaf', c, r1, r2, cp, l, a', r1', r2') = do
    r1cs <- loadR1CSFile fileWithdrawR1CS
    crs <- fromMaybe emptyCRS . decode <$> readFile fileCRS
    let sa = SetupArguments 9 34 (21183+34) r1cs
    -- constructing witness
    let w  = fromList $ zip ((0 :: Integer) : [5..34])
            ([one, root, a, key, leaf', c, r1, r2] ++ cp ++ l ++ [a', r1', r2'] :: [Fr])
        sol = solveR1CS r1cs w
        pa = ProveArguments sa crs sol

        aa = map (makeSub sol . leftCoefs) r1cs
        bb = map (makeSub sol . rightCoefs) r1cs
        cc = map (makeSub sol . outCoefs) r1cs
        bsol = zipWith (PlutusTx.Prelude.==) (zipWith (PlutusTx.Prelude.*) aa bb) cc
    print $ length $ filter (== False) bsol
    print $ map ((elems sol) !! ) [1, 2, 3, 4]
    generateProof pa

{-# INLINABLE verifyWithdraw #-}
verifyWithdraw :: [Fr] -> Proof -> Bool
verifyWithdraw = verify withdrawCRS


------------------------------ Withdraw Problem Definition --------------------------------------------

{-# INLINABLE withdrawCRS #-}
withdrawCRS :: ReducedReferenceString
withdrawCRS = ReducedReferenceString
    {
refRedGa = 
CP (Zp 2222405742820845777989426016452041511644032873298254838090929483795559014585274462663440012313147558220607360319020) (Zp 582157746107502982083412411572128364431232048936965246362990275396286649448030974754972985307680863859043131585046),
refRedHb = 
CP (E (P [Zp 1537764612035941685892670533228349767967900379142195050775137400428631603778652411241835162744326284539488708814447,Zp 52154626516776493356596555181334727535349537252937721376757751713186662531437399310714160604134251176187601110464])) (E (P [Zp 1164117765372175316486325273869408813400001356819656513037917544544136413909830815742415708158505787426992453474452,Zp 405374138610411643907447422914609844961659878654525792046619997461675742689771449015007063157510734364556272385171])),
refRedHg = 
CP (E (P [Zp 1442386322136645008700237858578038956422879716793345821692387428703287120387557321504086059727304355190795073385934,Zp 3291267150749769626489753496526403681612405584043860674253329714142942360315395751931165200129588781074231343036588])) (E (P [Zp 3297201925347919208662329804096443763224746730558125632903376350882302517389065808504389475392392789595085768006773,Zp 2175086078916036149173002182634448589840639819203685899041339861832233660273163074040885399629516143863680322323106])),
refRedHd = 
CP (E (P [Zp 857910145481555285854869337825873804945961631607974352922670770293585478648289339593485114227112843421322416754230,Zp 1328188210131674270947557090423940287797265837921971950558773714689120438904465652981087686797539825062265694745338])) (E (P [Zp 21946058294187975309726970638741353482339483652050154393496390054132990046935772226576315425134729349615330842359,Zp 3301656619640225725562489430937433913268810235212466284749188339698413099188417843438963691474682705314682236859013])),
refRedGpub = [
CP (Zp 3874919966105130325308450416102149307155031587668490655235004747524954421582103207049936555905058478543714559360088) (Zp 3312584087931766873811527398327299928808754641883323780796165172830113322138225809957052539222520081945245059917437),
CP (Zp 1835472579233264508525992931119235433805497909503684040332059212644021131183465138566122044911949357867050920203097) (Zp 3237481100667389154836162777172523653244357136631954080923871513892088139187057429175397579272661714377249436751003),
CP (Zp 1092070109778316356591534795541041543771425330955614557131585391668971658109076494278564389776412585723443437809598) (Zp 1260919943505336417239680669079769444330282700352591507471650343876600339641434481320059697827192844196694647986386),
CP (Zp 196884623720521834650667094726515677410199592134922406617384764655455141917067896250905785342003457836556708113163) (Zp 1929085123981976485242452123172159838107517148214066600512699356198422350925871504221158918925856497449635750313968),
CP (Zp 3421272856027340127982735035120443803967653089102263481553408466822590123591307907848149452097588429450318157550822) (Zp 1874105708838235238783179628662809315210104737225155552529937812445560984824968888564500408557826278096255660549957),
CP (Zp 1092070109778316356591534795541041543771425330955614557131585391668971658109076494278564389776412585723443437809598) (Zp 1260919943505336417239680669079769444330282700352591507471650343876600339641434481320059697827192844196694647986386),
CP (Zp 1366672729961367235855006209308398504221082736536285392709928221650686110881973390836179837172851463803779014358784) (Zp 3926195799319263915787756867606201091978085564434870993460270832245448482585484740268675713659920788157896455129379),
CP (Zp 1835472579233264508525992931119235433805497909503684040332059212644021131183465138566122044911949357867050920203097) (Zp 3237481100667389154836162777172523653244357136631954080923871513892088139187057429175397579272661714377249436751003),
CP (Zp 196884623720521834650667094726515677410199592134922406617384764655455141917067896250905785342003457836556708113163) (Zp 1929085123981976485242452123172159838107517148214066600512699356198422350925871504221158918925856497449635750313968),
CP (Zp 2336952906323695118630964532078441261993852633990397713898573283589401227114785071318233865678913612857103171377985) (Zp 2810384196860965618641756284888532685508355322697239407553994206391949961770533789960652770127324661646663078192538)
]
    }
    