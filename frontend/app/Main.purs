module Main where

import Prelude

import Data.BigInt.Argonaut (fromInt, pow)
import Effect (Effect)
import Effect.Console (log)

import Crypto.Zp (Fr, fromZp, toZp)

z1 ∷ Fr
z1 = toZp $ pow (fromInt 7388749) (fromInt 73)

z2 ∷ Fr
z2 = toZp $ pow (fromInt 32487) (fromInt 568)

main :: Effect Unit
main = do
  log "🍝"
  log $ show $ fromZp z1
  log $ show $ fromZp z2
  log $ show (z1 <= z2)
