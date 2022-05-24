#!/bin/bash

cd ../..

cabal update
cabal new-build
cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-mixer-backend-0.1.0.0/x/cardano-mixer-pab/build/cardano-mixer-pab/cardano-mixer-pab testnet/bin/cardano-mixer-pab
cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-mixer-backend-0.1.0.0/x/cardano-mixer-pab-client/build/cardano-mixer-pab-client/cardano-mixer-pab-client testnet/bin/cardano-mixer-pab-client

rm -f testnet/data/plutus-pab.db

testnet/bin/cardano-mixer-pab --config testnet/pab-config.yml migrate
  
testnet/bin/cardano-mixer-pab --config testnet/pab-config.yml webserver --passphrase 1234567890
