#!/bin/bash

rm -f testnet/plutus-pab.db

dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/cardano-mixer-0.1.0.0/x/cardano-mixer-pab/build/cardano-mixer-pab/cardano-mixer-pab \
 --config testnet/pab-config.yml migrate
  
dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/cardano-mixer-0.1.0.0/x/cardano-mixer-pab/build/cardano-mixer-pab/cardano-mixer-pab \
  --config testnet/pab-config.yml webserver \
  --passphrase 1234567890
