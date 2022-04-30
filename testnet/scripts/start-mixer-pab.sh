#!/bin/bash

cd ../..

rm -f testnet/data/plutus-pab.db

testnet/bin/cardano-mixer-pab \
 --config testnet/pab-config.yml migrate
  
testnet/bin/cardano-mixer-pab \
  --config testnet/pab-config.yml webserver \
  --passphrase 1234567890
