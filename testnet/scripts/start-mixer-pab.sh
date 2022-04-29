#!/bin/bash

pid=$(pidof cardano-mixer-pab)
killall -9 cardano-mixer-pab

cd ../..

rm -f testnet/data/plutus-pab.db

testnet/bin/cardano-mixer-pab \
 --config testnet/pab-config.yml migrate
  
testnet/bin/cardano-mixer-pab \
  --config testnet/pab-config.yml webserver \
  --passphrase 1234567890
