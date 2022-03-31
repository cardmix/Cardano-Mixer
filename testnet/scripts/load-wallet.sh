#!/bin/bash

cd ../..

curl -H "content-type: application/json" -XPOST \
  -d @testnet/Wallets/restore-wallet.json \
  localhost:8090/v2/wallets
  
curl -H "content-type: application/json" -XPOST \
  -d @testnet/Wallets/restore-wallet4.json \
  localhost:8090/v2/wallets
