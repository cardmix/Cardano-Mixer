#!/bin/bash

cd ../..

curl -H "content-type: application/json" -XPOST \
  -d @testnet/Wallets/restore-wallet2.json \
  localhost:8090/v2/wallets
