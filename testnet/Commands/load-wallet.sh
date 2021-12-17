#!/bin/bash

curl -H "content-type: application/json" -XPOST \
  -d @../Wallets/restore-wallet.json \
  localhost:8090/v2/wallets
