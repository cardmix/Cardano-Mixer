#!/bin/bash

cd ../..

curl -H "content-type: application/json" -XPOST \
  -d @tx.raw \
  localhost:8090/v2/wallets/5cd8d83d3de9770ac2970f6238386e183e216854/transactions-sign > tx.signed
