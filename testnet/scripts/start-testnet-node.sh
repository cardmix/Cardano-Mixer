#!/bin/bash

cd ../..

cardano-node run \
    --config testnet/testnet-config.json \
    --topology testnet/testnet-topology.json \
    --database-path testnet/data/db \
    --socket-path testnet/node.sock \
    --port 3003
