#!/bin/bash

cd ../..

cardano-wallet serve --testnet testnet/testnet-byron-genesis.json --node-socket testnet/node.sock
