#!/bin/bash

cd ../..

export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig"

testnet/bin/cardano-mixer-pab-client dispense
testnet/bin/cardano-mixer-pab-client retrieve
