#!/bin/bash

cd ../..

export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig"

/home/ec2-user/cardano-my-node/Cardano-Mixer/testnet/bin/cardano-mixer-pab-client dispense
/home/ec2-user/cardano-my-node/Cardano-Mixer/testnet/bin/cardano-mixer-pab-client retrieve
