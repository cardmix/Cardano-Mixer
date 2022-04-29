#!/bin/bash

cd ../..

$NODE_HOME/Cardano-Mixer/testnet/bin/cardano-mixer-pab-client dispense
$NODE_HOME/Cardano-Mixer/testnet/bin/cardano-mixer-pab-client retrieve
