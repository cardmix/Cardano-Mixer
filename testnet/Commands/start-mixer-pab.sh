#!/bin/bash

rm -f ../plutus-pab.db

../../dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/cardano-mixer-0.1.0.0/x/cardano-mixer-pab/build/cardano-mixer-pab/cardano-mixer-pab \
 --config ../pab-config.yml migrate
  
../../dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/cardano-mixer-0.1.0.0/x/cardano-mixer-pab/build/cardano-mixer-pab/cardano-mixer-pab \
  --config ../pab-config.yml webserver \
  --passphrase 1234567890 --rollback-history 1 --resume-from e0d89806f8fb336b99c74928ddc923c90ac2b89f351e8d3b6247f182c498e525,46745773
