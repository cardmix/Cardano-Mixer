#!/bin/bash

rm -f ../plutus-pab.db

../../dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/cardano-mixer-backend-0.1.0.0/x/cardano-mixer-pab/build/cardano-mixer-pab/cardano-mixer-pab \
 --config ../pab-config.yml migrate
  
../../dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/cardano-mixer-backend-0.1.0.0/x/cardano-mixer-pab/build/cardano-mixer-pab/cardano-mixer-pab \
  --config ../pab-config.yml webserver \
  --passphrase 1234567890 --rollback-history 1 --resume-from 160bfae0008de6deed50bb2d7a149e149988aeaa613c791a7de45c6b7e8911f5,53664822
