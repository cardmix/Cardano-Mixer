#!/bin/bash

rm -f ../plutus-pab.db

../../dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/cardano-mixer-backend-0.1.0.0/x/cardano-mixer-pab/build/cardano-mixer-pab/cardano-mixer-pab \
 --config ../pab-config.yml migrate
  
../../dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/cardano-mixer-backend-0.1.0.0/x/cardano-mixer-pab/build/cardano-mixer-pab/cardano-mixer-pab \
  --config ../pab-config.yml webserver \
  --passphrase 1234567890 --rollback-history 1 --resume-from bc78cfda9cd73df99b00eb3c35eadddbe97974bf52db706a4880277b2989c3b3,50201135
