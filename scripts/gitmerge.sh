#! /usr/bin/env bash

cd /workspace/fanner/katana2.0/Apps/Katana
git checkout KATANA_17A_BRANCH
git merge origin/KATANA_17A_BRANCH

cd /workspace/fanner/katana2.0/Apps/Geolib3
git checkout GEOLIB3_41A_BRANCH
git merge origin/GEOLIB3_41A_BRANCH
