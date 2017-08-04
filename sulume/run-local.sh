#!/bin/bash

source bin/imos-variables || exit 1
DEFINE_string map 'sample' 'Map'
IMOSH_PREDICATE=1 eval "${IMOSH_INIT}"

./ninetan/ninestream --debug \
    --master="./sulume/local --map=./map/${FLAGS_map}.json --ai='$*'"
