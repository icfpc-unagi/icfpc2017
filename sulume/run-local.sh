#!/bin/bash

source bin/imos-variables || exit 1
DEFINE_string map 'sample' 'Map'
IMOSH_PREDICATE=1 eval "${IMOSH_INIT}"

args=''
for arg in "$@"; do
  func::escapeshellarg arg
  args+=" ${arg}"
done

./ninetan/ninestream --debug \
    --master="./sulume/local --alsologtostderr --map=./map/${FLAGS_map}.json $args"
