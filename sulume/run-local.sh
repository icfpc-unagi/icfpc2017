#!/bin/bash

source bin/imos-variables || exit 1
DEFINE_string map 'sample' 'Map'
DEFINE_string dot '' 'dot'
DEFINE_int scale 3 'scale'
DEFINE_bool debug FALSE 'Debug'
IMOSH_PREDICATE=1 eval "${IMOSH_INIT}"

args=''
for arg in "$@"; do
  func::escapeshellarg arg
  args+=" ${arg}"
done

./ninetan/ninestream --debug=$FLAGS_debug --communicator \
    --master="./sulume/local --alsologtostderr --map=./map/${FLAGS_map}.json --dot=${FLAGS_dot} --scale=${FLAGS_scale} $args"
