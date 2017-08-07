#!/bin/bash

source bin/imos-variables || exit 1
DEFINE_string map 'sample' 'Map'
DEFINE_string dot '' 'dot'
DEFINE_int scale 3 'scale'
DEFINE_bool debug FALSE 'Debug'
DEFINE_string listener '' 'listener command'
DEFINE_bool say_you FALSE '' 'handshake'
DEFINE_bool futures TRUE 'futures extension'
DEFINE_bool splurges TRUE 'splurges extension'
DEFINE_bool options TRUE 'options extension'
IMOSH_PREDICATE=1 eval "${IMOSH_INIT}"

args=''
for arg in "$@"; do
  func::escapeshellarg arg
  args+=" ${arg}"
done


flags=(
	--debug="${FLAGS_debug}"
)

if [ -z "${FLAGS_listener}" ]; then
	flags+=(--communicate)
else
	func::escapeshellarg FLAGS_listener
fi

./ninetan/ninestream "${flags[@]}" \
    --master="./sulume/local --logtostderr --map=./map/${FLAGS_map}.json --dot=${FLAGS_dot} --scale=${FLAGS_scale} --listener=${FLAGS_listener} --say_you=${FLAGS_say_you} --futures=${FLAGS_futures} --splurges=${FLAGS_splurges} --options=${FLAGS_options} $args"
