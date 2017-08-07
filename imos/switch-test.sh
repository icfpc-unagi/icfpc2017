#!/bin/bash

set -e -u

################################################################################
# Setup testing.
################################################################################

cat <<'EOM' > "${TMPDIR}/expected"
OK 1
DEADLINE_EXCEEDED No ready stream.
OK 2
OK 3
OK 2
OK 3
OK 2 21:{"you":"Unagi 8101"}
OK 3 21:{"you":"Unagi 8102"}
OK 1 23:{"me":"Unagi Wrapper"}
OK 1
OK 1
OK 2 529:{"map":{"mines":[1,5],"rivers":[{"source":0,"target":1},{"source":1,"target":2},{"source":0,"target":7},{"source":7,"target":6},{"source":6,"target":5},{"source":5,"target":4},{"source":4,"target":3},{"source":3,"target":2},{"source":1,"target":7},{"source":1,"target":3},{"source":7,"target":5},{"source":5,"target":3}],"sites":[{"id":0,"x":0,"y":0},{"id":1,"x":1,"y":0},{"id":2,"x":2,"y":0},{"id":3,"x":2,"y":-1},{"id":4,"x":2,"y":-2},{"id":5,"x":1,"y":-2},{"id":6,"x":0,"y":-2},{"id":7,"x":0,"y":-1}]},"punter":0,"punters":2}
OK 2
OK 1 198:{"ready":0,"state":"22 serialization::archive 12 0 0 0 0 0 2 0 0 0 0 8 0 0 0 0 0 1 1 2 0 3 0 4 0 5 1 6 0 7 0 0 0 12 0 0 0 0 1 1 2 0 7 7 6 6 5 5 4 4 3 3 2 1 7 1 3 7 5 5 3 2 0 1 5 0 0 0 0 0 1 1 0 0"}
OK
EOM

ninetan/ninestream <<"EOM" >"${TMPDIR}/actual"
run 1 php imos/switch.php "nc -l 8101" "nc -l 8102"
read 1 300
run 1 nc localhost 8101
run 1 nc localhost 8102
write 2 20:{"me":"Unagi 8101"}
write 3 20:{"me":"Unagi 8102"}
read 2 1000
read 3 1000
read 1 1000
write 1 24:{"you":"Unagi Wrapper"}
write 1 529:{"map":{"mines":[1,5],"rivers":[{"source":0,"target":1},{"source":1,"target":2},{"source":0,"target":7},{"source":7,"target":6},{"source":6,"target":5},{"source":5,"target":4},{"source":4,"target":3},{"source":3,"target":2},{"source":1,"target":7},{"source":1,"target":3},{"source":7,"target":5},{"source":5,"target":3}],"sites":[{"id":0,"x":0,"y":0},{"id":1,"x":1,"y":0},{"id":2,"x":2,"y":0},{"id":3,"x":2,"y":-1},{"id":4,"x":2,"y":-2},{"id":5,"x":1,"y":-2},{"id":6,"x":0,"y":-2},{"id":7,"x":0,"y":-1}]},"punter":0,"punters":2}
read 2 1000
write 2 198:{"ready":0,"state":"22 serialization::archive 12 0 0 0 0 0 2 0 0 0 0 8 0 0 0 0 0 1 1 2 0 3 0 4 0 5 1 6 0 7 0 0 0 12 0 0 0 0 1 1 2 0 7 7 6 6 5 5 4 4 3 3 2 1 7 1 3 7 5 5 3 2 0 1 5 0 0 0 0 0 1 1 0 0"}
read 1
exit 0
EOM

diff "${TMPDIR}/expected" "${TMPDIR}/actual"

################################################################################
# Setup testing.
################################################################################

cat <<'EOM' > "${TMPDIR}/expected"
OK 1
DEADLINE_EXCEEDED No ready stream.
OK 2
OK 3
OK 2
OK 3
OK 2 21:{"you":"Unagi 8101"}
OK 3 21:{"you":"Unagi 8102"}
OK 1 23:{"me":"Unagi Wrapper"}
OK 1
OK 1
DEADLINE_EXCEEDED No ready stream.
OK
OK
EOM

ninetan/ninestream <<"EOM" >"${TMPDIR}/actual"
run 1 php imos/switch.php "nc -l 8101" "nc -l 8102"
read 1 300
run 1 nc localhost 8101
run 1 nc localhost 8102
write 2 20:{"me":"Unagi 8101"}
write 3 20:{"me":"Unagi 8102"}
read 2 1000
read 3 1000
read 1 1000
write 1 24:{"you":"Unagi Wrapper"}
write 1 -:{"stop":{"moves":[]}}
read -1 100
list
exit 0
EOM

diff "${TMPDIR}/expected" "${TMPDIR}/actual"
