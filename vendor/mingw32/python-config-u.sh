#!/usr/bin/env sh

THISDIR="$(dirname $0)"
test "$THISDIR" = "." && THISDIR=${PWD}

RESULT=$("${THISDIR}"/python-config.sh "$@")

exec_prefix_win=$("${THISDIR}"/python-config.sh --exec-prefix)
exec_prefix_unix=/mingw32
echo $(echo $RESULT | sed "s#${exec_prefix_win}#${exec_prefix_unix}#g")
