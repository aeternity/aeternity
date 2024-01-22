#!/usr/bin/env bash

PATH=$BINDIR:$PATH
export ERL_LIBS=$ROOTDIR/lib
APPS_VSN=${REL_VSN:?}

echo "${APPS_VSN}"

exit $?
