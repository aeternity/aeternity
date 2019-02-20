#!/usr/bin/env bash

# Wrapper to run get_peer_key

PATH=$BINDIR:$PATH
export ERL_LIBS=$ROOTDIR/lib
APPS_VSN=${REL_VSN:?}

relx_escript "lib/aeutils-${APPS_VSN:?}/priv/get_peer_key" "$NAME_TYPE" "$NAME" -setcookie "$COOKIE" $*

exit $?
