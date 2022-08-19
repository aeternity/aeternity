#!/usr/bin/env bash

# Wrapper to build a CLI 

PATH=$BINDIR:$PATH
export ERL_LIBS=$ROOTDIR/lib
APPS_VSN=${REL_VSN:?}

relx_escript "lib/aeutils-${APPS_VSN:?}/priv/status" "$NAME_TYPE" "$NAME" -setcookie "$COOKIE" $*

exit $?