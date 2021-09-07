#!/usr/bin/env bash

# Wrapper to inspect and change the runtime mode

PATH=$BINDIR:$PATH
export ERL_LIBS=$ROOTDIR/lib
APPS_VSN=${REL_VSN:?}

relx_escript "lib/aeutils-${APPS_VSN:?}/priv/runtime_mode" "$NAME_TYPE" "$NAME" -setcookie "$COOKIE" $*

exit $?
