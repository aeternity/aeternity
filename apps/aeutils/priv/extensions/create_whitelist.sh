#!/usr/bin/env bash

# Wrapper to run create_whitelist

PATH=$BINDIR:$PATH
export ERL_LIBS=$ROOTDIR/lib
APPS_VSN=${REL_VSN:?}

relx_escript "lib/aeutils-${APPS_VSN:?}/priv/create_whitelist" "$NAME_TYPE" "$NAME" -setcookie "$COOKIE" $*

exit $?
