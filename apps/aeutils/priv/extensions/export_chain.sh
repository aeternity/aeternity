#!/usr/bin/env bash

# Wrapper to run export_chain

PATH=$BINDIR:$PATH
export ERL_LIBS=$ROOTDIR/lib
APPS_VSN=${REL_VSN:?}

relx_escript "lib/aeutils-${APPS_VSN:?}/priv/export_chain" "$NAME_TYPE" "$NAME" -setcookie "$COOKIE" $*

exit $?
