#!/usr/bin/env bash

# Wrapper to run db_migrate

PATH=$BINDIR:$PATH
export ERL_LIBS=$ROOTDIR/lib
APPS_VSN=${REL_VSN:?}

relx_escript "lib/aeutils-${APPS_VSN:?}/priv/db_migrate" "$NAME_TYPE" "$NAME" -setcookie "$COOKIE" $*

exit $?
