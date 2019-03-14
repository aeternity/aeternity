#!/usr/bin/env bash

# Wrapper to run rename_db

PATH=$BINDIR:$PATH
export ERL_LIBS=$ROOTDIR/lib
APPS_VSN=${REL_VSN:?}

"$ERTS_DIR/bin/escript" "$ROOTDIR/lib/aeutils-${APPS_VSN:?}/priv/rename_db" "$@"

exit $?
