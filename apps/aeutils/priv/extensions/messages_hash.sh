#!/bin/bash

# Wrapper to run messages_hash

PATH=$BINDIR:$PATH
export ERL_LIBS=$ROOTDIR/lib
"$ERTS_DIR/bin/escript" "$ROOTDIR/lib/aeutils-0.1.0/priv/messages_hash" "$*"

exit $?
