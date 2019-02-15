#!/usr/bin/env bash

# Wrapper to run keys_gen
# Does not use relx_escript, as it does not work well with whitespaces in password, even in brackets.

PATH=$BINDIR:$PATH
export ERL_LIBS=$ROOTDIR/lib
APPS_VSN=${REL_VSN:?}
"$ERTS_DIR/bin/escript" "$ROOTDIR/lib/aeutils-${APPS_VSN:?}/priv/keys_gen" "$@"

exit $?
