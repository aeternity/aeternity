#!/bin/bash

# Wrapper to run keys_gen
# Does not use relx_escript, as it does not work well with whitespaces in password, even in brackets.

PATH=$BINDIR:$PATH
"$ERTS_DIR/bin/escript" "$ROOTDIR/lib/aeutils-0.1.0/priv/keys_gen" "$*"

exit $?
