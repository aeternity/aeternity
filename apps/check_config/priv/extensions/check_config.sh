#!/bin/bash

# Simple wrapper to run check_config
# Relx extensions cannot be binary files but sourced bash files only
# Also append ERTS BINDIR to PATH to run the escript

PATH=$BINDIR:$PATH
$SCRIPT_DIR/check_config $@

# Relx extensions require explicit exit
exit $?
