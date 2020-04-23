#!/usr/bin/env bash

# Simple wrapper to run `start` command, as it was renamed to `daemon`
# Relx extensions cannot be binary files but sourced bash files only
# Also append ERTS BINDIR to PATH to run the escript

PATH=$BINDIR:$PATH
$SCRIPT_DIR/aeternity daemon $@

# Relx extensions require explicit exit
exit $?
