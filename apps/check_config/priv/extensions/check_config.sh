#!/bin/bash

# Simple wrapper to run check_config
# Relx extensions cannot be binary files but sourced bash files only

$SCRIPT_DIR/check_config $@
