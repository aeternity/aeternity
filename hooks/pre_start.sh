#!/bin/bash

OPEN_FILES_RECOMMENDED=1024
OPEN_FILES_LIMIT=$(ulimit -n)

if [ "$OPEN_FILES_RECOMMENDED" -gt "$OPEN_FILES_LIMIT" ]; then
    echo "WARNING: ulimit -n is $OPEN_FILES_LIMIT; $OPEN_FILES_RECOMMENDED is the recommended minimum."
    echo "Try 'ulimit -n $OPEN_FILES_RECOMMENDED' before starting the node."
fi
