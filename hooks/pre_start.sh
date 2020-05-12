#!/bin/bash

OPEN_FILES_RECOMMENDED=24576
OPEN_FILES_LIMIT=$(ulimit -n)

if [ "$OPEN_FILES_RECOMMENDED" -gt "$OPEN_FILES_LIMIT" ]; then
    echo "WARNING: ulimit -n is $OPEN_FILES_LIMIT; $OPEN_FILES_RECOMMENDED is the recommended minimum."
    echo "You are recommended to ensure the node is stopped and raise the maximum number of open files before starting the node."
    echo "Visit https://github.com/aeternity/aeternity/wiki/Setting-Number-of-Max-Open-Files for additional pointers."
fi
