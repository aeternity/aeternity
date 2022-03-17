#!/bin/bash

OPEN_FILES_RECOMMENDED=1024
ULIMIT=$(ulimit -n)
AE_OPEN_FILES_LIMIT=${AE_OPEN_FILES_LIMIT:-$ULIMIT}
export AE_OPEN_FILES_LIMIT

if [ "$OPEN_FILES_RECOMMENDED" -gt "$AE_OPEN_FILES_LIMIT" ]; then
    echo "WARNING: ulimit -n is $ULIMIT; $OPEN_FILES_RECOMMENDED is the recommended minimum."
    echo "You are recommended to ensure the node is stopped and raise the maximum number of open files before starting the node."
    echo "Visit https://github.com/aeternity/aeternity/wiki/Setting-Number-of-Max-Open-Files for additional pointers."
fi
if [ "$ULIMIT" -lt "$AE_OPEN_FILES_LIMIT" ]; then
    echo "Trying to raise fd limit to $AE_OPEN_FILES_LIMIT"
    prlimit --pid=$PPID --nofile=$AE_OPEN_FILES_LIMIT
fi
