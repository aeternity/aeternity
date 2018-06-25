#!/bin/bash

# As this script might be used as docker health check it should exit with either 0/1

EXTERNAL_ADDRESS=${EXTERNAL_ADDRESS:-localhost:3013}
MIN_PEERS=${MIN_PEERS:-2}

curl -s -f -S -o /dev/null --retry 3 http://${EXTERNAL_ADDRESS}/v2/top || exit 1

if [ -z $INTERNAL_ADDRESS ]; then
    exit 0
fi

PEERS_COUNT=$(curl -s -S ${INTERNAL_ADDRESS}/v2/debug/peers | grep -o aenode | wc -l)

# Explicit exit because otherwise test would exit with status 127
test $PEERS_COUNT -ge $MIN_PEERS || exit 1
