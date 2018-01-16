#!/bin/bash
set -e

# If not provided set to public facing IP address of this node
PEER_ADDRESS="${PEER_ADDRESS:-$(curl -s https://api.ipify.org)}"

# Use TestNet example config as configuration template
sed "s|peer_address: http://127.0.0.1|peer_address: http://${PEER_ADDRESS}|g" \
    docs/examples/epoch_testnet.yaml > epoch.yaml

# Using console with extra arguments because "foreground" does not handle SIGTERM/SIGQUIT
exec ./bin/epoch console -noshell -noinput
