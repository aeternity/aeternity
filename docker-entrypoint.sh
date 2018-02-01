#!/bin/bash
set -e

# If not provided set to public facing IP address of this node
EXTERNAL_PEER_ADDRESS="${EXTERNAL_PEER_ADDRESS:-http://$(curl -s https://api.ipify.org):3013/}"

# Allow changing peers address[0], defaults to testnet node1
PEERS_ADDRESS_0="${PEERS_ADDRESS_0:-http://31.13.248.102:3013/}"

# Use TestNet example config as configuration template
sed -e "s|peer_address: http://127.0.0.1:3013/|peer_address: ${EXTERNAL_PEER_ADDRESS}|g" \
    -e "s|http://31.13.248.102:3013/|${PEERS_ADDRESS_0}|g" \
    docs/examples/epoch_testnet.yaml > epoch.yaml

# Using console with extra arguments because "foreground" does not handle SIGTERM/SIGQUIT
exec ./bin/epoch console -noshell -noinput
