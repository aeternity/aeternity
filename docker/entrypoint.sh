#!/bin/bash
set -e

# Allow changing peers address[0], defaults to testnet node1
PEERS_ADDRESS_0="${PEERS_ADDRESS_0:-aenode://pp\$ySU7cHqsymnuBP9iSe4rMnH1Rz2FStx5rnoewYMJcuPhdaqPk@31.13.249.1:3015}"

# Use TestNet example config as configuration template
sed -e "s|aenode://pp\$ySU7cHqsymnuBP9iSe4rMnH1Rz2FStx5rnoewYMJcuPhdaqPk@31.13.249.1:3015|${PEERS_ADDRESS_0}|g" \
    docs/examples/epoch_testnet.yaml > epoch.yaml

# Using console with extra arguments because "foreground" does not handle SIGTERM/SIGQUIT
exec ./bin/epoch console -noshell -noinput $@
