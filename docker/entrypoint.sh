#!/bin/bash
set -e

# Use TestNet example config as configuration template
cp docs/examples/epoch_testnet.yaml epoch.yaml

# Using console with extra arguments because "foreground" does not handle SIGTERM/SIGQUIT
exec ./bin/epoch console -noshell -noinput $@
