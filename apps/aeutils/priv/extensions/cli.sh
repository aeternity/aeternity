#!/usr/bin/env bash

node_name="$(echo $NAME | cut -d'@' -f1)"

ECLI_SOCKET_PATH="/var/tmp/mgmtd.${node_name}.cli.socket" $ROOTDIR/bin/cli
