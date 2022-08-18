#!/usr/bin/env bash

node_name="$(echo $NAME | cut -d'@' -f1)"

ECLI_SOCKET_PATH="/var/tmp/mgmtd.${node_name}.cli.socket" $ROOTDIR/lib/ecli-0.1.0/priv/ecli
