#!/bin/bash

# get the peer pubkey
CODE="{ok, PK} = aec_keys:peer_pubkey(),
      binary_to_list(aec_base58c:encode(peer_pubkey, PK))."

PK=$(relx_nodetool eval $CODE)

# now print it out
echo $PK

exit $?
