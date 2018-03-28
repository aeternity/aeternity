#!/bin/bash

# get the peer pubkey
CODE="{ok, PK} = aec_keys:peer_pubkey(),
      binary_to_list(aec_base58c:encode(peer_pubkey, PK))."

relx_nodetool eval $CODE

exit $?
