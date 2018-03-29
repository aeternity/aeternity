#!/bin/bash

KEYSDIR=${1:-"$HOME/node/keys"}
PASS=${2:-"secret"}

# get the peer pubkey
CODE="{ok, PK} = aec_keys:peer_pubkey(),
      binary_to_list(aec_base58c:encode(peer_pubkey, PK))."

! PK=$(relx_nodetool eval $CODE)

if [ $? -eq 1 ]; then
    echo -n $PK | tr -d '"'
    exit 0
fi

CODE="{ok, Bin} = file:read_file(filename:join(\"$KEYSDIR\", \"peer_key.pub\")),
      PwdHash = crypto:hash(sha256, \"$PASS\"),
      PK = crypto:block_decrypt(aes_ecb, PwdHash, Bin),
      PKStr = binary_to_list(aec_base58c:encode(peer_pubkey, PK)),
      io:format(\"~s\", [PKStr]),
      init:stop(). "


PATH=$BINDIR:$PATH
LIBPATH1=$PWD/lib/aecore-0.1.0/ebin
LIBPATH2=$PWD/lib/base58-0.0.1/ebin

! erl -boot no_dot_erlang -sasl errlog_type error -noshell -pa "$LIBPATH1" -pa "$LIBPATH2" -eval "$CODE"

RC=$?
if [ $RC -eq 0 ]; then
    exit 1
fi

exit 0
