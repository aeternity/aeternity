#!/bin/bash

KEYSDIR=${1:-"$HOME/node/keys"}
PASS=${2:-"secret"}

APPS_VSN=${REL_VSN:?}

# get the peer pubkey
CODE="{ok, PK} = aec_keys:peer_pubkey(),
      binary_to_list(aehttp_api_encoder:encode(peer_pubkey, PK))."

! PK=$(relx_nodetool eval $CODE)

if [ $? -eq 1 ]; then
    /bin/echo -n $PK | tr -d '"'
    exit 0
fi

CODE="{ok, Bin} = file:read_file(filename:join(\"$KEYSDIR\", \"peer_key.pub\")),
      PwdHash = crypto:hash(sha256, \"$PASS\"),
      PK = crypto:block_decrypt(aes_ecb, PwdHash, Bin),
      PKStr = binary_to_list(aehttp_api_encoder:encode(peer_pubkey, PK)),
      io:format(\"~s\", [PKStr]),
      init:stop(). "


PATH=$BINDIR:$PATH
LIBPATH1=$PWD/lib/aecore-${APPS_VSN:?}/ebin
LIBPATH2=$PWD/lib/base58-0.0.1/ebin
LIBPATH3=$PWD/lib/aehttp-${APPS_VSN:?}/ebin

! erl -boot no_dot_erlang -sasl errlog_type error -noshell -pa "$LIBPATH1" -pa "$LIBPATH2" -pa "$LIBPATH3" -eval "$CODE"

RC=$?
if [ $RC -eq 0 ]; then
    exit 1
fi

exit 0
