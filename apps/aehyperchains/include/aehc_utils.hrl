%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-

-define(COMMITER_PUB_BYTES, 32).
-define(POGF_HASH_BYTES, 32).
-define(COMMITMENT_HASH_BYTES, 32).

-type(commiter_pubkey() :: <<_:(?COMMITER_PUB_BYTES*8)>>).
-type(pogf_hash() :: <<_:(?POGF_HASH_BYTES*8)>>).
-type(commitment_hash() :: <<_:(?COMMITMENT_HASH_BYTES*8)>>).
