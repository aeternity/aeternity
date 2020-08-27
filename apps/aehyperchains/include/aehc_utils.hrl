%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-

-define(COMMITER_PUB_BYTES, 32).

-type(commiter_pubkey() :: <<_:(?COMMITER_PUB_BYTES*8)>>).
