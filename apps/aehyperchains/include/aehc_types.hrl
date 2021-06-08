%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-

-include_lib("aecore/include/blocks.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(STAKING_CONTRACT, {?MODULE, staking_contract}).
-define(STAKING_CONTRACT_ADDR, {?MODULE, staking_contract_addr}).
%% Lima or Iris as we need the FATE VM at genesis
%% In case that's unwanted then start up another consensus before hyperchains
-define(HC_GENESIS_VERSION, ?LIMA_PROTOCOL_VSN).

%% Magic nonces
-define(NONCE_HC_ENABLED, 16#ffffffffffffffff - 1).
-define(NONCE_HC_POGF, 16#ffffffffffffffff).
-define(NONCE_HC_GENESIS, 2). %% Hyperchain at genesis :)

-define(COMMITTER_PUB_BYTES, 32).
-define(POGF_HASH_BYTES, 32).
-define(COMMITMENT_HASH_BYTES, 32).

-type committer_pubkey() :: <<_:(?COMMITTER_PUB_BYTES*8)>>.
-type pogf_hash() :: <<_:(?POGF_HASH_BYTES*8)>>.
-type commitment_hash() :: <<_:(?COMMITMENT_HASH_BYTES*8)>>.

-type hash() :: aec_hash:hash().
-type header() :: aec_headers:header().
