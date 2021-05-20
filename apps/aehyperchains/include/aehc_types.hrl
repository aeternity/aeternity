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

-define(COMMITER_PUB_BYTES, 32).
-define(POGF_HASH_BYTES, 32).
-define(COMMITMENT_HASH_BYTES, 32).

-type commiter_pubkey() :: <<_:(?COMMITER_PUB_BYTES*8)>>.
-type pogf_hash() :: <<_:(?POGF_HASH_BYTES*8)>>.
-type commitment_hash() :: <<_:(?COMMITMENT_HASH_BYTES*8)>>.

-type pubkey() :: aec_keys:pubkey().
-type hash() :: aec_hash:hash().
-type signature() :: <<_:512>>. %% 64 bytes
-type header() :: aec_headers:header().
-type block() :: aec_blocks:block().
-type chain_node() :: #chain_node{}. %%TODO Factor 'chain_node' into separate module.
-type trees() :: aec_trees:trees().
-type env() :: aetx_env:env().
-type amount() :: non_neg_integer().
-type nonce() :: non_neg_integer().
-type account() :: aec_accounts:account().
-type query() :: binary() | string().
