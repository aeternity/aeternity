%%%=============================================================================
%%% @copyright 2019, Aeternity Anstalt
%%% @doc
%%%   Module defining State Channel's pinned block hash. It sets the on-chain
%%%   environment the updates - either off-chain of force progress are
%%%   executed
%%% @end
%%%=============================================================================
-module(aesc_pinned_block).

-export([ no_hash/0
        , block_hash/1
        , serialize/1
        , deserialize/1
        %, serialize_for_client/1 TODO: expose key and micro hash serialization
        ]).

-define(NO_PINNED_BLOCK, <<>>).
-opaque hash() :: ?NO_PINNED_BLOCK | aec_blocks:block_header_hash().
-export_type([hash/0]).

-spec no_hash() -> hash().
no_hash() -> ?NO_PINNED_BLOCK.

-spec block_hash(aec_blocks:block_header_hash()) -> hash().
block_hash(BH) -> BH.

serialize(BH) -> BH.

deserialize(BH) -> BH.

