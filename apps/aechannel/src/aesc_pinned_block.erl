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
        , block_hash/2
        , serialize/1
        , deserialize/1
        , serialize_for_client/1
        ]).

-define(KEYBLOCK,   1).
-define(MICROBLOCK, 2).
-record(pinned, {
          hash :: aec_blocks:block_header_hash(),
          type :: key | micro 
         }).

-define(VSN, 1).
-define(NO_PINNED_BLOCK, <<>>).
-define(BINARY_NO_PINNED_BLOCK, <<>>).
-opaque hash() :: ?NO_PINNED_BLOCK | #pinned{}.
-export_type([hash/0]).

-spec no_hash() -> hash().
no_hash() -> ?NO_PINNED_BLOCK.

-spec block_hash(aec_blocks:block_header_hash(), key | micro) -> hash().
block_hash(BH, Type) when Type =:= key;
                          Type =:= micro ->
    #pinned{hash = BH, type = Type}.

-spec serialize(hash()) -> binary().
serialize(?NO_PINNED_BLOCK) -> ?BINARY_NO_PINNED_BLOCK;
serialize(#pinned{hash = BH, type = Type}) ->
    Vsn = ?VSN,
    aeser_chain_objects:serialize(
      pinned_block,
      Vsn,
      serialization_template(Vsn),
      [{hash, BH},
       {type, type_to_int(Type)}]).

-spec deserialize(binary()) -> hash().
deserialize(?BINARY_NO_PINNED_BLOCK) -> ?NO_PINNED_BLOCK;
deserialize(Bin) ->
    {pinned_block, ?VSN, RawFields} =
        aeser_chain_objects:deserialize_type_and_vsn(Bin),
    [{hash, BH}, {type, Type}]
        = aeserialization:decode_fields(serialization_template(?VSN), RawFields),
    #pinned{hash = BH, type = int_to_type(Type)}.

serialization_template(?VSN) ->
    [{hash, binary},
     {type, int}].

type_to_int(key)   -> ?KEYBLOCK;
type_to_int(micro) -> ?MICROBLOCK.

int_to_type(?KEYBLOCK)   -> key;
int_to_type(?MICROBLOCK) -> micro.

-spec serialize_for_client(hash()) -> binary().
serialize_for_client(?NO_PINNED_BLOCK) -> <<>>;
serialize_for_client(#pinned{hash = BH, type = Type}) ->
    AESERType =
        case Type of
            key -> key_block_hash;
            micro -> micro_block_hash
        end,
    aeser_api_encoder:encode(AESERType, BH).

