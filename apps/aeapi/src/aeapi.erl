%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity Crypto Foundation
%%% @doc Provide a common API to parts of the system
%%% external interfaces interact with including
%%% the various JSON interfaces, CLI and GUI.
%%%
%%% Brings together in one place a useful subset of internal APIs for looking
%%% up blocks, accounts etc, and provides routines to convert between internal 
%%% and internal forms of hashes, keys etc.
%%% @end
%%%-------------------------------------------------------------------
-module(aeapi).

-export([
         blockchain_name/0
        , network_id/0
        , node_revision/0
        , node_version/0
        , sync_progress/0
        , connected_peers/0

        %% API to blocks
        , top_key_block/0
        , block_height/1
        , block_time_in_msecs/1
        , key_block_by_height/1
        , key_block_by_hash/1
        , key_block_txs/1
        , micro_blocks_at_key_block/1
        , micro_block_txs/1
        , prev_key_block/1
        , prev_block/1
        , generation_by_height/1

        , balance_at_height/2
        , balance_at_block/2

        , oracle_at_block/2
        , oracle_at_height/2
        , oracle_queries_at_block/5
        , oracle_queries_at_height/5
        , oracle_query_at_block/3
        , oracle_query_at_height/3

        %% Dealing with id records
        , create_id/2
        , id_type/1
        , id_value/1
        , format_id/1

        %% Formatting and decoding hashes and keys
        , format/2
        , decode/1
        , safe_decode/2
        ]).

blockchain_name() ->
    <<"aeternity">>. %% TODO: check hardcoding

%% @doc Retrieve the configured network id of this node.
%% e.g.
%% ```aeapi:network_id() -> <<"ae_mainnet">>'''
-spec network_id() -> binary().
network_id() ->
    aec_governance:get_network_id().

%% @doc Retrieve the git sha1 of the code version running in this node.
%% e.g.
%% ```aeapi:node_revision() -> <<"6051fc946d17e276b1b1dbe4436b5fd52ea5ae0a">>'''
-spec node_revision() -> binary().
node_revision() ->
    aeu_info:get_revision().

%% @doc Retrieve the code version running in this node.
%% e.g.
%% ```aeapi:node_revision() -> <<"6.4.0+46.6051fc94">>'''
-spec node_version() -> binary().
node_version() ->
    aeu_info:get_version().

%% @doc Retrieve the keyblock that is currently
%% accumulating microblocks at its height.
-spec top_key_block() -> 'error' | {ok, aec_blocks:block()}.
top_key_block() ->
    aec_chain:top_key_block().

%% @doc Get the current sync status, percent complete and height reached.
-spec sync_progress() -> {boolean(), float(), aec_blocks:height()}.
sync_progress() ->
    aec_sync:sync_progress().

connected_peers() ->
    aec_peers:connected_peers().

prev_key_block(Block) ->
    PrevBlockHash = aec_blocks:prev_key_hash(Block),
    Height = aec_blocks:height(Block),
    case aec_chain:get_block(PrevBlockHash) of
        {ok, _} = PrevBlock ->
            PrevBlock;
        error when Height == 0 ->
            {ok, Block};
        error ->
            {error, block_not_found}
    end.

prev_block(Block) ->
    PrevHash = aec_blocks:prev_hash(Block),
    case aec_chain:get_block(PrevHash) of
        {ok, _} = PrevBlock ->
            PrevBlock;
        error ->
            {error, block_not_found}
    end.

block_height(Block) ->
    aec_blocks:height(Block).

block_time_in_msecs(Block) ->
    aec_blocks:time_in_msecs(Block).

micro_blocks_at_key_block(KeyBlock) ->
    {ok, BlockHash} = aec_headers:hash_header(aec_blocks:to_key_header(KeyBlock)),
    case aec_chain:get_generation_by_hash(BlockHash, forward) of
        {ok, #{micro_blocks := MicroBlocks}} ->
            {ok, MicroBlocks};
        error ->
            error
    end.

key_block_txs(Block) ->
    {ok, BlockHash} = aec_headers:hash_header(aec_blocks:to_key_header(Block)),
    case aec_chain:get_generation_by_hash(BlockHash, forward) of
        {ok, #{micro_blocks := MicroBlocks}} ->
            Txs = lists:foldl(
              fun(MicBlock, Acc) ->
                      [aec_blocks:txs(MicBlock) | Acc]
              end, [], MicroBlocks),
            lists:flatten(lists:reverse(Txs));
        error ->
            error
    end.

micro_block_txs(MicroBlock) ->
    aec_blocks:txs(MicroBlock).

%%
%% Converting Ids between internal and printable forms
%%

%% @doc Convert a formatted pubkey to an internal #id{} record.
%% Ids are supported for these types: account_pubkey | oracle_pubkey | name
%% | commitment | contract_pubkey | channel
%%
%% For example:
%% ```create_id(<<"ak_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>, [account_pubkey]) ->
%%    {ok, #id{tag = account,
%%             val = <<170,20,197,4,89,131,91,158,24,63,28,200,44,49,35,88,224,211,211,30,
%%                     29,108,91,130,93,230,47,111,34,172,124,50>>}}'''
-spec create_id(aeser_api_encoder:encoded(),
                [account_pubkey | oracle_pubkey | name 
                 | commitment | contract_pubkey
                 | channel]) -> {'ok', aeser_id:id()} | {'error', atom()}.
create_id(Binary, AllowedTypes) ->
    aeser_api_encoder:safe_decode({id_hash, AllowedTypes}, Binary).

%% @doc Get the internal type of an Id.
%% Note that this is not the same as the type outside of the id realm
%%
%% For example:
%% ``id_type(Id :: aeser_id:id()) -> account.''
-spec id_type(aeser_id:id()) -> aeser_id:tag().
id_type(Id) ->
    case aeser_id:is_id(Id) of
        true ->
            aeser_id:specialize_type(Id);
        false ->
            {error, not_id}
    end.

%% @doc Get the internal value of an Id.
%% Example:
%% ```id_value(Id :: aeser_id:id()) ->
%%            <<170,20,197,4,89,131,91,158,24,63,28,200,44,49,35,88,224,211,211,30,
%%              29,108,91,130,93,230,47,111,34,172,124,50>>.'''
id_value(Id) ->
    case aeser_id:is_id(Id) of
        true ->
            {_Tag, Val} = aeser_id:specialize(Id),
            Val;
        false ->
            {error, not_id}
    end.

%% @doc Format an aeser_id:id() to its printable / external form.
%% Example:
%% ```format_id(Id :: aeser_id:id()) -> <<"ak_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>.'''
-spec format_id(aeser_id:id()) -> binary().
format_id(Id) ->
     case aeser_id:is_id(Id) of
        true ->
            aeser_api_encoder:encode(id_hash, Id);
        false ->
            {error, not_id}
    end.

%% @doc Format internal hash and key types to their external / printable form.
%% Takes the internal binary form of the key/hash and formats it
%% into the printable form of the specified type.
%%
%% For example an internal pubkey might be formatted as either a
%% normal account or oracle depending on context.
%%
%% Format as normal account:
%% ```aeapi:format(account_pubkey, <<170,20,197,4,89,131,91,158,24,63,28,200,44,49,35,88,224,211,211,30,
%%                     29,108,91,130,93,230,47,111,34,172,124,50>>) ->
%%      <<"ak_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>.'''
%%
%% Format as an Oracle linked to the same account:
%% ```aeapi:format(oracle_pubkey, <<170,20,197,4,89,131,91,158,24,63,28,200,44,49,35,88,224,211,211,30,
%%                     29,108,91,130,93,230,47,111,34,172,124,50>>) ->
%%      <<"ok_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>.'''
-spec format(aeser_api_encoder:known_type(), aeser_api_encoder:payload()) -> aeser_api_encoder:encoded().
format(Type, Payload) ->
    aeser_api_encoder:encode(Type, Payload).

%% @doc Decode the external / printable form of a key / hash.
%% Returns its type and internal binary value.
%%
%% For example:
%% ```aeapi:decode(<<"ct_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>) ->
%%       {contract_pubkey,<<170,20,197,4,89,131,91,158,24,63,28,
%%                  200,44,49,35,88,224,211,211,30,29,108,
%%                   91,130,93,230,47,...>>}'''
-spec decode(binary()) -> {aeser_api_encoder:known_type(), aeser_api_encoder:payload()}.
decode(Binary) ->
    aeser_api_encoder:decode(Binary).

%% @doc Decode the external / printable form of a key / hash, returning an error it is not of the specified type.
%% Returns the internal binary value or {error,Reason}
%%
%% For example:
%% ```aeapi:safe_decode(contract_pubkey, <<"ct_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>) ->
%%       {ok, <<170,20,197,4,89,131,91,158,24,63,28,
%%                  200,44,49,35,88,224,211,211,30,29,108,
%%                   91,130,93,230,47,...>>}'''
-spec safe_decode(aeser_api_encoder:known_type() | block_hash,
                  aeser_api_encoder:encoded()) -> {'ok', aeser_api_encoder:payload()}
                                                     | {'error', atom()}.
safe_decode(Type, Binary) ->
    aeser_api_encoder:safe_decode(Type, Binary).

-spec key_block_by_height(aec_blocks:height()) -> {ok, aec_blocks:key_block()} | error.

key_block_by_height(Height) when is_integer(Height) ->
    aec_chain:get_key_block_by_height(Height).

-spec key_block_by_hash(aeser_api_encoder:encoded()) -> {ok, aec_blocks:key_block()} | error.
key_block_by_hash(Hash) when is_binary(Hash) ->
    {key_block_hash, DecodedHash} = aeser_api_encoder:decode(Hash),
    aec_chain:get_block(DecodedHash).

generation_by_height(Height) when is_integer(Height) ->
    case aec_chain_state:get_key_block_hash_at_height(Height) of
        error -> {error, "Block not found"};
        {ok, Hash} ->
            case aec_chain:get_generation_by_hash(Hash, forward) of
                {ok, #{ key_block := KeyBlock, micro_blocks := MicroBlocks }} ->
                    case aec_blocks:height(KeyBlock) of
                        0 ->
                            {ok, encode_generation(KeyBlock, MicroBlocks, key)};
                        _ ->
                            PrevBlockHash = aec_blocks:prev_hash(KeyBlock),
                            case aec_chain:get_block(PrevBlockHash) of
                                {ok, PrevBlock} ->
                                    PrevBlockType = aec_blocks:type(PrevBlock),
                                    {ok, encode_generation(KeyBlock, MicroBlocks, PrevBlockType)};
                                error ->
                                    {error, "Block not found"}
                            end
                    end;
                _ ->
                    {error, "Block not found"}
            end
    end.

encode_generation(KeyBlock, MicroBlocks, PrevBlockType) ->
    Header = aec_blocks:to_header(KeyBlock),
    #{<<"key_block">> => aec_headers:serialize_for_client(Header, PrevBlockType),
      <<"micro_blocks">> => [begin
                           {ok, Hash} = aec_blocks:hash_internal_representation(M),
                           aeser_api_encoder:encode(micro_block_hash, Hash)
                       end || M <- MicroBlocks]}.

%% @doc Lookup the opening balance of an account or contract at the keyblock with the given height.
%% Example:
%% ```balance_at_height(<<"ak_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>, ) -> {ok, 200}.'''
balance_at_height(Address, Height) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    case create_id(Address, AllowedTypes) of
        {ok, Id} ->
            PubKey = aeapi:id_value(Id),
            {value, Account} = aec_chain:get_account_at_height(PubKey, Height),
            {ok, aec_accounts:balance(Account)};
        _ ->
            {error, invalid_pubkey}
    end.

balance_at_block(AccountAddress, BlockHash) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    {ok, Id} = create_id(AccountAddress, AllowedTypes),
    PubKey = aeapi:id_value(Id),
    {_BlockType, Hash} = aeser_api_encoder:decode(BlockHash),
    {value, Account} = aec_chain:get_account_at_hash(PubKey, Hash),
    {ok, aec_accounts:balance(Account)}.

oracle_at_height(OracleAddress, Height) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleAddress),
    {ok, KeyBlock} = key_block_by_height(Height),
    Header = aec_blocks:to_header(KeyBlock),
    {ok, Hash} = aec_headers:hash_header(Header),
    aec_chain:get_oracle_at_hash(OraclePubkey, Hash).

oracle_at_block(OracleAddress, BlockHash) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleAddress),
    {_BlockType, Hash} = aeser_api_encoder:decode(BlockHash),
    aec_chain:get_oracle_at_hash(OraclePubkey, Hash).

oracle_query_at_height(OracleAddress, QueryId, Height) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleAddress),
    {oracle_query_id, Query} = decode(QueryId),
    {ok, KeyBlock} = key_block_by_height(Height),
    Header = aec_blocks:to_header(KeyBlock),
    {ok, Hash} = aec_headers:hash_header(Header),
    get_oracle_query_at_hash(OraclePubkey, Query, Hash).

oracle_query_at_block(OracleAddress, QueryId, BlockHash) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleAddress),
    {oracle_query_id, Query} = decode(QueryId),
    {_BlockType, Hash} = aeser_api_encoder:decode(BlockHash),
    get_oracle_query_at_hash(OraclePubkey, Query, Hash).

get_oracle_query_at_hash(OraclePubkey, Query, Hash) ->
    case aec_chain:get_oracle_query_at_hash(OraclePubkey, Query, Hash) of
        {ok, Q} ->
            {ok, aeo_query:serialize_for_client(Q)};
        Else ->
            Else
    end.

oracle_queries_at_height(OracleAddress, From, QueryType, Max, Height) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleAddress),
    {ok, KeyBlock} = key_block_by_height(Height),
    Header = aec_blocks:to_header(KeyBlock),
    {ok, Hash} = aec_headers:hash_header(Header),
    aec_chain:get_oracle_queries_at_hash(OraclePubkey, From, QueryType, Max, Hash).

-spec oracle_queries_at_block(aec_keys:pubkey(), binary() | '$first', open | closed | all, non_neg_integer(), aeser_api_encoder:encoded()) ->
    {ok, [aeo_query:query()]} | {error, no_state_trees}.
oracle_queries_at_block(OracleAddress, From, QueryType, Max, BlockHash) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleAddress),
    {_BlockType, Hash} = aeser_api_encoder:decode(BlockHash),
    aec_chain:get_oracle_queries_at_hash(OraclePubkey, From, QueryType, Max, Hash).