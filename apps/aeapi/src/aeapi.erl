%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity Crypto Foundation
%%% @doc Provide a common API to parts of the system
%%% external interfaces interact with including
%%% the various JSON interfaces, CLI and GUI.
%%%
%%% Brings together in one place a useful subset of internal APIs for looking
%%% up blocks, accounts etc, and provides routines to convert between internal 
%%% and internal forms of hashes, keys etc.
%%%
%%% TODOs (2023-06-05 zxq9 <ceverett@tsuriai.jp>):
%%% - Possibly re-think naming conventions.
%%% - Add specific error values for garbage collection cases.
%%% - Decide whether three modules might make more sense:
%%%     1. Retrieval from key values (hashes, heights, names, etc.)
%%%     2. Block inspection
%%%     3. Encoding/Decoding (that is, human/JSON-friendly VS internal forms)
%%% @end
%%%-------------------------------------------------------------------
-module(aeapi).

-include_lib("aecontract/include/hard_forks.hrl").

%% Node Info API
-export([blockchain_name/0,
         network_id/0,
         node_revision/0,
         node_version/0,
         sync_progress/0,
         connected_peers/0]).

%% Chain Data API
-export([current_block_height/0,
         top_block/0,
         top_block_header/0,
         top_block_hash/0,
         top_key_block/0,
         top_key_block_header/0,
         top_key_block_hash/0,
         key_block_at_height/1,
         key_block_at_hash/1,
         key_block_txs/1,
         prev_key_block/1,
         prev_block/1,
         micro_blocks_at_key_block/1,
         generation_at_height/1,
         balance_at_height/2,
         balance_at_block/2]).

%% Block Inspection API
-export([block_height/1,
         block_time_in_msecs/1,
         micro_block_txs/1,

         balance_change_events_in_tx/2,
         balance_change_events_in_block/1,
         next_nonce/1,

         oracle_at_block/2,
         oracle_at_height/2,
         oracle_queries_at_block/5,
         oracle_queries_at_height/5,
         oracle_query_at_block/3,
         oracle_query_at_height/3,

         %% Dealing with id records
         create_id/2,
         id_type/1,
         id_value/1,
         format_id/1,

         %% Formatting and decoding hashes and keys
         format_key_block_hash/1,
         format_micro_block_hash/1,
         format_block_pof_hash/1,
         format_block_tx_hash/1,
         format_block_state_hash/1,
         format_channel/1,
         format_contract_bytearray/1,
         format_contract_pubkey/1,
         format_contract_store_key/1,
         format_transaction/1,
         format_tx_hash/1,
         format_oracle_pubkey/1,
         format_oracle_query/1,
         format_oracle_query_id/1,
         format_oracle_response/1,
         format_account_pubkey/1,
         format_signature/1,
         format_name/1,
         format_commitment/1,
         format_peer_pubkey/1,
         format_state/1,
         format_poi/1,
         format_state_trees/1,
         format_call_state_tree/1,
         format_bytearray/1,

         decode/1,
         decode_key_block_hash/1,
         decode_micro_block_hash/1,
         decode_block_pof_hash/1,
         decode_block_tx_hash/1,
         decode_block_state_hash/1,
         decode_channel/1,
         decode_contract_bytearray/1,
         decode_contract_pubkey/1,
         decode_contract_store_key/1,
         decode_transaction/1,
         decode_tx_hash/1,
         decode_oracle_pubkey/1,
         decode_oracle_query/1,
         decode_oracle_query_id/1,
         decode_oracle_response/1,
         decode_account_pubkey/1,
         decode_signature/1,
         decode_name/1,
         decode_commitment/1,
         decode_peer_pubkey/1,
         decode_state/1,
         decode_poi/1,
         decode_state_trees/1,
         decode_call_state_tree/1,
         decode_bytearray/1]).



%%% Node Info API

-spec blockchain_name() -> Name
    when Name :: binary().
%% @doc
%% Return the name of the chain served by this node as a binary string.
%% e.g.
%% ```aeapi:blockchain_name() -> <<"aeternity">>'''

blockchain_name() ->
    %% TODO: Check whether this should be hardcoded or retrieved from settings (?)
    <<"aeternity">>.


-spec network_id() -> binary().
%% @doc
%% Retrieve the configured network id of this node.
%% e.g.
%% ```aeapi:network_id() -> <<"ae_mainnet">>'''

network_id() ->
    aec_governance:get_network_id().


-spec node_revision() -> binary().
%% @doc
%% Retrieve the git sha1 of the code version running in this node.
%% e.g.
%% ```aeapi:node_revision() -> <<"6051fc946d17e276b1b1dbe4436b5fd52ea5ae0a">>'''

node_revision() ->
    aeu_info:get_revision().


-spec node_version() -> binary().
%% @doc
%% Retrieve the code version running in this node.
%% e.g.
%% ```aeapi:node_version() -> <<"6.4.0+46.6051fc94">>'''

node_version() ->
    aeu_info:get_version().


-spec sync_progress() -> {boolean(), float(), aec_blocks:height()}.
%% @doc
%% Get the current sync status, percent complete and height reached.

sync_progress() ->
    aec_sync:sync_progress().


-spec connected_peers() -> [Peer]
    when Peer :: #{host   => binary(),           % e.g. <<"1.2.3.4">>
                   port   => inet:port_number(),
                   pubkey => binary()}.
%% @doc
%% Retrieve the list of currently connected peers formatted as annoyingly
%% structured maps. This list is flat and does not indicate whether a given
%% peer is an inbound or outbound connection.

connected_peers() ->
    aec_peers:connected_peers().


%%% Chain Data API

-spec current_block_height() -> error | {ok, integer()}.
%% @doc
%% Retrieve the current top block height

current_block_height() ->
    case aec_chain:top_block() of
        undefined -> error;
        Block     -> {ok, aec_blocks:height(Block)}
    end.


-spec top_block() -> error | {ok, aec_blocks:block()}.
%% @doc
%% Retrieve the top block (whether it is a key or micro block).

top_block() ->
    case aec_chain:top_block() of
        undefined -> error;
        Block     -> {ok, Block}
    end.


-spec top_block_header() -> error | {ok, aec_headers:header()}.
%% @doc
%% Retrieve the current top block header.

top_block_header() ->
    case aec_chain:top_block() of
        undefined -> error;
        Block     -> {ok, aec_blocks:to_header(Block)}
    end.


-spec top_block_hash() -> error | {ok, binary()}.
%% @doc
%% Rerieve the current top block hash.

top_block_hash() ->
    case aec_chain:top_block_hash() of
        undefined -> error;
        Hash      -> {ok, Hash}
    end.


-spec top_key_block() -> error | {ok, aec_blocks:key_block()}.
%% @doc
%% Retrieve the current top keyblock.

top_key_block() ->
    aec_chain:top_key_block().


-spec top_key_block_header() -> error | {ok, aec_headers:header()}.
%% @doc
%% Retrieve the current top keyblock header.

top_key_block_header() ->
    case aec_chain:top_key_block() of
        {ok, Block} -> {ok, aec_blocks:to_header(Block)};
        error       -> error
    end.

-spec top_key_block_hash() -> error | {ok, binary()}.
%% @doc
%% Retrieve the top key block hash.

top_key_block_hash() ->
    case aec_chain:top_key_block_hash() of
        undefined -> error;
        Hash      -> {ok, Hash}
    end.


-spec key_block_at_height(Height) -> Result
    when Height :: aec_blocks:height(),
         Result :: {ok, aec_blocks:key_block()}
                 | {error, chain_too_short}
                 | error.
%% @doc
%% Retrieve the keyblock that applies to a given height.

key_block_at_height(Height) when is_integer(Height) ->
    aec_chain:get_key_block_by_height(Height).


-spec key_block_at_hash(Hash) -> Result
    when Hash   :: aeser_api_encoder:encoded(),
         Result :: {ok, aec_blocks:key_block()}
                 | error.
%% @doc
%% Retrieve the keyblock that applies to a given block hash.

key_block_at_hash(Hash) when is_binary(Hash) ->
    {key_block_hash, DecodedHash} = aeser_api_encoder:decode(Hash),
    aec_chain:get_block(DecodedHash).


-spec key_block_txs(Block) -> Result
    when Block  :: aec_blocks:block(),
         Result :: [aetx_sign:signed_tx()].
%% @doc
%% Given a keyblock, retrieve the signed transactions contained in its generation.

key_block_txs(Block) ->
    {ok, BlockHash} = aec_headers:hash_header(aec_blocks:to_key_header(Block)),
    case aec_chain:get_generation_by_hash(BlockHash, forward) of
        {ok, #{micro_blocks := MBs}} ->
            lists:flatten(lists:map(fun aec_blocks:txs/1, MBs));
        error ->
            error
    end.


-spec prev_key_block(Block) -> Result
    when Block  :: aec_blocks:block(),
         Result :: {ok, aec_blocks:key_block()}
                 | error.
%% @doc
%% Given a block, retrieve the previous keyblock.

prev_key_block(Block) ->
    case aec_blocks:height(Block) =:= 0 of
        false ->
            PrevBlockHash = aec_blocks:prev_key_hash(Block),
            aec_chain:get_block(PrevBlockHash);
        true ->
            {ok, Block}
    end.


-spec prev_block(Block) -> Result
    when Block  :: aec_blocks:block(),
         Result :: {ok, aec_blocks:block()}
                 | {error, genesis}
                 | error.
%% @doc
%% Given a block, retrieve the previous block.

prev_block(Block) ->
    case aec_blocks:height(Block) =:= 0 of
        false ->
            PrevHash = aec_blocks:prev_hash(Block),
            aec_chain:get_block(PrevHash);
        true ->
            {error, genesis}
    end.


-spec micro_blocks_at_key_block(KeyBlock) -> Result
    when KeyBlock :: aec_blocks:key_block(),
         Result   :: {ok, [aec_blocks:micro_block()]}
                   | error.
%% @doc
%% Given a key block, retrieve the microblocks included in its generation.

micro_blocks_at_key_block(KeyBlock) ->
    {ok, BlockHash} = aec_headers:hash_header(aec_blocks:to_key_header(KeyBlock)),
    case aec_chain:get_generation_by_hash(BlockHash, forward) of
        {ok, #{micro_blocks := MicroBlocks}} ->
            {ok, MicroBlocks};
        error ->
            error
    end.


-spec generation_at_height(Height) -> Result
    when Height :: aec_blocks:height(),
         Result :: {ok, EncodedGeneration :: map()}
                 | error.
%% @doc
%% Retrieve the generation at the given height.
%% The encoded generation will appear as a map of the form
%% ```
%%  #{<<"key_block">>    => KeyBlock,
%%    <<"micro_blocks">> => MicroBlockHashes}
%% '''
%% where `KeyBlock' is the form returned by `aec_headers:serialize_for_client/1'
%% and `MicroBlockHashes' are microblock hashes encoded in the `<<"mh_...">>'
%% external form.

generation_at_height(Height) when is_integer(Height) ->
    case aec_chain_state:get_key_block_hash_at_height(Height) of
        {ok, Hash} ->
            case aec_chain:get_generation_by_hash(Hash, forward) of
                {ok, #{key_block := KeyBlock, micro_blocks := MicroBlocks}} ->
                    {ok, encode_generation(KeyBlock, MicroBlocks)};
                error ->
                    error
            end;
        error ->
            error
    end.


-spec balance_at_height(Address, Height) -> Result
    when Address :: binary(),
         Height  :: aec_blocks:height(),
         Result  :: {ok, Balance :: non_neg_integer()}
                  | {error, Reason},
         Reason  :: invalid_pubkey
                  | invalid_encoding
                  | invalid_prefix
                  | chain_too_short
                  | garbage_collected
                  | account_not_found
                  | contract_not_found.

%% @doc Lookup the opening balance of an account or contract at the keyblock with
%% the given height.
%% Example (on testnet, AKA the "ae_uat" chain):
%% ```
%%   Account = <<"ak_2W2NjVgAo6pkj96R71JHsTK5h2UVvmot7hYrqVfhzAqHGGU5ea">>,
%%   Height = 750000,
%%   balance_at_height(Account, Height) -> {ok, 9999346828000000000}.
%% '''

balance_at_height(Address, Height) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    case create_id(Address, AllowedTypes) of
        {ok, ID} ->
            PubKey = id_value(ID),
            balance_at_height(element(2, ID), PubKey, Height);
        Error ->
            Error
    end.

balance_at_height(Type, PubKey, Height) ->
    case aec_chain:get_account_at_height(PubKey, Height) of
        {value, Account} ->
            {ok, aec_accounts:balance(Account)};
        none ->
            case Type of
                account  -> {error, account_not_found};
                contract -> {error, contract_not_found}
            end;
        Error ->
            Error
    end.


-spec balance_at_block(Address, BlockHash) -> Result
    when Address   :: binary(),
         BlockHash :: binary(),
         Result    :: {ok, Balance :: non_neg_integer()}
                    | {error, Reason},
         Reason    :: invalid_block_hash
                    | invalid_address
                    | block_not_found
                    | garbage_collected
                    | account_not_found
                    | contract_not_found.
%% @doc
%% Return the given account's balance at the height of the block ID provided.

balance_at_block(Address, BlockHash) ->
    case decode_catcher(BlockHash) of
        {ok, Hash} -> balance_at_block2(Address, Hash);
        Error      -> Error
    end.

balance_at_block2(Address, Hash) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    case create_id(Address, AllowedTypes) of
        {ok, ID} ->
            PubKey = id_value(ID),
            balance_at_block3(element(2, ID), PubKey, Hash);
        {error, invalid_encoding} ->
            {error, invalid_address}
    end.

balance_at_block3(Type, PubKey, Hash) ->
    case aec_db:get_block(Hash) of
        undefined ->
            {error, block_not_found};
        Block ->
            Height = aec_blocks:height(Block),
            balance_at_height(Type, PubKey, Height)
    end.

decode_catcher(BlockHash) ->
    try
        case aeser_api_encoder:decode(BlockHash) of
            {micro_block_hash, Hash} -> {ok, Hash};
            {key_block_hash, Hash}   -> {ok, Hash};
            _                        -> {error, invalid_block_hash}
        end
    catch
        error -> {error, invalid_block_hash}
    end.



%%% Block Inspection API

-spec block_height(Block) -> Height
    when Block  :: aec_blocks:block(),
         Height :: aec_blocks:height().
%% @doc
%% Given a block, return its height as an integer.

block_height(Block) ->
    aec_blocks:height(Block).


-spec block_time_in_msecs(Block) -> TimeMS
    when Block  :: aec_blocks:block(),
         TimeMS :: non_neg_integer().
%% @doc
%% Given a block, return its timestamp in milliseconds.

block_time_in_msecs(Block) ->
    aec_blocks:time_in_msecs(Block).


-spec micro_block_txs(Block) -> TXs
    when Block :: aec_blocks:micro_block(),
         TXs   :: [aetx_sign:signed_tx()].
%% @doc
%% Given a microblock, extract the signed transactions and return them.

micro_block_txs(MicroBlock) ->
    aec_blocks:txs(MicroBlock).

%% HERE

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
%% ```aeapi:format_account_pubkey(<<170,20,197,4,89,131,91,158,24,63,28,200,44,49,35,88,224,211,211,30,
%%                     29,108,91,130,93,230,47,111,34,172,124,50>>) ->
%%      <<"ak_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>.'''
%%
%% Format as an Oracle linked to the same account:
%% ```aeapi:format_oracle_pubkey(<<170,20,197,4,89,131,91,158,24,63,28,200,44,49,35,88,224,211,211,30,
%%                     29,108,91,130,93,230,47,111,34,172,124,50>>) ->
%%      <<"ok_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>.'''
format_key_block_hash(Payload) ->
    aeser_api_encoder:encode(key_block_hash, Payload).
format_micro_block_hash(Payload) ->
    aeser_api_encoder:encode(micro_block_hash, Payload).
format_block_pof_hash(Payload) ->
    aeser_api_encoder:encode(block_pof_hash, Payload).
format_block_tx_hash(Payload) ->
    aeser_api_encoder:encode(block_tx_hash, Payload).
format_block_state_hash(Payload) ->
    aeser_api_encoder:encode(block_state_hash, Payload).
format_channel(Payload) ->
    aeser_api_encoder:encode(channel, Payload).
format_contract_bytearray(Payload) ->
    aeser_api_encoder:encode(contract_bytearray, Payload).
format_contract_pubkey(Payload) ->
    aeser_api_encoder:encode(contract_pubkey, Payload).
format_contract_store_key(Payload) ->
    aeser_api_encoder:encode(contract_store_key, Payload).
format_transaction(Payload) ->
    aeser_api_encoder:encode(transaction, Payload).
format_tx_hash(Payload) ->
    aeser_api_encoder:encode(tx_hash, Payload).
format_oracle_pubkey(Payload) ->
    aeser_api_encoder:encode(oracle_pubkey, Payload).
format_oracle_query(Payload) ->
    aeser_api_encoder:encode(oracle_query, Payload).
format_oracle_query_id(Payload) ->
    aeser_api_encoder:encode(oracle_query_id, Payload).
format_oracle_response(Payload) ->
    aeser_api_encoder:encode(oracle_response, Payload).
format_account_pubkey(Payload) ->
    aeser_api_encoder:encode(account_pubkey, Payload).
format_signature(Payload) ->
    aeser_api_encoder:encode(signature, Payload).
format_name(Payload) ->
    aeser_api_encoder:encode(name, Payload).
format_commitment(Payload) ->
    aeser_api_encoder:encode(commitment, Payload).
format_peer_pubkey(Payload) ->
    aeser_api_encoder:encode(peer_pubkey, Payload).
format_state(Payload) ->
    aeser_api_encoder:encode(state, Payload).
format_poi(Payload) ->
    aeser_api_encoder:encode(poi, Payload).
format_state_trees(Payload) ->
    aeser_api_encoder:encode(state_trees, Payload).
format_call_state_tree(Payload) ->
    aeser_api_encoder:encode(call_state_tree, Payload).
format_bytearray(Payload) ->
    aeser_api_encoder:encode(bytearray, Payload).

%% @doc Decode the external / printable form of a key / hash.
%% Returns its type and internal binary value.
%%
%% For example:
%% ```aeapi:decode(<<"ct_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>) ->
%%       {contract_pubkey,<<170,20,197,4,89,131,91,158,24,63,28,
%%                  200,44,49,35,88,224,211,211,30,29,108,
%%                   91,130,93,230,47,...>>}'''
-spec decode(binary()) -> {atom(), binary()}.
decode(Binary) ->
    aeser_api_encoder:decode(Binary).

%% FIXME: Maybe add variants to decode further - aeapi:decode_contract(<<"cb_..">>, [decompile]).

%% @doc Decode the external / printable form of a key / hash, returning an error it is not of the specified type.
%% Returns the internal binary value or {error,Reason}
%%
%% For example:
%% ```aeapi:decode_contract_pubkey(<<"ct_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>) ->
%%       {ok, <<170,20,197,4,89,131,91,158,24,63,28,
%%                  200,44,49,35,88,224,211,211,30,29,108,
%%                   91,130,93,230,47,...>>}'''
decode_key_block_hash(Binary) ->
    aeser_api_encoder:safe_decode(key_block_hash, Binary).
decode_micro_block_hash(Binary) ->
    aeser_api_encoder:safe_decode(micro_block_hash, Binary).
decode_block_pof_hash(Binary) ->
    aeser_api_encoder:safe_decode(block_pof_hash, Binary).
decode_block_tx_hash(Binary) ->
    aeser_api_encoder:safe_decode(block_tx_hash, Binary).
decode_block_state_hash(Binary) ->
    aeser_api_encoder:safe_decode(block_state_hash, Binary).
decode_channel(Binary) ->
    aeser_api_encoder:safe_decode(channel, Binary).
decode_contract_bytearray(Binary) ->
    aeser_api_encoder:safe_decode(contract_bytearray, Binary).
decode_contract_pubkey(Binary) ->
    aeser_api_encoder:safe_decode(contract_pubkey, Binary).
decode_contract_store_key(Binary) ->
    aeser_api_encoder:safe_decode(contract_store_key, Binary).
decode_transaction(Binary) ->
    aeser_api_encoder:safe_decode(transaction, Binary).
decode_tx_hash(Binary) ->
    aeser_api_encoder:safe_decode(tx_hash, Binary).
decode_oracle_pubkey(Binary) ->
    aeser_api_encoder:safe_decode(oracle_pubkey, Binary).
decode_oracle_query(Binary) ->
    aeser_api_encoder:safe_decode(oracle_query, Binary).
decode_oracle_query_id(Binary) ->
    aeser_api_encoder:safe_decode(oracle_query_id, Binary).
decode_oracle_response(Binary) ->
    aeser_api_encoder:safe_decode(oracle_response, Binary).
decode_account_pubkey(Binary) ->
    aeser_api_encoder:safe_decode(account_pubkey, Binary).
decode_signature(Binary) ->
    aeser_api_encoder:safe_decode(signature, Binary).
decode_name(Binary) ->
    aeser_api_encoder:safe_decode(name, Binary).
decode_commitment(Binary) ->
    aeser_api_encoder:safe_decode(commitment, Binary).
decode_peer_pubkey(Binary) ->
    aeser_api_encoder:safe_decode(peer_pubkey, Binary).
decode_state(Binary) ->
    aeser_api_encoder:safe_decode(state, Binary).
decode_poi(Binary) ->
    aeser_api_encoder:safe_decode(poi, Binary).
decode_state_trees(Binary) ->
    aeser_api_encoder:safe_decode(state_trees, Binary).
decode_call_state_tree(Binary) ->
    aeser_api_encoder:safe_decode(call_state_tree, Binary).
decode_bytearray(Binary) ->
    aeser_api_encoder:safe_decode(bytearray, Binary).


%% Format a single Tx in a block. We need to dry run all the Txs in the microblock
%% up to and including the one we want to know about. dry run works on dummy signed
%% Txs so the tx_hash included in the results is not the same as the one requested.
%% Solve this by pre-filtering, then patching in the correct hash values afterwards.
balance_change_events_in_tx(SignedTx, MicroBlock) ->
    {ok, MBHash} =
        aec_headers:hash_header(
            aec_blocks:to_header(MicroBlock)),
    BlockTxs = micro_block_txs(MicroBlock),
    NeededTxs = keep_tx_until(BlockTxs, aetx_sign:hash(SignedTx)),
    Ops = format_txs(NeededTxs, MBHash),
    SpendOps = tx_spend_operations(Ops),
    lists:last(SpendOps).

keep_tx_until(Txs, TxHash) ->
    keep_tx_until(Txs, TxHash, []).

keep_tx_until([], _UntilTxHash, _Acc) ->
    [];
keep_tx_until([SignedTx | Txs], UntilTxHash, Acc) ->
    TxHash = aetx_sign:hash(SignedTx),
    if TxHash == UntilTxHash ->
           lists:reverse([SignedTx | Acc]);
       true ->
           keep_tx_until(Txs, UntilTxHash, [SignedTx | Acc])
    end.

balance_change_events_in_block(Block) ->
    case aec_blocks:type(Block) of
        micro ->
            BlockTxs = micro_block_txs(Block),
            {ok, MBHash} =
                aec_headers:hash_header(
                    aec_blocks:to_header(Block)),
            Txs = format_txs(BlockTxs, MBHash),
            tx_spend_operations(Txs);
        key ->
            {ok, MicroBlocks} = micro_blocks_at_key_block(Block),

            {BlockTxs, []} = aeu_lib:pmap(
                fun(MicroBlock) ->
                    BlockTxs = micro_block_txs(MicroBlock),
                    {ok, MBHash} =
                        aec_headers:hash_header(
                            aec_blocks:to_header(MicroBlock)),
                    format_txs(BlockTxs, MBHash)
                end,
                MicroBlocks, 600000),
            BlockTxs1 = lists:flatten(lists:reverse(BlockTxs)),
            KeyBlockTxs = format_block_txs(Block),
            tx_spend_operations(KeyBlockTxs ++ BlockTxs1)
    end.

format_txs(Txs, MBHash) ->
    DryTxs = [{tx, aetx_sign:tx(Tx)} || Tx <- Txs],
    case aec_dry_run:dry_run({in, MBHash}, [], DryTxs, [tx_events]) of
        {ok, {Results, _Events}} ->
            TxHashes = [aetx_sign:hash(Tx) || Tx <- Txs],
            lists:zip(TxHashes, Results);
        {error, Reason} ->
            lager:debug("dry_run_error: ~p", [Reason]),
            throw({dry_run_err, Reason})
    end.

format_block_txs(KeyBlock) ->
    RewardTxs = reward_txs(KeyBlock, aec_consensus:get_genesis_consensus_module()),
    PreTxs = expiry_txs(KeyBlock),
    ForkTxs = hardfork_txs(aec_blocks:height(KeyBlock)),
    case RewardTxs ++ PreTxs ++ ForkTxs of
        [] ->
            [];
        KBTxs ->
            %% Supply empty tx hash. Could invent one??
            [{<<"">>,
                {block, KBTxs}}]
    end.

expiry_txs(KeyBlock0) ->
    Height = aec_blocks:height(KeyBlock0),
    case key_block_at_height(Height + 1) of
        {error,chain_too_short} ->
            [];
        {ok, KeyBlock} ->
            Header = aec_blocks:to_header(KeyBlock),
            {ok, Hash} = aec_headers:hash_header(Header),
            TxEnv = aetx_env:tx_env_from_key_header(Header, Hash, aec_headers:time_in_msecs(Header), aec_headers:prev_hash(Header)),

            %% Take the trees from the end of the last microblock of the prev tx
            PrevHash = aec_headers:prev_hash(Header),
            {ok, Trees} = aec_chain:get_block_state(PrevHash),
            Protocol = aetx_env:consensus_version(TxEnv),
            {Trees1, TxEnv1} = aeo_state_tree:prune(Height + 1, Trees, TxEnv),
            {_, TxEnv2} = aens_state_tree:prune(Height + 1, Protocol, Trees1, TxEnv1),
            %% Could possibly just use this call to take care of everything including hard fork accounts:
            %% {_Trees1, Env1} = aec_trees:perform_pre_transformations(Trees, TxEnv, PrevVersion),
            %% Possibly later
            aetx_env:events(TxEnv2)
    end.

reward_txs(_Block, aec_consensus_hc) -> 
    [];
reward_txs(Block, _Consensus) ->
    %% Block rewards.
    %% In this block the reward for the beneficiary 180 blocks earlier will be paid
    %% We don't store the amount on chain, so need to re-calculate
    Delay = aec_governance:beneficiary_reward_delay(),
    {ok, TopBlock} = top_key_block(),
    TopHeight = aec_blocks:height(TopBlock),
    Height = aec_blocks:height(Block),
    if Height >= Delay, Height < TopHeight ->
           %% For Rosetta the reward Txs need to be in the block before the Beneficiary
           %% account has their updated balance. No rewards yet at the top
           {ok, NextBlock} = key_block_at_height(Height + 1),
           Node = aec_chain_state:wrap_block(NextBlock),
           Trees = aec_chain_state:grant_fees(Node, aec_trees:new(), Delay, false, nil),
           Accounts =
               aec_accounts_trees:to_list(
                    aec_trees:accounts(Trees)),
             lists:map(fun({K, V}) ->
                          Acct = aec_accounts:deserialize(K, V),
                          {reward, {aec_accounts:pubkey(Acct), aec_accounts:balance(Acct)}}
                       end,
                       Accounts);
       true ->
           []
    end.

%% Inject balance change operations at each of our historical hard forks
%% The balance changes need to be in the block before the fork to keep Rosetta happy
hardfork_txs(Height) ->
    Forks =  maps:to_list(aec_hard_forks:protocols()),
    case lists:keyfind(Height + 1, 2, Forks) of
        false ->
            [];
        {?MINERVA_PROTOCOL_VSN, _} ->
            Deltas = aec_fork_block_settings:minerva_accounts(),
            hardfork_ops(Deltas);
        {?FORTUNA_PROTOCOL_VSN, _} ->
            Deltas = aec_fork_block_settings:fortuna_accounts(),
            hardfork_ops(Deltas);
        {?LIMA_PROTOCOL_VSN, _} ->
            Deltas = aec_fork_block_settings:lima_accounts(),
            Ops = hardfork_ops(Deltas, []),
            ExtraDeltas = aec_fork_block_settings:lima_extra_accounts(),
            Ops1 = hardfork_ops(ExtraDeltas, Ops),
            Contracts = aec_fork_block_settings:lima_contracts(),
            Ops2 = hardfork_contract_ops(Contracts, Ops1),
            lists:reverse(Ops2);
        {?IRIS_PROTOCOL_VSN, _} ->
            [];
        _ ->
            []
    end.

hardfork_ops(Deltas) ->
    lists:map(fun({K, V}) ->
                          {fork, {K, V}}
                       end,
                       Deltas).

hardfork_ops(Deltas, Acc0) ->
    lists:foldl(fun({PubKey, Amount}, Acc) ->
        [{fork, {PubKey, Amount}} | Acc]
    end, Acc0, Deltas).

hardfork_contract_ops(Contracts, Acc0) ->
    lists:foldl(fun(#{pubkey := PubKey, amount := Amount}, Acc) ->
        [{fork, {PubKey, Amount}} | Acc]
    end, Acc0, Contracts).

encode_generation(KeyBlock, MicroBlocks) ->
    Header = aec_blocks:to_header(KeyBlock),
    #{<<"key_block">>    => aec_headers:serialize_for_client(Header),
      <<"micro_blocks">> => lists:map(fun encode_microblock/1, MicroBlocks)}.

encode_microblock(MB) ->
    {ok, Hash} = aec_blocks:hash_internal_representation(MB),
    aeser_api_encoder:encode(micro_block_hash, Hash).


tx_spend_operations(Results) ->
    lists:map(fun({TxHash, Result}) ->
                    {Res, _} = tx_spend_ops(Result),
                    #{<<"transaction_identifier">> => #{<<"hash">> => format_tx_hash(TxHash)},
                    <<"operations">> => lists:reverse(Res)}
                end,
                Results).

tx_spend_ops({_Type, {ok, Events}}) ->
    lists:foldl(fun tx_spend_op/2, {[], 0}, Events);
tx_spend_ops({_Type, {ok, Events, CallObj}}) ->
    Call = aect_call:serialize_for_client(CallObj),
    #{<<"return_type">> := ReturnType} = Call,
    case ReturnType of
        <<"ok">> ->
            lists:foldl(fun tx_spend_op/2, {[], 0}, Events);
        Fail when Fail == <<"error">>; Fail == <<"revert">> ->
            %% Just take the fees
            lists:foldl(fun tx_spend_op/2, {[], 0}, Events)
    end;
tx_spend_ops({block, BlockOps}) ->
    lists:foldl(fun tx_spend_op/2, {[], 0}, BlockOps).

tx_spend_op({{internal_call_tx, _Key}, _}, {Acc, Ix}) ->
    %% Balance change ops with contract calls 
    {Acc, Ix};
tx_spend_op({{spend, {SenderPubkey, RecipientPubkey, Amount}}, #{type := _Type}},
            {Acc, Ix}) ->
    From = format_account_pubkey(SenderPubkey),
    To = format_account_pubkey(RecipientPubkey),
    FromOp = spend_tx_op(Ix, <<"Spend.amount">>, From, -Amount),
    ToOp = spend_tx_op(Ix + 1, <<"Spend.amount">>, To, Amount),
    {[ToOp, FromOp | Acc], Ix + 2};
tx_spend_op({{delta, {Pubkey, Amount}}, #{info := Info}}, {Acc, Ix}) ->
    From = format_account_pubkey(Pubkey),
    DeltaOp = spend_tx_op(Ix, Info, From, Amount),
    {[DeltaOp | Acc], Ix + 1};
tx_spend_op({reward, {Pubkey, Amount}}, {Acc, Ix}) ->
    To = format_account_pubkey(Pubkey),
    Op = spend_tx_op(Ix, <<"Chain.reward">>, To, Amount),
    {[Op | Acc], Ix + 1};
tx_spend_op({fork, {Pubkey, Amount}}, {Acc, Ix}) ->
    To = format_account_pubkey(Pubkey),
    Op = spend_tx_op(Ix, <<"Chain.amount">>, To, Amount),
    {[Op | Acc], Ix + 1};
tx_spend_op({{channel, _Pubkey}, #{}}, {Acc, Ix}) ->
    {Acc, Ix}.

spend_tx_op(Index, Type, Address, Amount) ->
    #{<<"operation_identifier">> => #{<<"index">> => Index},
        <<"type">> => Type,
        <<"status">> => <<"SUCCESS">>,
        <<"account">> => #{<<"address">> => Address},
        <<"amount">> => amount(Amount)}.

amount(Amount) ->
    #{<<"value">> => integer_to_binary(Amount),
        <<"currency">> => #{<<"symbol">> => <<"AE">>, <<"decimals">> => 18}}.

-spec next_nonce(aeser_api_encoder:encoded()) -> {ok, non_neg_integer()} | {error, account_not_found}.
next_nonce(AccountAddress) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    {ok, Id} = create_id(AccountAddress, AllowedTypes),
    PubKey = id_value(Id),
    aec_next_nonce:pick_for_account(PubKey).

oracle_at_height(OracleAddress, Height) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleAddress),
    {ok, KeyBlock} = key_block_at_height(Height),
    Header = aec_blocks:to_header(KeyBlock),
    {ok, Hash} = aec_headers:hash_header(Header),
    aec_chain:get_oracle_at_hash(OraclePubkey, Hash).

oracle_at_block(OracleAddress, BlockHash) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleAddress),
    {_BlockType, Hash} = aeser_api_encoder:decode(BlockHash),
    aec_chain:get_oracle_at_hash(OraclePubkey, Hash).

%% @doc Lookup the state of an oracle query against a specific oracle at height.
%% Example:
%% ```aeapi:oracle_query_at_height(<<"ok_AFbLSrppnBFgbKPo4GykK5XEwdq15oXgosgcdL7ud6Vo2YPsH">>, <<"oq_2SkZv9hyJTbocBtHdk36yMXXrycWURyB2zjXXh8CaNaPC4RFvG">>, 248940).'''
oracle_query_at_height(OracleAddress, QueryId, Height) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleAddress),
    {oracle_query_id, Query} = decode(QueryId),
    {ok, KeyBlock} = key_block_at_height(Height),
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

%% @doc Lookup oracle queries against an oracle at a given height.
%% Example:
%% ```{ok, Qs} = aeapi:oracle_queries_at_height(<<"ok_4HGhEdjeRtpsWzfSEJZnBKNmjgHALAifcBUey8EvRAdDfRsqc">>,'$first', all, 100, 248940).'''
-spec oracle_queries_at_height(aec_keys:pubkey(), binary() | '$first', open | closed | all, non_neg_integer(), non_neg_integer()) ->
    {ok, [aeo_query:query()]} | {error, no_state_trees}.
oracle_queries_at_height(OracleAddress, From, QueryType, Max, Height) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleAddress),
    {ok, KeyBlock} = key_block_at_height(Height),
    Header = aec_blocks:to_header(KeyBlock),
    {ok, Hash} = aec_headers:hash_header(Header),
    aec_chain:get_oracle_queries_at_hash(OraclePubkey, From, QueryType, Max, Hash).


%% @doc Lookup oracle queries against an oracle at a given blockhash.
%% Example:
%% ```{ok, Qs} = aeapi:oracle_queries_at_height(<<"ok_4HGhEdjeRtpsWzfSEJZnBKNmjgHALAifcBUey8EvRAdDfRsqc">>,'$first', all, 100, <<"kh_2hWHCGRcrYoZkuwD4GZ4DdZfyYY7PTrvf7SKT9ugjhh9NLVF19">>).'''
-spec oracle_queries_at_block(aec_keys:pubkey(), binary() | '$first', open | closed | all, non_neg_integer(), aeser_api_encoder:encoded()) ->
    {ok, [aeo_query:query()]} | {error, no_state_trees}.
oracle_queries_at_block(OracleAddress, From, QueryType, Max, BlockHash) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleAddress),
    {_BlockType, Hash} = aeser_api_encoder:decode(BlockHash),
    aec_chain:get_oracle_queries_at_hash(OraclePubkey, From, QueryType, Max, Hash).


