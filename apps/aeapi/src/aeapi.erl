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
         key_block_candidate_header/0,
         key_block_at_height/1,
         key_block_header_at_height/1,
         key_block_at_hash/1,
         key_block_header_at_hash/1,
         key_block_txs/1,
         prev_key_block/1,
         prev_block/1,
         micro_blocks_at_key_block/1,
         generation_at_height/1,
         balance_change_events_in_tx/2,
         balance_change_events_in_block/1]).

%%% Accounts, Contracts and Transactions API
-export([account/1,
         account_at_height/2,
         account_at_block/2,
         next_nonce/1,
         next_nonce/2,
         contract/1,
         balance_at_height/2,
         balance_at_block/2]).

%% Block Inspection API
-export([block_height/1,
         block_time_in_msecs/1,
         micro_block_txs/1]).

%%% ID Record API
-export([create_id/2,
         id_type/1,
         id_value/1]).

%%% Oracle API
-export([oracle_at_block/2,
         oracle_at_height/2,
         oracle_queries_at_block/5,
         oracle_queries_at_height/5,
         oracle_query_at_block/3,
         oracle_query_at_height/3]).

%%% Formatting and decoding hashes and keys
-export([format_id/1,
         format/2,
         decode/1,
         decode/2]).



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


-spec key_block_candidate_header() -> Result
    when Result :: {ok, aec_headers:header()}
                 | {error, Reason},
         Reason :: beneficiary_not_configured
                 | not_found.
%% @doc
%% Retrieve the current key block candidate header.

key_block_candidate_header() ->
    case aec_conductor:get_key_block_candidate() of
        {ok, Block} -> {ok, aec_blocks:to_header(Block)};
        Error       -> Error
    end.


-spec key_block_at_height(Height) -> Result
    when Height :: aec_blocks:height(),
         Result :: {ok, aec_blocks:key_block()}
                 | {error, Reason},
         Reason :: chain_too_short
                 | block_not_found.
%% @doc
%% Retrieve the keyblock that applies to a given height.

key_block_at_height(Height) ->
    aec_chain:get_key_block_by_height(Height).


-spec key_block_header_at_height(Height) -> Result
    when Height :: aec_blocks:height(),
         Result :: {ok, aec_headers:key_header()}
                 | {error, Reason},
         Reason :: chain_too_short
                 | block_not_found.
%% @doc
%% Retrieve the header of the keyblock that applies to a given height.

key_block_header_at_height(Height) when is_integer(Height) ->
    case key_block_at_height(Height) of
        {ok, Block} -> {ok, aec_blocks:to_header(Block)};
        Error       -> Error
    end.


-spec key_block_at_hash(Hash) -> Result
    when Hash   :: aeser_api_encoder:encoded(),
         Result :: {ok, aec_blocks:key_block()}
                 | {error, Reason},
         Reason :: invalid_hash
                 | block_not_found.
%% @doc
%% Retrieve the keyblock that applies to a given block hash.

key_block_at_hash(Hash) ->
    try
        {key_block_hash, DecodedHash} = aeser_api_encoder:decode(Hash),
        case aec_chain:get_block(DecodedHash) of
            {ok, Block} -> {ok, Block};
            error       -> {error, block_not_found}
        end
    catch
        error -> {error, invalid_hash}
    end.


-spec key_block_header_at_hash(Hash) -> Result
    when Hash   :: aeser_api_encoder:encoded(),
         Result :: {ok, aec_headers:key_header()}
                 | {error, Reason},
         Reason :: invalid_hash
                 | block_not_found.
%% @doc
%% Retrieve the keyblock header that applies to a given block hash.

key_block_header_at_hash(Hash) ->
    case key_block_at_hash(Hash) of
        {ok, Block} -> {ok, aec_blocks:to_header(Block)};
        Error       -> Error
    end.


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
%% #{<<"key_block">>    => KeyBlock,
%%   <<"micro_blocks">> => MicroBlockHashes}
%% '''
%% where `KeyBlock' is the form returned by `aec_headers:serialize_for_client/1'
%% and `MicroBlockHashes' are microblock hashes encoded in the `<<"mh_...">>' form.

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

encode_generation(KeyBlock, MicroBlocks) ->
    Header = aec_blocks:to_header(KeyBlock),
    #{<<"key_block">>    => aec_headers:serialize_for_client(Header),
      <<"micro_blocks">> => lists:map(fun encode_microblock/1, MicroBlocks)}.

encode_microblock(MB) ->
    {ok, Hash} = aec_blocks:hash_internal_representation(MB),
    aeser_api_encoder:encode(micro_block_hash, Hash).


-spec balance_change_events_in_tx(SignedTX, MicroBlock) -> Events
    when SignedTX   :: term(), % FIXME
         MicroBlock :: aec_blocks:micro_block(),
         Events     :: term(). % FIXME
%% @doc
%% Given a signed transaction and a microblock, extract the balance change events
%% that occur as a consequence of executing the transaction. This requires replaying
%% the entire microblock up to the point of this transaction and the transaction
%% itself may have arbitrary side effects (it may be a conract call, for example),
%% so this operation can be quite involved.

balance_change_events_in_tx(SignedTX, MicroBlock) ->
    % Format a single TX in a block. We need to dry run all the TXs in the microblock
    % up to and including the one we want to know about. dry run works on dummy signed
    % TXs so the tx_hash included in the results is not the same as the one requested.
    % Solve this by pre-filtering, then patching in the correct hash values afterwards.
    {ok, MBHash} = aec_headers:hash_header(aec_blocks:to_header(MicroBlock)),
    BlockTXs = micro_block_txs(MicroBlock),
    NeededTXs = keep_tx_until(BlockTXs, aetx_sign:hash(SignedTX)),
    Ops = format_txs(NeededTXs, MBHash),
    SpendOps = tx_spend_operations(Ops),
    lists:last(SpendOps).

keep_tx_until(TXs, TXHash) ->
    keep_tx_until(TXs, TXHash, []).

keep_tx_until([], _UntilTXHash, _Acc) ->
    [];
keep_tx_until([SignedTX | TXs], UntilTXHash, Acc) ->
    TXHash = aetx_sign:hash(SignedTX),
    if TXHash == UntilTXHash ->
           lists:reverse([SignedTX | Acc]);
       true ->
           keep_tx_until(TXs, UntilTXHash, [SignedTX | Acc])
    end.


-spec balance_change_events_in_block(Block) -> Events
    when Block  :: aec_blocks:block(),
         Events :: term(). % FIXME
%% @doc
%% Given a block, extract the balance change events that occur within it.
%% If a keyblock is passed as the argument the balance events from the entire
%% generation are returned. If a microblock is passed as the argument then only
%% the balance change events within that microblock are returned.

balance_change_events_in_block(Block) ->
    case aec_blocks:type(Block) of
        micro ->
            BlockTXs = micro_block_txs(Block),
            {ok, MBHash} = aec_headers:hash_header(aec_blocks:to_header(Block)),
            TXs = format_txs(BlockTXs, MBHash),
            tx_spend_operations(TXs);
        key ->
            {ok, MicroBlocks} = micro_blocks_at_key_block(Block),
            FormatTXs =
                fun(MB) ->
                    BlockTXs = micro_block_txs(MB),
                    {ok, MBHash} = aec_headers:hash_header(aec_blocks:to_header(MB)),
                    format_txs(BlockTXs, MBHash)
                end,
            {BlockTXs, []} = aeu_lib:pmap(FormatTXs, MicroBlocks, 600000),
            BlockTXs1 = lists:flatten(lists:reverse(BlockTXs)),
            KeyBlockTXs = format_block_txs(Block),
            tx_spend_operations(KeyBlockTXs ++ BlockTXs1)
    end.

format_txs(TXs, MBHash) ->
    DryTXs = [{tx, aetx_sign:tx(TX)} || TX <- TXs],
    case aec_dry_run:dry_run({in, MBHash}, [], DryTXs, [tx_events]) of
        {ok, {Results, _Events}} ->
            lists:zip(lists:map(fun aetx_sign:hash/1, TXs), Results);
        {error, Reason} ->
            lager:debug("dry_run_error: ~p", [Reason]),
            throw({dry_run_err, Reason})
    end.

format_block_txs(KeyBlock) ->
    RewardTXs =
        case aec_consensus:get_genesis_consensus_module() =:= aec_consensus_hc of
            false -> reward_txs(KeyBlock);
            true  -> []
        end,
    PreTXs = expiry_txs(KeyBlock),
    ForkTXs = hardfork_txs(aec_blocks:height(KeyBlock)),
    case RewardTXs ++ PreTXs ++ ForkTXs of
        [] ->
            [];
        KBTXs ->
            %% Supply empty TX hash. Could invent one??
            [{<<"">>, {block, KBTXs}}]
    end.

expiry_txs(KeyBlock0) ->
    Height = aec_blocks:height(KeyBlock0),
    NextHeight = Height + 1,
    case key_block_at_height(NextHeight) of
        {error, chain_too_short} ->
            [];
        {ok, KeyBlock} ->
            Header = aec_blocks:to_header(KeyBlock),
            {ok, Hash} = aec_headers:hash_header(Header),
            TS = aec_headers:time_in_msecs(Header),
            PrevHash = aec_headers:prev_hash(Header),
            TXEnv = aetx_env:tx_env_from_key_header(Header, Hash, TS, PrevHash),
            % Take the trees from the end of the last microblock of the prev TX
            {ok, Trees} = aec_chain:get_block_state(PrevHash),
            Protocol = aetx_env:consensus_version(TXEnv),
            {Trees1, TXEnv1} = aeo_state_tree:prune(NextHeight, Trees, TXEnv),
            {_, TXEnv2} = aens_state_tree:prune(NextHeight, Protocol, Trees1, TXEnv1),
            % Could possibly just use this call to take care of everything
            % including hard fork accounts:
            % {_, Env1} =
            %     aec_trees:perform_pre_transformations(Trees, TXEnv, PrevVersion),
            % Possibly later
            aetx_env:events(TXEnv2)
    end.

% Block rewards.
% In this block the reward for the beneficiary 180 blocks earlier will be paid
% We don't store the amount on chain, so need to re-calculate
reward_txs(Block) ->
    Delay = aec_governance:beneficiary_reward_delay(),
    {ok, TopBlock} = top_key_block(),
    TopHeight = aec_blocks:height(TopBlock),
    Height = aec_blocks:height(Block),
    if Height >= Delay, Height < TopHeight ->
            % For Rosetta the reward TXs need to be in the block before the Beneficiary
            % account has their updated balance. No rewards yet at the top
            {ok, NextBlock} = key_block_at_height(Height + 1),
            Node = aec_chain_state:wrap_block(NextBlock),
            NewTree = aec_trees:new(),
            Trees = aec_chain_state:grant_fees(Node, NewTree, Delay, false, nil),
            Accounts = aec_accounts_trees:to_list(aec_trees:accounts(Trees)),
            lists:map(fun form_reward/1, Accounts);
       true ->
            []
    end.

form_reward({K, V}) ->
    Acct = aec_accounts:deserialize(K, V),
    {reward, {aec_accounts:pubkey(Acct), aec_accounts:balance(Acct)}}.

% Inject balance change operations at each of our historical hard forks
% The balance changes need to be in the block before the fork to keep Rosetta happy
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
    [{fork, {K, V}} || {K, V} <- Deltas].

hardfork_ops(Deltas, Acc) ->
    lists:foldl(fun form_fork_t/2, Acc, Deltas).

form_fork_t({PubKey, Amount}, Acc) ->
    [{fork, {PubKey, Amount}} | Acc].

hardfork_contract_ops(Contracts, Acc) ->
    lists:foldl(fun form_fork_m/2, Acc, Contracts).

form_fork_m(#{pubkey := PubKey, amount := Amount}, Acc) ->
    [{fork, {PubKey, Amount}} | Acc].

tx_spend_operations(Results) ->
    lists:map(fun format_spend_txs/1, Results).

format_spend_txs({TXHash, Result}) ->
    {Res, _} = tx_spend_ops(Result),
    #{<<"transaction_identifier">> => #{<<"hash">> => format(tx_hash, TXHash)},
      <<"operations">> => lists:reverse(Res)}.

tx_spend_ops({_, {ok, Events}}) ->
    lists:foldl(fun tx_spend_op/2, {[], 0}, Events);
tx_spend_ops({_, {ok, Events, _}}) ->
    lists:foldl(fun tx_spend_op/2, {[], 0}, Events);
tx_spend_ops({block, BlockOps}) ->
    lists:foldl(fun tx_spend_op/2, {[], 0}, BlockOps).

tx_spend_op({{internal_call_tx, _Key}, _}, {Acc, Ix}) ->
    %% Balance change ops with contract calls 
    {Acc, Ix};
tx_spend_op({{spend, {SenderPubkey, RecipientPubkey, Amount}}, #{type := _Type}},
            {Acc, Ix}) ->
    From = format(account_pubkey, SenderPubkey),
    To = format(account_pubkey, RecipientPubkey),
    FromOp = spend_tx_op(Ix, <<"Spend.amount">>, From, -Amount),
    ToOp = spend_tx_op(Ix + 1, <<"Spend.amount">>, To, Amount),
    {[ToOp, FromOp | Acc], Ix + 2};
tx_spend_op({{delta, {Pubkey, Amount}}, #{info := Info}}, {Acc, Ix}) ->
    From = format(account_pubkey, Pubkey),
    DeltaOp = spend_tx_op(Ix, Info, From, Amount),
    {[DeltaOp | Acc], Ix + 1};
tx_spend_op({reward, {Pubkey, Amount}}, {Acc, Ix}) ->
    To = format(account_pubkey, Pubkey),
    Op = spend_tx_op(Ix, <<"Chain.reward">>, To, Amount),
    {[Op | Acc], Ix + 1};
tx_spend_op({fork, {Pubkey, Amount}}, {Acc, Ix}) ->
    To = format(account_pubkey, Pubkey),
    Op = spend_tx_op(Ix, <<"Chain.amount">>, To, Amount),
    {[Op | Acc], Ix + 1};
tx_spend_op({{channel, _Pubkey}, #{}}, {Acc, Ix}) ->
    {Acc, Ix}.

spend_tx_op(Index, Type, Address, Amount) ->
    #{<<"operation_identifier">> => #{<<"index">> => Index},
      <<"type">>                 => Type,
      <<"status">>               => <<"SUCCESS">>,
      <<"account">>              => #{<<"address">> => Address},
      <<"amount">>               => amount(Amount)}.

amount(Amount) ->
    #{<<"value">>    => integer_to_binary(Amount),
      <<"currency">> => #{<<"symbol">> => <<"AE">>, <<"decimals">> => 18}}.



%%% Accounts, Contracts and Transactions API

-spec account(Address) -> Result
    when Address :: binary(), % <<"ak_...">>
         Result  :: {ok, aec_accounts:account()}
                  | {error, Reason},
         Reason  :: account_not_found
                  | invalid_prefix
                  | invalid_encoding.
%% @doc
%% Retrieve an account by its address at the current chain height.

account(Address) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    case aeser_api_encoder:safe_decode({id_hash, AllowedTypes}, Address) of
        {ok, ID} ->
            {_, PubKey} = aeser_id:specialize(ID),
            account2(PubKey);
        Error ->
            Error
    end.

account2(PubKey) ->
    case aec_chain:get_account(PubKey) of
        {value, Account} -> {ok, Account};
        none             -> {error, account_not_found}
    end.


-spec account_at_height(Address, Height) -> Result
    when Address :: binary(), % <<"ak_...">>
         Height  :: aec_blocks:height(),
         Result  :: {ok, aec_accounts:account()}
                  | {error, Reason},
         Reason  :: invalid_encoding
                  | invalid_prefix
                  | chain_too_short
                  | garbage_collected
                  | account_not_found.
%% @doc
%% Retrieve an account as it appeared on the chain at the given height.
%%
%% Note that this can fail in the event that the node being queried has garbage
%% collection enabled and the account at this height has been removed.

account_at_height(Address, Height) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    case aeser_api_encoder:safe_decode({id_hash, AllowedTypes}, Address) of
        {ok, ID} ->
            {_, PubKey} = aeser_id:specialize(ID),
            account_at_height2(PubKey, Height);
        Error ->
            Error
    end.

account_at_height2(PubKey, Height) ->
    case aec_chain:get_account_at_height(PubKey, Height) of
        {value, Account} -> {ok, Account};
        none             -> {error, account_not_found};
        Error            -> Error
    end.


-spec account_at_block(Address, BlockID) -> Result
    when Address :: binary(), % <<"ak_...">>
         BlockID :: binary(), % <<"kh_...">> or <<"mh_...">>
         Result  :: {ok, aec_accounts:account()}
                  | {error, Reason},
         Reason  :: account_not_found
                  | {Element :: pubkey | block,
                     Info    :: invalid_encoding | invalid_prefix}.
%% @doc
%% Retrieve an account as it appeared at the point in the chain's history that it
%% appeared at the given block's height.
%%
%% Note that this can fail in the event that the node being queried has garbage
%% collection enabled and the account at this height has been removed.

account_at_block(Address, BlockID) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    case aeser_api_encoder:safe_decode({id_hash, AllowedTypes}, Address) of
        {ok, ID} ->
            {_, PubKey} = aeser_id:specialize(ID),
            account_at_block2(PubKey, BlockID);
        {error, Info} ->
            {error, {pubkey, Info}}
    end.

account_at_block2(PubKey, BlockID) ->
    case aeser_api_encoder:safe_decode(block_hash, BlockID) of
        {ok, Hash}    -> account_at_block3(PubKey, Hash);
        {error, Info} -> {error, {block, Info}}
    end.

account_at_block3(PubKey, Hash) ->
    case aec_chain:get_account_at_hash(PubKey, Hash) of
        {value, Account} -> {ok, Account};
        none             -> {error, account_not_found};
        Error            -> Error
    end.


-spec next_nonce(Address) -> Result
    when Address  :: binary(), % <<"ak_...">>
         Result   :: {ok, non_neg_integer()}
                   | {error, Reason},
         Reason   :: invalid_prefix
                   | invalid_encoding
                   | account_not_found.
%% @doc
%% Given an account ID, return its next nonce.
%% The same as calling `next_nonce(Address, max)'.

next_nonce(Address) ->
    next_nonce(Address, max).


-spec next_nonce(Address, Strategy) -> Result
    when Address  :: binary(), % <<"ak_...">>
         Strategy :: max | continuity,
         Result   :: {ok, non_neg_integer()}
                   | {error, Reason},
         Reason   :: invalid_prefix
                   | invalid_encoding
                   | account_not_found.
%% @doc
%% Given an account ID, return its next nonce.

next_nonce(Address, Strategy) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    case aeser_api_encoder:safe_decode({id_hash, AllowedTypes}, Address) of
        {ok, ID} ->
            {_, PubKey} = aeser_id:specialize(ID),
            aec_next_nonce:pick_for_account(PubKey, Strategy);
        Error ->
            Error
    end.


-spec contract(Address) -> Result
    when Address :: binary(), % <<"ct_...">>
         Result  :: {ok, aect_contracts:contract()}
                  | {error, Reason},
         Reason  :: invalid_prefix
                  | invalid_encoding
                  | contract_not_found.
%% @doc
%% Given a contract public address, retrieve the contract-specific details of
%% it. To retrieve the account-style information about the contract (it's balance,
%% for example) call address/1 with the same address.

contract(Address) ->
    case aeser_api_encoder:safe_decode(contract_pubkey, Address) of
        {ok, PubKey} ->
            case aec_chain:get_contract(PubKey) of
                {ok, Con}  -> {ok, Con};
                {error, _} -> {error, contract_not_found}
            end;
        Error ->
            Error
    end.


-spec balance_at_height(Address, Height) -> Result
    when Address :: binary(),
         Height  :: aec_blocks:height(),
         Result  :: {ok, Balance :: non_neg_integer()}
                  | {error, Reason},
         Reason  :: invalid_encoding
                  | invalid_prefix
                  | chain_too_short
                  | garbage_collected
                  | account_not_found
                  | contract_not_found.
%% @doc
%% Lookup the opening balance of an account or contract at the keyblock with
%% the given height.
%% Example (on testnet, AKA the "ae_uat" chain):
%% ```
%% Account = <<"ak_2W2NjVgAo6pkj96R71JHsTK5h2UVvmot7hYrqVfhzAqHGGU5ea">>,
%% Height = 750000,
%% balance_at_height(Account, Height) -> {ok, 9999346828000000000}.
%% '''
%%
%% Note that this can fail in the event that the node being queried has garbage
%% collection enabled and the account at this height has been removed.

balance_at_height(Address, Height) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    case create_id(Address, AllowedTypes) of
        {ok, ID} -> balance_at_height2(ID, Height);
        Error    -> Error
    end.

balance_at_height2(ID, Height) ->
    {Type, PubKey} = aeser_id:specialize(ID),
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


-spec balance_at_block(Address, BlockID) -> Result
    when Address :: binary(), % <<"ak_...">> or <<"ct_...">>
         BlockID :: binary(), % <<"kh_...">> or <<"mh_...">>
         Result  :: {ok, Balance :: non_neg_integer()}
                  | {error, Reason},
         Reason  :: invalid_block_hash
                  | invalid_address
                  | block_not_found
                  | garbage_collected
                  | account_not_found
                  | contract_not_found.
%% @doc
%% Return the given account's balance at the height of the block ID provided.
%% `Address' should be an externally serialized binary form with the prefix
%% `<<"ak_...">>' or `<<"ct_...">>'.
%% `BlockID' shhould be an externally serialized binary form with the prefix
%% `<<"kh_...">>' or `<<"mh_...">>'.
%%
%% Note that this can fail in the event that the node being queried has garbage
%% collection enabled and the account at this height has been removed.

balance_at_block(Address, BlockID) ->
    case decode_catcher(BlockID) of
        {ok, Hash} -> balance_at_block2(Address, Hash);
        Error      -> Error
    end.

balance_at_block2(Address, Hash) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    case create_id(Address, AllowedTypes) of
        {ok, ID}                  -> balance_at_block3(ID, Hash);
        {error, invalid_encoding} -> {error, invalid_address}
    end.

balance_at_block3(ID, Hash) ->
    case aec_db:get_block(Hash) of
        undefined ->
            {error, block_not_found};
        Block ->
            Height = aec_blocks:height(Block),
            balance_at_height2(ID, Height)
    end.

decode_catcher(BlockID) ->
    try
        case aeser_api_encoder:decode(BlockID) of
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



%% ID Records API

-spec create_id(Key, AcceptableTypes) -> Result
    when Key             :: aeser_api_encoder:encoded(),
         AcceptableTypes :: [Type],
         Type            :: account_pubkey
                          | oracle_pubkey
                          | name 
                          | commitment
                          | contract_pubkey
                          | channel,
         Result          :: {ok, aeser_id:id()} | {error, Reason},
         Reason          :: any(). %% FIXME
%% @doc Convert a formatted pubkey to an internal #id{} record.
%% For example:
%% ```
%% Address = <<"ak_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>,
%% create_id(Address, [account_pubkey]) ->
%%     {ok,
%%      #id{tag = account,
%%          val = <<170,20,197,4,89,131,91,158,24,63,28,200,44,49,35,88,224,211,211,30,
%%                  29,108,91,130,93,230,47,111,34,172,124,50>>}}
%% '''

create_id(Binary, AllowedTypes) ->
    aeser_api_encoder:safe_decode({id_hash, AllowedTypes}, Binary).


-spec id_type(aeser_id:id()) -> aeser_id:tag().
%% @doc Get the internal type of an ID.
%% Note that this is not the same as the type outside of the id realm
%%
%% For example:
%% ``id_type(Id :: aeser_id:id()) -> account.''

id_type(ID) ->
    case aeser_id:is_id(ID) of
        true ->
            aeser_id:specialize_type(ID);
        false ->
            {error, not_id}
    end.


-spec id_value(ID) -> Result
    when ID     :: aeser_id:id(),
         Result :: {ok, Value :: binary()}
                 | {error, bad_id}.
%% @doc Get the internal value of an ID.
%% Example:
%% ```id_value(ID :: aeser_id:id()) ->
%%            <<170,20,197,4,89,131,91,158,24,63,28,200,44,49,35,88,224,211,211,30,
%%              29,108,91,130,93,230,47,111,34,172,124,50>>.'''

id_value(ID) ->
    case aeser_id:is_id(ID) of
        true ->
            {_Tag, Val} = aeser_id:specialize(ID),
            {ok, Val};
        false ->
            {error, bad_id}
    end.


-spec format_id(ID) -> Result
    when ID     :: aeser_id:id(),
         Result :: {ok, binary()}
                 | {error, not_id}.
%% @doc
%% A safe way to Format an aeser_id:id() to its printable / external form.
%% This function tests the provided ID for validity before serialization and
%% returns an `ok | error' tuple instead of throwing an exception on bad data.
%% For the unsafe version, call `aeser:format(account_pubkey, ID)' instead.
%%
%% Example:
%% ```
%% format_id(ID :: aeser_id:id()) ->
%%     {ok, <<"ak_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>}.
%% '''

format_id(ID) ->
    case aeser_id:is_id(ID) of
        true  -> aeser_api_encoder:encode(id_hash, ID);
        false -> {error, not_id}
    end.


-spec format(Type, Data) -> Formatted
    when Type      :: key_block_hash
                    | micro_block_hash
                    | block_pof_hash
                    | block_tx_hash
                    | block_state_hash
                    | channel
                    | contract_bytearray
                    | contract_pubkey
                    | contract_store_key
                    | transaction
                    | tx_hash
                    | oracle_pubkey
                    | oracle_query
                    | oracle_query_id
                    | oracle_response
                    | account_pubkey
                    | signature
                    | name
                    | commitment
                    | peer_pubkey
                    | state
                    | poi
                    | state_trees
                    | call_state_tree
                    | bytearray,
         Data      :: term(), % FIXME
         Formatted :: binary().
%% @doc Format internal hash and key types to their external / printable form.
%% Takes the internal binary form of the key/hash and formats it into the printable
%% form of the specified type.
%%
%% For example an internal pubkey might be formatted as either a
%% normal account or oracle depending on context.
%%
%% Format as normal account:
%% ```
%% Data =
%%     <<170,20,197,4,89,131,91,158,24,63,28,200,44,49,35,88,224,
%%       211,211,30, 29,108,91,130,93,230,47,111,34,172,124,50>>,
%% aeapi:format(account_pubkey, Data) ->
%%      <<"ak_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>.
%% '''
%%
%% Format as an Oracle linked to the same account:
%% ```
%% Data =
%%     <<170,20,197,4,89,131,91,158,24,63,28,200,44,49,35,88,224,
%%       211,211,30,29,108,91,130,93,230,47,111,34,172,124,50>>,
%% aeapi:format(oracle_pubkey, Data) ->
%%      <<"ok_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>.
%% '''

format(Type, Data) ->
    aeser_api_encoder:encode(Type, Data).


-spec decode(binary()) -> {atom(), binary()}.
%% @doc Decode the external / printable form of a key / hash.
%% Returns its type and internal binary value.
%%
%% For example:
%% ```
%% aeapi:decode(<<"ct_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>) ->
%%       {contract_pubkey,<<170,20,197,4,89,131,91,158,24,63,28,
%%                          200,44,49,35,88,224,211,211,30,29,108,
%%                          91,130,93,230,47,...>>}
%% '''

decode(Binary) ->
    aeser_api_encoder:decode(Binary).


-spec decode(Type, Data) -> Result
    when Type   :: key_block_hash
                 | micro_block_hash
                 | block_pof_hash
                 | block_tx_hash
                 | block_state_hash
                 | channel
                 | contract_bytearray
                 | contract_pubkey
                 | contract_store_key
                 | transaction
                 | tx_hash
                 | oracle_pubkey
                 | oracle_query
                 | oracle_query_id
                 | oracle_response
                 | account_pubkey
                 | signature
                 | name
                 | commitment
                 | peer_pubkey
                 | state
                 | poi
                 | state_trees
                 | call_state_tree
                 | bytearray,
         Data   :: aeser_api_encoder:encoded(),
         Result :: {ok, binary() | aeser_id:id()}
                 | {error, Reason :: atom()}.
%% @doc
%% A safe way to convert an external form of a key or hash to its internal
%% representation.
%%
%% For example:
%% ```
%% Address = <<"ct_2HuVfa8qJmYeJPb5ntE5dXre9e4pmFEq9FYthvemB7idvbjUbE">>,
%% aeapi:decode(contract_pubkey, Address) ->
%%       {ok, <<170,20,197,4,89,131,91,158,24,63,28,
%%              200,44,49,35,88,224,211,211,30,29,108,
%%              91,130,93,230,47,...>>}
%% '''
%% @end

%% TODO: Maybe add variants to decode further.
%%    Example: `aeapi:decode(contract, <<"cb_..">>, [decompile]).'

decode(Type, Data) ->
    aeser_api_encoder:safe_decode(Type, Data).


-spec oracle_at_height(OracleID, Height) -> Result
    when OracleID :: aec_keys:pubkey(),
         Height   :: non_neg_integer(),
         Result   :: {ok, aeo_oracles:oracle()}
                   | {error, Reason},
         Reason   :: not_found
                   | no_state_trees.
%% @doc
%% Retrieve an oracle's state at the given height.

oracle_at_height(OracleID, Height) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleID),
    {ok, KeyBlock} = key_block_at_height(Height),
    Header = aec_blocks:to_header(KeyBlock),
    {ok, Hash} = aec_headers:hash_header(Header),
    aec_chain:get_oracle_at_hash(OraclePubkey, Hash).


-spec oracle_at_block(OracleID, BlockID) -> Result
    when OracleID :: aec_keys:pubkey(),
         BlockID  :: binary(), % <<"kh_...">> or <<"mh_...">>
         Result   :: {ok, aeo_oracles:oracle()}
                   | {error, Reason},
         Reason   :: not_found
                   | no_state_trees.
%% @doc
%% Retrieve an oracle's state at the height of the given block's height.

oracle_at_block(OracleID, BlockID) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleID),
    {_BlockType, Hash} = aeser_api_encoder:decode(BlockID),
    aec_chain:get_oracle_at_hash(OraclePubkey, Hash).


-spec oracle_query_at_height(OracleID, QueryID, Height) -> Result
    when OracleID :: aec_keys:pubkey(),
         QueryID  :: aec_keys:pubkey(),
         Height   :: non_neg_integer(),
         Result   :: {ok, aeo_query:query()}
                   | {error, Reason},
         Reason   :: no_state_trees.
%% @doc
%% Lookup the state of an oracle query against a specific oracle the given height.
%% Example:
%% ```
%% Oracle = <<"ok_AFbLSrppnBFgbKPo4GykK5XEwdq15oXgosgcdL7ud6Vo2YPsH">>,
%% QueryID = <<"oq_2SkZv9hyJTbocBtHdk36yMXXrycWURyB2zjXXh8CaNaPC4RFvG">>,
%% aeapi:oracle_query_at_height(Oracle, QueryID, 248940).
%% '''

oracle_query_at_height(OracleID, QueryID, Height) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleID),
    {oracle_query_id, Query} = decode(QueryID),
    {ok, KeyBlock} = key_block_at_height(Height),
    Header = aec_blocks:to_header(KeyBlock),
    {ok, Hash} = aec_headers:hash_header(Header),
    get_oracle_query_at_hash(OraclePubkey, Query, Hash).


-spec oracle_query_at_block(OracleID, QueryID, BlockID) -> Result
    when OracleID :: aec_keys:pubkey(),
         QueryID  :: aec_keys:pubkey(),
         BlockID  :: binary(), % <<"kh_...">> or <<"mh_...">>
         Result   :: {ok, aeo_oracles:oracle()}
                   | {error, Reason},
         Reason   :: not_found
                   | no_state_trees.
%% @doc
%% Lookup the state of an oracle query at the given oracle at the height of the
%% given block.
%% Example:
%% ```
%% OracleID = <<"ok_4HGhEdjeRtpsWzfSEJZnBKNmjgHALAifcBUey8EvRAdDfRsqc">>,
%% QueryID = <<"oq_2SkZv9hyJTbocBtHdk36yMXXrycWURyB2zjXXh8CaNaPC4RFvG">>,
%% BlockID = <<"kh_2hWHCGRcrYoZkuwD4GZ4DdZfyYY7PTrvf7SKT9ugjhh9NLVF19">>,
%% {ok, Query} = aeapi:oracle_query_at_block(OracleID, QueryID, BlockID).
%% '''

oracle_query_at_block(OracleID, QueryID, BlockID) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleID),
    {oracle_query_id, Query} = decode(QueryID),
    {_BlockType, Hash} = aeser_api_encoder:decode(BlockID),
    get_oracle_query_at_hash(OraclePubkey, Query, Hash).

get_oracle_query_at_hash(OraclePubkey, Query, Hash) ->
    case aec_chain:get_oracle_query_at_hash(OraclePubkey, Query, Hash) of
        {ok, Q} ->
            {ok, aeo_query:serialize_for_client(Q)};
        Else ->
            Else
    end.

-spec oracle_queries_at_height(OracleID, From, Type, Max, Height) -> Result
    when OracleID :: aec_keys:pubkey(),
         From     :: binary() | '$first',
         Type     :: open | closed | all,
         Max      :: non_neg_integer(),
         Height   :: non_neg_integer(),
         Result   :: {ok, [aeo_query:query()]} | {error, no_state_trees}.
%% @doc Lookup oracle queries against an oracle at a given height.
%% Example:
%% ```
%% OracleID = <<"ok_4HGhEdjeRtpsWzfSEJZnBKNmjgHALAifcBUey8EvRAdDfRsqc">>,
%% {ok, Qs} = aeapi:oracle_queries_at_height(OracleID, '$first', all, 100, 248940).
%% '''
oracle_queries_at_height(OracleID, From, QueryType, Max, Height) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleID),
    {ok, KeyBlock} = key_block_at_height(Height),
    Header = aec_blocks:to_header(KeyBlock),
    {ok, Hash} = aec_headers:hash_header(Header),
    aec_chain:get_oracle_queries_at_hash(OraclePubkey, From, QueryType, Max, Hash).


-spec oracle_queries_at_block(OracleID, From, QueryType, Max, BlockID) -> Result
    when OracleID  :: aec_keys:pubkey(),
         From      :: binary() | '$first',
         QueryType :: open | closed | all,
         Max       :: non_neg_integer(),
         BlockID   :: aeser_api_encoder:encoded(),
         Result    :: {ok, [aeo_query:query()]}
                    | {error, Reason},
         Reason    :: not_found
                    | no_state_trees.
%% @doc Lookup oracle queries against an oracle at a given blockhash.
%% Example:
%% ```
%% Oracle = <<"ok_4HGhEdjeRtpsWzfSEJZnBKNmjgHALAifcBUey8EvRAdDfRsqc">>
%% Block = <<"kh_2hWHCGRcrYoZkuwD4GZ4DdZfyYY7PTrvf7SKT9ugjhh9NLVF19">>,
%% {ok, Qs} = aeapi:oracle_queries_at_height(Oracle,'$first', all, 100, Block).
%% '''

oracle_queries_at_block(OracleID, From, QueryType, Max, BlockID) ->
    {oracle_pubkey, OraclePubkey} = decode(OracleID),
    {_BlockType, Hash} = aeser_api_encoder:decode(BlockID),
    aec_chain:get_oracle_queries_at_hash(OraclePubkey, From, QueryType, Max, Hash).
