%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% API for chain related information.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_chain).

%%% Chain API
-export([ find_common_ancestor/2
        , find_tx_location/1
        , find_tx_with_location/1
        , genesis_block/0
        , genesis_hash/0
        , genesis_header/0
        , get_block/1
        , get_key_block_by_height/1
        , get_block_state/1
        , get_current_generation/0
        , get_generation_by_hash/2
        , get_generation_by_height/2
        , get_header/1
        , dirty_get_header/1
        , get_key_header_by_height/1
        , get_n_generation_headers_backwards_from_hash/2
        , get_at_most_n_generation_headers_forward_from_hash/2
        , get_top_N_blocks_time_summary/1
        , get_transactions_between/2
        , has_block/1
        , hash_is_in_main_chain/1
        , hash_is_connected_to_genesis/1
        , prev_hash_from_hash/1
        , sum_tokens_at_height/1
        , top_block/0
        , top_block_hash/0
        , top_block_node/0
        , top_key_block/0
        , top_key_block_hash/0
        , top_block_with_state/0
        , top_header/0
        , top_header_hash_and_state/0
        ]).

%%% Accounts API
-export([ all_accounts_balances_at_hash/1
        , get_account/1
        , get_account_at_hash/2
        , get_account_at_height/2
        ]).

%%% NS API
-export([ name_entry/1
        , resolve_name/2
        , resolve_namehash/2
        ]).

%%% Oracles API
-export([ get_oracles/2
        , get_oracle/1
        , get_oracle_queries/4
        , get_oracle_query/2
        ]).

%%% Contracts API
-export([ get_contract/1
        , get_contract_with_code/1
        , get_contract_call/3
        ]).

%%% Channels API
-export([ get_channel/1
        , get_channel/2
        , get_channel_at_hash/2
        ]).

%%% Generalized Accounts API
-export([get_ga_call/3
        ]).

%%% trees
-export([ get_top_state/0
        ]).

%%% Difficulty API
-export([ difficulty_at_hash/1
        , difficulty_at_top_block/0
        ]).

-type generation() :: #{ key_block => aec_blocks:key_block(),
                         micro_blocks := [aec_blocks:micro_block()],
                         dir := backward | forward }.

%%%===================================================================
%%% Accounts
%%%===================================================================

get_account(PubKey) ->
    case get_top_state() of
        {ok, Trees} ->
            aec_accounts_trees:lookup(PubKey, aec_trees:accounts(Trees));
        error -> {error, no_state_trees}
    end.

get_account_at_hash(PubKey, Hash) ->
    case get_block_state_partial(Hash, [accounts]) of
        {ok, Trees} ->
            aec_accounts_trees:lookup(PubKey, aec_trees:accounts(Trees));
        error -> {error, no_state_trees}
    end.

get_account_at_height(PubKey, Height) ->
    case aec_chain_state:get_key_block_hash_at_height(Height) of
        error -> {error, chain_too_short};
        {ok, Hash} -> get_account_at_hash(PubKey, Hash)
    end.

-spec all_accounts_balances_at_hash(binary()) ->
                                           {'ok', [{aec_keys:pubkey(), non_neg_integer()}]}
                                               | {'error', 'no_state_trees'}.
all_accounts_balances_at_hash(Hash) when is_binary(Hash) ->
    case get_block_state_partial(Hash, [accounts]) of
        {ok, Trees} ->
            ATrees = aec_trees:accounts(Trees),
            {ok, aec_accounts_trees:get_all_accounts_balances(ATrees)};
        error ->
            {error, no_state_trees}
    end.

%%%===================================================================
%%% Oracles
%%%===================================================================

-spec get_oracles(binary() | '$first', non_neg_integer()) ->
    {ok, [aeo_oracles:oracle()]} | {error, no_state_trees}.
get_oracles(From, Max) ->
    case get_top_state() of
        {ok, Trees} ->
            {ok, aeo_state_tree:get_oracles(From, Max, aec_trees:oracles(Trees))};
        error ->
            {error, no_state_trees}
    end.

-spec get_oracle(aec_keys:pubkey()) ->
    {ok, aeo_oracles:oracle()} | {error, not_found} | {error, no_state_trees}.
get_oracle(Pubkey) ->
    case get_top_state() of
        {ok, Trees} -> get_oracle(Pubkey, aec_trees:oracles(Trees));
        error -> {error, no_state_trees}
    end.

-spec get_oracle_queries(aec_keys:pubkey(), binary() | '$first', open | closed | all, non_neg_integer()) ->
    {ok, [aeo_query:query()]} | {error, no_state_trees}.
get_oracle_queries(Oracle, From, QueryType, Max) ->
    case get_top_state() of
        {ok, Trees} ->
            OT = aec_trees:oracles(Trees),
            {ok, aeo_state_tree:get_oracle_queries(Oracle, From, QueryType, Max, OT)};
        error -> {error, no_state_trees}
    end.

-spec get_oracle_query(aec_keys:pubkey(), aeo_query:id()) ->
    {ok, aeo_query:query()} | {error, not_found} | {error, no_state_trees}.
get_oracle_query(Pubkey, Id) ->
    case get_top_state() of
        {ok, Trees} -> get_oracle_query(Pubkey, Id, aec_trees:oracles(Trees));
        error -> {error, no_state_trees}
    end.

get_oracle(Pubkey, Trees) ->
    case aeo_state_tree:lookup_oracle(Pubkey, Trees) of
        {value, Oracle} -> {ok, Oracle};
        none -> {error, not_found}
    end.

get_oracle_query(Pubkey, Id, Trees) ->
    case aeo_state_tree:lookup_query(Pubkey, Id, Trees) of
        {value, Query} -> {ok, Query};
        none -> {error, not_found}
    end.

%%%===================================================================
%%% State channels
%%%===================================================================

-spec get_channel(aesc_channels:pubkey()) ->
                         {'ok', aesc_channels:channel()} |
                         {'error', 'no_state_trees'|'not_found'}.
get_channel(ChannelPubkey) ->
    case get_top_state() of
        {ok, Trees} ->
            get_channel(ChannelPubkey, Trees);
        error ->
            {error, no_state_trees}
    end.


-spec get_channel(aesc_channels:pubkey(), aec_trees:trees()) ->
                         {'ok', aesc_channels:channel()} |
                         {'error', 'no_state_trees'|'not_found'}.
get_channel(ChannelPubkey, Trees) ->
    case aesc_state_tree:lookup(ChannelPubkey, aec_trees:channels(Trees)) of
        {value, Channel} -> {ok, Channel};
        none -> {error, not_found}
    end.

-spec get_channel_at_hash(aesc_channels:pubkey(), binary()) ->
    {ok, aesc_channels:channel()} | {error, no_state_trees | not_found}.
get_channel_at_hash(ChannelPubkey, Hash) ->
    case get_block_state_partial(Hash, [channels]) of
        {ok, Trees} ->
            get_channel(ChannelPubkey, Trees);
        error -> {error, no_state_trees}
    end.
%%%===================================================================
%%% Name service
%%%===================================================================

-spec name_entry(binary()) ->
                        {'ok', map()} |
                        {'error', atom()}.
name_entry(Name) ->
    case get_top_state() of
        {ok, Trees} -> aens:get_name_entry(Name, aec_trees:ns(Trees));
        error -> {error, no_state_trees}
    end.

-spec resolve_name(binary(), binary()) ->
    {'ok', binary()} | {error, atom()}.
resolve_name(Key, Name) ->
    case get_top_state() of
        {ok, Trees} -> aens:resolve(Key, Name, aec_trees:ns(Trees));
        error -> {error, no_state_trees}
    end.

-spec resolve_namehash(binary(), binary()) ->
    {'ok', aeser_id:id()} | {error, atom()}.
resolve_namehash(Key, NameHash) ->
    case get_top_state() of
        {ok, Trees} -> aens:resolve_hash(Key, NameHash, aec_trees:ns(Trees));
        error -> {error, no_state_trees}
    end.

%%%===================================================================
%%% Contracts
%%%===================================================================

-spec get_contract(aec_keys:pubkey()) ->
                          {'ok', aect_contracts:contract()} |
                          {'error', atom()}.
get_contract(PubKey) ->
    case get_top_state() of
        {ok, Trees} ->
            ContractTree = aec_trees:contracts(Trees),
            try aect_state_tree:get_contract(PubKey, ContractTree) of
                Contract -> {ok, Contract}
            catch error:{not_present,_ContractKey} -> {error, not_present}
            end;
        error -> {error, no_state_trees}
    end.

-spec get_contract_with_code(aec_keys:pubkey()) ->
    {'ok', aect_contracts:contract(), binary()} |
    {'error', atom()}.
get_contract_with_code(PubKey) ->
    case get_contract(PubKey) of
        {ok, Contract} ->
            Code =
                case aect_contracts:code(Contract) of
                    {code, C} -> C;
                    {ref, Ref} ->
                        RefContractPK = aeser_id:specialize(Ref, contract),
                        {ok, RefContract} = get_contract(RefContractPK),
                        {code, C} = aect_contracts:code(RefContract),
                        C
                end,
            {ok, Contract, Code};
        {error, _} = Err -> Err
    end.

-spec get_contract_call(aect_contracts:id() | binary(), aect_call:id(), binary()) ->
                               {'ok', aect_call:call()} |
                               {'error', atom()}.
get_contract_call(ContractId, CallId, BlockHash) ->
    case get_block_state_partial(BlockHash, [calls]) of
        error -> {error, no_state_trees};
        {ok, Trees} ->
            CallTree = aec_trees:calls(Trees),
            case aect_call_state_tree:lookup_call(ContractId, CallId, CallTree) of
                none -> {error, call_not_found};
                {value, Call} -> {ok, Call}
            end
    end.

%%%===================================================================
%%% Generalized Accounts
%%%===================================================================

-spec get_ga_call(binary(), aect_call:id(), binary()) ->
                               {'ok', aega_call:call()} |
                               {'error', atom()}.
get_ga_call(Owner, AuthId, BlockHash) ->
    {value, Account} = get_account(Owner),
    {_, AuthCtPK}    = aeser_id:specialize(aec_accounts:ga_contract(Account)),
    CallId = aect_call:ga_id(AuthId, AuthCtPK),
    case get_block_state_partial(BlockHash, [calls]) of
        error -> {error, no_state_trees};
        {ok, Trees} ->
            CallTree = aec_trees:calls(Trees),
            case aect_call_state_tree:lookup_call(Owner, CallId, CallTree) of
                none -> {error, call_not_found};
                {value, Call} ->
                    GAId        = aect_call:caller_id(Call),
                    Id          = CallId,
                    GasPrice    = aect_call:gas_price(Call),
                    GasUsed     = aect_call:gas_used(Call),
                    Height      = aect_call:height(Call),
                    ReturnType  = aect_call:return_type(Call),
                    ReturnValue = aect_call:return_value(Call),
                    {ok, aega_call:new(GAId, Id, Height, GasPrice, GasUsed, ReturnType, ReturnValue)}
            end
    end.

%%%===================================================================
%%% Time summary.
%%%===================================================================
%%% TODO: This doesn't belong here.

get_top_N_blocks_time_summary(N) when is_integer(N), N > 0 ->
    case top_header() of
        undefined -> [];
        Header ->
            Prev = get_header(aec_headers:prev_hash(Header)),
            get_time_summary(Prev, N, Header, [])
    end.

get_time_summary(_, 0,_Header, Acc) ->
    lists:reverse(Acc);
get_time_summary(error,_N, Header, Acc) ->
    NewAcc = [{aec_headers:height(Header),
               aec_headers:time_in_msecs(Header),
               aec_headers:difficulty(Header)}|Acc],
    lists:reverse(NewAcc);
get_time_summary({ok, Header}, N, ParentHeader, Acc) ->
    Time = aec_headers:time_in_msecs(Header),
    NewAcc = [{aec_headers:height(ParentHeader),
               aec_headers:time_in_msecs(ParentHeader),
               aec_headers:time_in_msecs(ParentHeader) - Time,
               aec_headers:difficulty(ParentHeader)}
              | Acc],
    Prev = get_header(aec_headers:prev_hash(Header)),
    get_time_summary(Prev, N - 1, Header, NewAcc).

%%%===================================================================
%%% Transactions
%%%===================================================================

get_transactions_between(Hash1, Hash2) ->
    get_transactions_between(Hash1, Hash2, []).

get_transactions_between(Hash, Hash, Acc) ->
    {ok, Acc};
get_transactions_between(Hash, Root, Acc) ->
    case get_block(Hash) of
        {ok, Block} ->
            NewAcc =
                case aec_blocks:type(Block) of
                    key -> Acc;
                    micro ->
                     [{T,Hash} || T <- aec_blocks:txs(Block)] ++ Acc
                end,
            get_transactions_between(aec_blocks:prev_hash(Block), Root, NewAcc);
        error -> error
    end.

-spec find_tx_with_location(binary()) ->
                                   'none' |
                                   {binary() | 'mempool', aetx_sign:signed_tx()}.
find_tx_with_location(TxHash) ->
    case aec_db:find_tx_with_location(TxHash) of
        none -> none;
        {BlockHash, STx} when is_binary(BlockHash) -> {BlockHash, STx};
        {mempool, STx} -> {mempool, STx}
    end.

-spec find_tx_location(binary()) -> 'not_found' | 'none' | 'mempool' | binary().

find_tx_location(TxHash) ->
    aec_db:find_tx_location(TxHash).

%%%===================================================================
%%% Chain
%%%===================================================================

-spec top_header() -> 'undefined' | aec_headers:header().
top_header() ->
    case top_block_node() of
        #{ header := Header } ->
            Header;
        undefined ->
            undefined
    end.

-spec top_block() -> 'undefined' | aec_blocks:block().
top_block() ->
    case top_block_hash() of
        undefined -> undefined;
        Hash -> aec_db:get_block(Hash)
    end.

-spec top_block_node() -> 'undefined' | #{ hash := binary()
                                         , header := aec_headers:header() }.
top_block_node() ->
    aec_db:get_top_block_node().

-spec top_block_hash() -> 'undefined' | binary().
top_block_hash() ->
    aec_db:get_top_block_hash().

-spec top_header_hash_and_state() -> 'undefined'
                                   | {aec_headers:header(),
                                      aec_blocks:block_header_hash(),
                                      aec_trees:trees()}.
top_header_hash_and_state() ->
    case top_block_node() of
        undefined -> error;
        #{hash := Hash, header := Header} ->
            {Header, Hash, aec_db:get_block_state(Hash)}
    end.

-spec top_key_block() -> 'error' | {ok, aec_blocks:block()}.
top_key_block() ->
    case aec_db:get_top_block_hash() of
        Hash when is_binary(Hash) ->
            {ok, Block} = get_block(Hash),
            case aec_blocks:type(Block) of
                key -> {ok, Block};
                micro -> get_block(aec_blocks:prev_key_hash(Block))
            end;
        undefined ->
            error
    end.

-spec top_key_block_hash() -> 'undefined' | binary().
top_key_block_hash() ->
    case aec_db:get_top_block_hash() of
        Hash when is_binary(Hash) ->
            {ok, Block} = get_block(Hash),
            case aec_blocks:type(Block) of
                key -> Hash;
                micro -> aec_blocks:prev_key_hash(Block)
            end;
        undefined ->
            undefined
    end.

-spec top_block_with_state() -> 'undefined' | {aec_blocks:block(), aec_trees:trees()}.
top_block_with_state() ->
    case top_block_hash() of
        undefined -> undefined;
        Hash -> {aec_db:get_block(Hash), aec_db:get_block_state(Hash)}
    end.

-spec genesis_hash() -> 'undefined' | binary().
genesis_hash() ->
    aec_db:get_genesis_hash().

-spec genesis_block() -> 'undefined' | aec_blocks:block().
genesis_block() ->
    case aec_db:get_genesis_hash() of
        undefined -> undefined;
        Hash -> aec_db:get_block(Hash)
    end.

-spec genesis_header() -> 'undefined' | aec_headers:header().
genesis_header() ->
    case aec_db:get_genesis_hash() of
        undefined -> undefined;
        Hash -> aec_db:get_header(Hash)
    end.

-spec find_common_ancestor(binary(), binary()) ->
                                  {'ok', binary()} |
                                  {error, atom()}.
find_common_ancestor(Hash1, Hash2) when is_binary(Hash1), is_binary(Hash2) ->
    aec_chain_state:find_common_ancestor(Hash1, Hash2).

-spec prev_hash_from_hash(binary()) -> binary().
prev_hash_from_hash(Hash) ->
    {ok, Header} = get_header(Hash),
    aec_headers:prev_hash(Header).

-spec hash_is_in_main_chain(binary()) -> boolean().
hash_is_in_main_chain(Hash) when is_binary(Hash) ->
    aec_chain_state:hash_is_in_main_chain(Hash).

-spec hash_is_connected_to_genesis(binary()) -> boolean().
hash_is_connected_to_genesis(Hash) when is_binary(Hash) ->
    aec_chain_state:hash_is_connected_to_genesis(Hash).

-spec get_at_most_n_generation_headers_forward_from_hash(binary(), pos_integer()) ->
        {'ok', [aec_headers:header()]} | 'error'.

%%% @doc Get n headers forwards in chain. Returns headers old -> new
%%%      This function is only defined for headers in the main chain to avoid
%%%      ambiguity.
get_at_most_n_generation_headers_forward_from_hash(Hash, N)
        when is_binary(Hash), is_integer(N), N > 0 ->
    case top_header() of
        undefined -> error;
        TopHeader ->
            TopHeight = aec_headers:height(TopHeader),
            case get_header(Hash) of
                error -> error;
                {ok, Header} ->
                    Height = aec_headers:height(Header),
                    Delta = min(TopHeight - Height, N - 1),
                    case Delta < 0 of
                        true -> error;
                        false ->
                            {ok, HeaderDelta} = get_key_header_by_height(Height + Delta),
                            {ok, Headers} =
                                aec_chain_state:get_n_key_headers_backward_from(HeaderDelta,
                                                                                Delta + 1),

                            %% Validate that we were on the main fork.
                            case aec_headers:hash_header(hd(Headers)) =:= {ok, Hash} of
                                true -> {ok, Headers};
                                false -> error
                            end
                    end
            end
    end.

-spec get_n_generation_headers_backwards_from_hash(binary(), pos_integer()) ->
                                           {'ok', [aec_headers:header()]} |
                                           'error'.
%%% @doc Get n headers backwards in chain. Returns headers old -> new
get_n_generation_headers_backwards_from_hash(Hash, N) when is_binary(Hash), is_integer(N), N > 0 ->
    case get_header(Hash) of
        {ok, Header} -> aec_chain_state:get_n_key_headers_backward_from(Header, N);
        error        -> error
    end.

%%%===================================================================
%%% Difficulty
%%%===================================================================

-spec difficulty_at_top_block() -> {'ok', float()} | {'error', atom()}.
difficulty_at_top_block() ->
    case top_block_hash() of
        undefined -> {error, no_top};
        Hash -> difficulty_at_hash(Hash)
    end.

-spec difficulty_at_hash(binary()) -> {'ok', float()} | {'error', atom()}.
difficulty_at_hash(Hash) ->
    difficulty_at_hash(Hash, 0).

difficulty_at_hash(Hash, Acc) ->
    case aec_db:find_block_difficulty(Hash) of
        {value, Difficulty} -> {ok, Acc + Difficulty};
        none -> {error, 'not_rooted'}
    end.

%%%===================================================================
%%% State
%%%===================================================================

get_top_state() ->
    case top_block_hash() of
        undefined -> error;
        Hash -> get_block_state(Hash)
    end.

get_block_state(Hash) ->
    case aec_db:find_block_state(Hash) of
        {value, Trees} -> {ok, Trees};
        none -> error
    end.

get_block_state_partial(Hash, Elements) ->
    get_block_state_partial(Hash, Elements, true).

get_block_state_partial(Hash, Elements, DirtyBackend) ->
    case aec_db:find_block_state_partial(Hash, DirtyBackend, Elements) of
        {value, Trees} -> {ok, Trees};
        none -> error
    end.

%%%===================================================================
%%% Blocks
%%%===================================================================

-spec has_block(binary()) -> boolean().
has_block(Hash) ->
    aec_db:has_block(Hash).

-spec get_block(binary()) -> {'ok', aec_blocks:block()} | 'error'.
get_block(Hash) when is_binary(Hash) ->
    case aec_db:find_block(Hash) of
        none -> error;
        {value, Block} -> {ok, Block}
    end.

-spec get_key_block_by_height(non_neg_integer()) ->
                                 {'ok', aec_blocks:block()} |
                                 {'error', 'chain_too_short' | 'block_not_found'}.
get_key_block_by_height(Height) when is_integer(Height), Height >= 0 ->
    case aec_chain_state:get_key_block_hash_at_height(Height) of
        error -> {error, chain_too_short};
        {ok, Hash} ->
            case get_block(Hash) of
                {ok, _} = Res -> Res;
                error -> {error, block_not_found}
            end
    end.

%%%===================================================================
%%% Generations
%%%===================================================================

-spec get_current_generation() -> 'error' | {'ok', generation()}.
get_current_generation() ->
    get_generation_(top_block_node()).

get_generation_(#{hash := Hash, header := Header}) ->
    case aec_headers:type(Header) of
        key ->
            {ok, #{ key_block => aec_blocks:new_key_from_header(Header), micro_blocks => [], dir => forward }};
        micro ->
            Index = maps:from_list(aec_db:find_headers_and_hash_at_height(aec_headers:height(Header))),
            get_generation_with_header_index(Hash, Index)
    end;
get_generation_(Hash) when is_binary(Hash) ->
    case get_header(Hash) of
        error -> error;
        {ok, Header} ->
            get_generation_(#{hash => Hash, header => Header})
    end;
get_generation_(_) ->
    error.

get_generation_with_header_index(TopHash, Index) ->
    get_generation_with_header_index(TopHash, Index, []).

get_generation_with_header_index(TopHash, Index, MBs) ->
    H = maps:get(TopHash, Index),
    case aec_headers:type(H) of
        key -> {ok, #{ key_block => aec_blocks:new_key_from_header(H), micro_blocks => MBs, dir => forward }};
        micro ->
            Block = aec_db:get_block_from_micro_header(TopHash, H),
            get_generation_with_header_index(aec_headers:prev_hash(H), Index, [Block | MBs])
    end.

-spec get_generation_by_hash(binary(), forward | backward) ->
        'error' | {'ok', generation()}.
get_generation_by_hash(KeyBlockHash, Dir) ->
    case aec_db:find_key_block(KeyBlockHash) of
        none -> error;
        {value, KeyBlock} when Dir == backward ->
            case get_generation_(aec_blocks:prev_hash(KeyBlock)) of
                error         -> error;
                {ok, G = #{}} -> {ok, G#{ key_block => KeyBlock, dir => Dir }}
            end;
        {value, KeyBlock} when Dir == forward ->
            case get_generation_by_height(aec_blocks:height(KeyBlock), forward) of
                {ok, G = #{ key_block := KeyBlock }} -> {ok, G};
                {ok, _G}                             -> error; %% KeyBlockHash not on chain!!
                error                                -> error
            end
    end.

-spec get_generation_by_height(aec_blocks:height(), forward | backward) ->
        'error' | {'ok', generation()}.
get_generation_by_height(Height, backward) ->
    case get_key_block_by_height(Height) of
        {error, _Reason} -> error;
        {ok, KeyBlock} ->
            case get_generation_(aec_blocks:prev_hash(KeyBlock)) of
                error         -> error;
                {ok, G = #{}} -> {ok, G#{ key_block => KeyBlock, dir => backward }}
            end
    end;
get_generation_by_height(Height, forward) ->
    #{header := TopHeader} = TopNode = top_block_node(),
    TopHeight = aec_headers:height(TopHeader),
    if TopHeight < Height  -> error;
       TopHeight == Height -> get_generation_(TopNode);
       true                ->
           case get_key_block_by_height(Height + 1) of
               {error, _Reason} -> error;
               {ok, KeyBlock}   -> get_generation_(aec_blocks:prev_hash(KeyBlock))
           end
    end.

%%%===================================================================
%%% Headers
%%%===================================================================

-spec get_header(binary()) -> {'ok', aec_headers:header()} | 'error'.
get_header(Hash) when is_binary(Hash) ->
    case aec_db:find_header(Hash) of
        none -> error;
        {value, Header} -> {ok, Header}
    end.

-spec dirty_get_header(binary()) -> {'ok', aec_headers:header()} | 'error'.
dirty_get_header(Hash) when is_binary(Hash) ->
    case aec_db:dirty_find_header(Hash) of
        none -> error;
        {value, Header} -> {ok, Header}
    end.

-spec get_key_header_by_height(non_neg_integer()) ->
                              {'ok', aec_headers:header()} | {'error', 'chain_too_short'}.
get_key_header_by_height(Height) when is_integer(Height), Height >= 0 ->
    case aec_chain_state:get_key_block_hash_at_height(Height) of
        error -> {error, chain_too_short};
        {ok, Hash} -> get_header(Hash)
    end.

%%%===================================================================
%%% Statistics
%%%===================================================================

-spec sum_tokens_at_height(aec_blocks:height()) ->
                                  {error, 'chain_too_short'}
                                | {ok, #{ 'accounts'         => non_neg_integer()
                                        , 'contracts'        => non_neg_integer()
                                        , 'contract_oracles' => non_neg_integer()
                                        , 'locked'           => non_neg_integer()
                                        , 'oracles'          => non_neg_integer()
                                        , 'oracle_queries'   => non_neg_integer()
                                        , 'pending_rewards'  => non_neg_integer()
                                        , 'total'            => non_neg_integer()
                                        }}.


sum_tokens_at_height(Height) ->
    %% Wrap in transaction for speed.
    %% TODO: This could be done dirty
    aec_db:ensure_transaction(fun() -> int_sum_tokens_at_height(Height) end).

int_sum_tokens_at_height(Height) ->
    case aec_chain_state:get_key_block_hash_at_height(Height) of
        error -> {error, chain_too_short};
        {ok, Hash} ->
            {ok, Trees} = get_block_state(Hash),
            Sum = aec_trees:sum_total_coin(Trees),
            Sum1 = Sum#{pending_rewards => sum_pending_rewards(Height, Hash)},
            Total = maps:fold(fun(_, X, Acc) -> Acc + X end, 0, Sum1),
            {ok, Sum1#{ total => Total }}
    end.

sum_pending_rewards(0,_Hash) ->
    0;
sum_pending_rewards(Height, Hash) when Height > 0 ->
    %% Rewards for a generation is given for the closing of the generation,
    %% so we need stop at the closing key block (hence the +1).
    StopHeight = max(Height - aec_governance:beneficiary_reward_delay() + 1, 1),
    sum_pending_rewards(Hash, StopHeight, 0).

sum_pending_rewards(Hash, StopHeight, Acc) ->
    {ok, Header}  = get_header(Hash),
    Height        = aec_headers:height(Header),
    {value, Fees} = aec_db:find_block_fees(Hash),
    Coinbase      = aec_coinbase:coinbase_at_height(Height),
    Acc1 = Acc + Fees + Coinbase,
    case Height > StopHeight of
        true ->
            PrevKeyHash = aec_headers:prev_key_hash(Header),
            sum_pending_rewards(PrevKeyHash, StopHeight, Acc1);
        false ->
            Acc1
    end.

