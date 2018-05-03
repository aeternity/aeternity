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
        , find_transaction_in_main_chain_or_mempool/1
        , genesis_block/0
        , genesis_hash/0
        , genesis_header/0
        , get_block/1
        , get_block_by_height/1
        , get_block_range_by_hash/2
        , get_block_range_by_height/2
        , get_block_state/1
        , get_header/1
        , get_header_by_height/1
        , get_n_headers_backwards_from_hash/2
        , get_at_most_n_headers_forward_from_hash/2
        , get_top_N_blocks_time_summary/1
        , get_transactions_between/2
        , has_block/1
        , hash_is_in_main_chain/1
        , hash_is_connected_to_genesis/1
        , prev_hash_from_hash/1
        , top_block/0
        , top_block_hash/0
        , top_block_with_state/0
        , top_header/0
        , get_top_and_key_with_state/0
        ]).

%%% Accounts API
-export([ all_accounts_balances_at_hash/1
        , get_account/1
        , get_account_at_hash/2
        ]).

%%% NS API
-export([ name_entry/1
        , resolve_name/2
        ]).

%%% Oracles API
-export([ get_open_oracle_queries/3
        , get_oracles/2
        ]).

%%% Contracts API
-export([ get_contract/1
        ]).

%%% Channels API
-export([ get_channel/1
        , get_channel/2
        ]).

%%% trees
-export([ get_top_state/0
        ]).

%%% Difficulty API
-export([ difficulty_at_hash/1
        , difficulty_at_top_block/0
        ]).

%%% For tests
-export([ max_block_range/0
        ]).


-include("common.hrl"). %% Just for types

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
    case get_block_state(Hash) of
        {ok, Trees} ->
            aec_accounts_trees:lookup(PubKey, aec_trees:accounts(Trees));
        error -> {error, no_state_trees}
    end.

-spec all_accounts_balances_at_hash(binary()) ->
                                           {'ok', [{pubkey(), non_neg_integer()}]}
                                               | {'error', 'no_state_trees'}.
all_accounts_balances_at_hash(Hash) when is_binary(Hash) ->
    case get_block_state(Hash) of
        {ok, Trees} ->
            ATrees = aec_trees:accounts(Trees),
            {ok, aec_accounts_trees:get_all_accounts_balances(ATrees)};
        error ->
            {error, no_state_trees}
    end.

%%%===================================================================
%%% Oracles
%%%===================================================================

-spec get_open_oracle_queries(pubkey(), binary() | '$first', non_neg_integer()) ->
                                     {'ok', list()} |
                                     {'error', 'no_state_trees'}.
get_open_oracle_queries(Oracle, From, Max) ->
    case get_top_state() of
        {ok, Trees} ->
            OT = aec_trees:oracles(Trees),
            {ok, aeo_state_tree:get_open_oracle_queries(Oracle, From, Max, OT)};
        error -> {error, no_state_trees}
    end.

-spec get_oracles(binary() | '$first', non_neg_integer()) ->
                         {'ok', list()} |
                         {'error', 'no_state_trees'}.
get_oracles(From, Max) ->
    case get_top_state() of
        {ok, Trees} ->
            {ok, aeo_state_tree:get_oracles(From, Max, aec_trees:oracles(Trees))};
        error ->
            {error, no_state_trees}
    end.

%%%===================================================================
%%% State channels
%%%===================================================================

-spec get_channel(aesc_channels:id()) ->
                         {'ok', aesc_channels:channel()} |
                         {'error', 'no_state_trees'|'not_found'}.
get_channel(ChannelId) ->
    case get_top_state() of
        {ok, Trees} ->
            get_channel(ChannelId, Trees);
        error ->
            {error, no_state_trees}
    end.


-spec get_channel(aesc_channels:id(), aec_trees:trees()) ->
                         {'ok', aesc_channels:channel()} |
                         {'error', 'no_state_trees'|'not_found'}.
get_channel(ChannelId, Trees) ->
    case aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees)) of
        {value, Channel} -> {ok, Channel};
        none -> {error, not_found}
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

-spec resolve_name(atom(), binary()) -> {'ok', binary()} |
                                        {error, atom()}.
resolve_name(Type, Name) ->
    case get_top_state() of
        {ok, Trees} -> aens:resolve(Type, Name, aec_trees:ns(Trees));
        error -> {error, no_state_trees}
    end.

%%%===================================================================
%%% Contracts
%%%===================================================================

-spec get_contract(pubkey()) ->
                          {'ok', map()} |
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
            NewAcc = [{T,Hash} || T <- aec_blocks:txs(Block)] ++ Acc,
            get_transactions_between(aec_blocks:prev_hash(Block), Root, NewAcc);
        error -> error
    end.

-spec find_transaction_in_main_chain_or_mempool(binary()) ->
                                                   'none' |
                                                   {binary() | 'mempool', aetx_sign:signed_tx()}.
find_transaction_in_main_chain_or_mempool(TxHash) ->
    case aec_db:find_transaction_in_main_chain_or_mempool(TxHash) of
        none -> none;
        {BlockHash, STx} when is_binary(BlockHash) -> {BlockHash, STx};
        {mempool, [STx]} -> {mempool, STx};
        {mempool, [STx|_] = TXs} ->
            lager:info("More than one signed version of the same transaction: ~p",
                       [TXs]),
            %% TODO: The api should return all versions
            {mempool, STx}
    end.

%%%===================================================================
%%% Chain
%%%===================================================================

-spec top_header() -> 'undefined' | aec_headers:header().
top_header() ->
    case top_block_hash() of
        undefined -> undefined;
        Hash -> aec_db:get_header(Hash)
    end.

-spec top_block() -> 'undefined' | aec_blocks:block().
top_block() ->
    case top_block_hash() of
        undefined -> undefined;
        Hash -> aec_db:get_block(Hash)
    end.

-spec top_block_hash() -> 'undefined' | binary().
top_block_hash() ->
    aec_db:get_top_block_hash().

-spec top_block_with_state() -> 'undefined' | {aec_blocks:block(), aec_trees:trees()}.
top_block_with_state() ->
    case top_block_hash() of
        undefined -> undefined;
        Hash -> {aec_db:get_block(Hash), aec_db:get_block_state(Hash)}
    end.

get_top_and_key_with_state() ->
    {TopBlock, TopBlockState} = top_block_with_state(),
    CurrentKeyBlock = case aec_blocks:type(TopBlock) of
                          micro -> case aec_db:get_header(aec_blocks:key_hash(TopBlock)) of
                                       none -> error_missing_block;
                                       Header -> Header
                                   end;
                          key -> TopBlock
                      end,
    {TopBlock, CurrentKeyBlock, TopBlockState}.

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

-spec get_at_most_n_headers_forward_from_hash(binary(), pos_integer()) ->
                                                 {'ok', [aec_headers:header()]} |
                                                 'error'.

%%% @doc Get n headers forwards in chain. Returns headers old -> new
%%%      This function is only defined for headers in the main chain to avoid
%%%      ambiguity.
get_at_most_n_headers_forward_from_hash(Hash, N) when is_binary(Hash), is_integer(N), N > 0 ->
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
                            Res = {ok, _} = get_header_by_height(Height + Delta),
                            {ok, Headers} = get_n_headers_backwards_from_hash(Res, Delta + 1, []),
                            %% Validate that we were on the main fork.
                            case aec_headers:hash_header(hd(Headers)) =:= {ok, Hash} of
                                true -> {ok, Headers};
                                false -> error
                            end
                    end
            end
    end.

-spec get_n_headers_backwards_from_hash(binary(), pos_integer()) ->
                                           {'ok', [aec_headers:header()]} |
                                           'error'.
%%% @doc Get n headers backwards in chain. Returns headers old -> new
get_n_headers_backwards_from_hash(Hash, N) when is_binary(Hash), is_integer(N), N > 0 ->
    get_n_headers_backwards_from_hash(get_header(Hash), N, []).

get_n_headers_backwards_from_hash(_, 0, Acc) ->
    {ok, Acc};
get_n_headers_backwards_from_hash({ok, Header}, N, Acc) ->
    PrevHash = aec_headers:prev_hash(Header),
    NewAcc = [Header|Acc],
    get_n_headers_backwards_from_hash(get_header(PrevHash), N - 1, NewAcc);
get_n_headers_backwards_from_hash(error,_N,_Acc) ->
    error.

%%%===================================================================
%%% Get a block range
%%%===================================================================
-define(MAXIMUM_BLOCK_RANGE, 10).

max_block_range() -> ?MAXIMUM_BLOCK_RANGE.

get_block_range_by_height(H1, H2) ->
    Fun = fun() -> {get_block_by_height(H1), get_block_by_height(H2)} end,
    case aec_db:ensure_transaction(Fun) of
        {{ok, B1}, {ok, B2}} -> get_block_range(H1, H2, B1, B2);
        {{error, Err}, _} -> {error, Err};
        {_, {error, Err}} -> {error, Err}
    end.

get_block_range_by_hash(Hash1, Hash2) ->
    case {get_block(Hash1), get_block(Hash2)} of
        {{ok, B1}, {ok, B2}} ->
            H1 = aec_blocks:height(B1),
            H2 = aec_blocks:height(B2),
            get_block_range(H1, H2, B1, B2);
        {error, _} -> {error, block_not_found};
        {_, error} -> {error, block_not_found}
    end.

get_block_range(Height1, Height2, B1, B2) ->
    case validate_block_range(Height1, Height2) of
        ok -> get_block_range(Height1, Height2, B1, {ok, B2}, []);
        {error, _} = E -> E
    end.

get_block_range(_Height1,_Height2,_B, error,_Acc) ->
    {error, block_not_found};
get_block_range(Height, Height, B, {ok, B}, Acc) ->
    {ok, [B|Acc]};
get_block_range(Height, Height, _, _,_Acc) ->
    {error, invalid_range};
get_block_range(Height1, Height2, B1, {ok, B2}, Acc) when Height2 > Height1 ->
    NewAcc = [B2|Acc],
    PrevHash = aec_blocks:prev_hash(B2),
    get_block_range(Height1, Height2 - 1, B1, get_block(PrevHash), NewAcc).

validate_block_range(HeightFrom, HeightTo) when HeightFrom > HeightTo ->
    {error, invalid_range};
validate_block_range(HeightFrom, HeightTo) ->
    case (HeightTo - HeightFrom) > max_block_range() of
        true ->
            {error, range_too_big};
        false ->
            ok
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
        none ->
            case get_header(Hash) of
                error -> {error, 'not_rooted'};
                {ok, Header} ->
                    NewAcc = Acc + aec_headers:difficulty(Header),
                    PrevHash = aec_headers:prev_hash(Header),
                    difficulty_at_hash(PrevHash, NewAcc)
            end
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

-spec get_block_by_height(non_neg_integer()) ->
                                 {'ok', aec_blocks:block()} |
                                 {'error', 'chain_too_short' | 'block_not_found'}.
get_block_by_height(Height) when is_integer(Height), Height >= 0 ->
    case aec_chain_state:get_hash_at_height(Height) of
        error -> {error, chain_too_short};
        {ok, Hash} ->
            case get_block(Hash) of
                {ok, _} = Res -> Res;
                error -> {error, block_not_found}
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

-spec get_header_by_height(non_neg_integer()) ->
                              {'ok', aec_headers:header()} | {'error', 'chain_too_short'}.
get_header_by_height(Height) when is_integer(Height), Height >= 0 ->
    case aec_chain_state:get_hash_at_height(Height) of
        error -> {error, chain_too_short};
        {ok, Hash} -> get_header(Hash)
    end.
