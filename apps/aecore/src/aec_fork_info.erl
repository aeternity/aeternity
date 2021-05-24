%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Aeternity Anstalt
%%% @doc
%%% Metadata about the given fork
%%% ForkId is local with regards to the node and for recently gossiped blocks
%%% nodes might disagree about the exact value - fork_id is eventually consistent
%%% across the network.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_fork_info).
-author("ra").

-export([new/4
        , decompose/1
        , difficulty/1
        , id/1, id/2
        , fees/1, fees/2
        , fraud/1, fraud/2
        , fees_fraud/1, fees_fraud/3
        , fees_fraud_difficulty/4]).

-export_type([fork_info/0]).

-type hash() :: aec_hash:hash().
-type difficulty() :: integer().
-type fees() :: non_neg_integer().

-record(fork_info, {
    fork_id :: hash()
    , difficulty = 0 :: difficulty()
    , fees = 0 :: fees()
    , fraud = false :: boolean()
}).
-opaque fork_info() :: #fork_info{}.


%% API

-spec new(hash(), difficulty(), fees(), boolean()) -> fork_info().
new(ForkId, Difficulty, Fees, Fraud) ->
    #fork_info
    {
        fork_id = ForkId,
        difficulty = Difficulty,
        fees = Fees,
        fraud = Fraud
    }.

-spec decompose(fork_info()) -> {hash(), difficulty(), fees(), boolean()}.
decompose(ForkInfo) ->
    #fork_info
    {
        fork_id = Id,
        difficulty = Difficulty,
        fees = Fees,
        fraud = Fraud
    } = ForkInfo,
    {Id, Difficulty, Fees, Fraud}.

-spec difficulty(fork_info()) -> difficulty().
difficulty(#fork_info{difficulty = Difficulty}) -> Difficulty.

-spec id(fork_info()) -> hash().
id(#fork_info{fork_id = Id}) -> Id.

-spec id(fork_info(), hash()) -> fork_info().
id(ForkInfo, Id) -> ForkInfo#fork_info{fork_id = Id}.

-spec fees(fork_info()) -> fees().
fees(#fork_info{fees = Fees}) -> Fees.

-spec fees(fork_info(), fees()) -> fork_info().
fees(ForkInfo, Fees) -> ForkInfo#fork_info{fees = Fees}.

-spec fraud(fork_info()) -> boolean().
fraud(#fork_info{fraud = Fraud}) -> Fraud.

-spec fraud(fork_info(), boolean()) -> fork_info().
fraud(ForkInfo, Fraud) -> ForkInfo#fork_info{fraud = Fraud}.

-spec fees_fraud(fork_info()) -> {fees(), boolean()}.
fees_fraud(#fork_info{fees = Fees, fraud = Fraud}) -> {Fees, Fraud}.

-spec fees_fraud(fork_info(), fees(), boolean()) -> fork_info().
fees_fraud(ForkInfo, Fees, Fraud) ->
    ForkInfo#fork_info{fees = Fees, fraud = Fraud}.

-spec fees_fraud_difficulty(fork_info(), fees(), boolean(), difficulty()) ->
    fork_info().
fees_fraud_difficulty(ForkInfo, Fees, Fraud, Difficulty) ->
    ForkInfo#fork_info{fees = Fees, fraud = Fraud, difficulty = Difficulty}.


%% ***

%%maybe_set_new_fork_id(Node, ForkInfoIn, State) ->
%%    case {get_top_block_hash(State), node_prev_hash(Node)} of
%%        {H, H} ->
%%%% When extending the existing top it doesn't make sense to perform a full scan
%%%% of 2 generations. PrevHash(Node) =:= TopHash() can only be true if we have NO siblings
%%%% This will be the case when syncing and will allow us to skip the expensive DB scan
%%            {ForkInfoIn, [], []};
%%        _ ->
%%%% When benchmarking it turned out that this is very expensive to calculate
%%%% Execute it only when it's possible for siblings to exist
%%            case db_sibling_blocks(Node) of
%%                #{key_siblings := [],
%%                    micro_siblings := []} ->
%%                    {ForkInfoIn, [], []};
%%                #{micro_siblings := MicSibs
%%                    , key_siblings := KeySibs} ->
%%                    {ForkInfoIn#fork_info{fork_id = node_hash(Node)}, MicSibs, KeySibs}
%%            end
%%    end.
%%
%%get_state_trees_in(PrevNode, DirtyBackend) ->
%%    PrevHash = node_hash(PrevNode),
%%    case db_find_state(PrevHash, DirtyBackend) of
%%        {ok, Trees, ForkInfo} ->
%%            %% For key blocks, reset:
%%            %% 1. Fees, to accumulate new fees for generation
%%            %% 2. Fraud, since a new generation starts fresh
%%            case node_type(PrevNode) of
%%                key -> {ok, Trees, ForkInfo#fork_info{fees = 0,
%%                    fraud = false}};
%%                micro -> {ok, Trees, ForkInfo}
%%            end;
%%        error -> error
%%    end.
%%
%%apply_and_store_state_trees(Node, PrevNode, PrevKeyNode, TreesIn, ForkInfoIn, State) ->
%%    {Trees, Fees, Events} = apply_node_transactions(Node, PrevNode, PrevKeyNode, TreesIn, ForkInfoIn, State),
%%    assert_state_hash_valid(Trees, Node),
%%    DifficultyOut = ForkInfoIn#fork_info.difficulty + node_difficulty(Node),
%%    Fraud = update_fraud_info(ForkInfoIn, Node, State),
%%    ForkInfoInNode = ForkInfoIn#fork_info{fees = Fees
%%        , difficulty = DifficultyOut
%%        , fraud = Fraud
%%    },
%%    ok = db_put_state(node_hash(Node), Trees, ForkInfoInNode),
%%    ok = db_put_found_pof(Node, maps:get(found_pof, State)),
%%    {DifficultyOut, Events}.
%%
%%db_put_state(Hash, Trees, ForkInfo) when is_binary(Hash) ->
%%    #fork_info{difficulty = Difficulty
%%        , fork_id = ForkId
%%        , fees = Fees
%%        , fraud = Fraud
%%    } = ForkInfo,
%%    ok = aec_db:write_block_state(Hash, Trees, Difficulty, ForkId, Fees, Fraud).
%%
%%db_find_state(Hash, DirtyBackend) ->
%%    case aec_db:find_block_state_and_data(Hash, DirtyBackend) of
%%        {value, Trees, Difficulty, ForkId, Fees, Fraud} ->
%%            {ok, Trees,
%%                #fork_info{difficulty = Difficulty
%%                    , fork_id = ForkId
%%                    , fees = Fees
%%                    , fraud = Fraud
%%                }
%%            };
%%        none -> error
%%    end.

%% ***
