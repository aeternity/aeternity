%%% -*- erlang-indent-level:4; indent-tabs-mode:nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity
%%% @doc
%%% Manage interaction with hyperchain parent chain
%%% @end
%%%-------------------------------------------------------------------

-module(aec_parent_connector).

%% Functionality:
%% - at intervals check parent chain to understand when new top block is
%%   available.
%% - New top block from parent chain should include commitment transactions
%%   on the parent chain.
%% - Call consensus smart contract
%%
-behaviour(gen_server).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([start_link/0, start_link/3, stop/0]).

%% Use in test only
-export([trigger_fetch/0]).

-export([%% async getting of blocks
         request_block_by_hash/1,
         request_block_by_height/1,
         request_top/0,
         %% blocking getting of blocks
         fetch_block_by_hash/1,
         fetch_block_by_height/1]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SEED_BYTES, 16).


%% Loop state
-record(state,
    {
        parent_conn_mod = aehttpc_btc,
        fetch_interval = 10000, % Interval for parent top change checks
        parent_hosts = [],
        parent_top = not_yet_fetched :: not_yet_fetched | aec_parent_chain_block:block(),
        rpc_seed = crypto:strong_rand_bytes(?SEED_BYTES) % BTC Api only
    }).
-type state() :: #state{}.


%%%=============================================================================
%%% API
%%%=============================================================================
-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link() ->
    FetchInterval = 10000,
    ParentHosts = [#{host => <<"127.0.0.1">>,
                    port => 3013,
                    user => "test",
                    password => "Pass"
                    }],
    ParentConnMod = aehttpc_aeternity,
    start_link(ParentConnMod, FetchInterval, ParentHosts).

%% Start the parent connector process
%% ParentConnMod :: atom() - module name of the http client module aehttpc_btc | aehttpc_aeternity
%% FetchInterval :: integer() | on_demand - millisecs between parent chain checks or when asked (useful for test)
%% ParentHosts :: [#{host => Host, port => Port, user => User, password => Pass}]
-spec start_link(atom(), integer() | on_demand, [map()]) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(ParentConnMod, FetchInterval, ParentHosts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ParentConnMod, FetchInterval, ParentHosts], []).

stop() ->
    gen_server:stop(?MODULE).

trigger_fetch() ->
    gen_server:call(?SERVER, trigger_fetch).

request_block_by_hash(Hash) ->
    gen_server:cast(?SERVER, {request_block_by_hash, Hash}).

request_block_by_height(Height) ->
    gen_server:cast(?SERVER, {request_block_by_height, Height}).

request_top() ->
    ?SERVER ! check_parent.

%% this blocks the caller process, use with caution
fetch_block_by_hash(Hash) ->
    gen_server:call(?SERVER, {fetch_block_by_hash, Hash}).

%% this blocks the caller process, use with caution
fetch_block_by_height(Height) ->
    gen_server:call(?SERVER, {fetch_block_by_height, Height}).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init([any()]) -> {ok, #state{}}.
init([ParentConnMod, FetchInterval, ParentHosts]) ->
    if is_integer(FetchInterval) ->
        erlang:send_after(FetchInterval, self(), check_parent);
        true -> ok
    end,
    {ok, #state{parent_conn_mod = ParentConnMod,
                fetch_interval = FetchInterval,
                parent_hosts = ParentHosts}}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call(trigger_fetch, _From, State) ->
    self() ! check_parent,
    Reply = ok,
    {reply, Reply, State};
handle_call({fetch_block_by_hash, Hash}, _From, State) ->
    Reply = handle_fetch_block(fun fetch_block_by_hash/4, Hash, State),
    {reply, Reply, State};
handle_call({fetch_block_by_height, Height}, _From, State) ->
    Reply = handle_fetch_block(fun fetch_block_by_height/4, Height, State),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({request_block_by_hash, Hash}, State) ->
    case handle_fetch_block(fun fetch_block_by_hash/4, Hash, State) of
        {ok, Block} -> aec_parent_chain_cache:post_block(Block);
        {error, not_found} -> pass;
        {error, no_parent_chain_agreement} -> pass
    end,
    {noreply, State};
handle_cast({request_block_by_height, Height}, State) ->
    case handle_fetch_block(fun fetch_block_by_height/4, Height, State) of
        {ok, Block} -> aec_parent_chain_cache:post_block(Block);
        {error, not_found} -> pass;
        {error, no_parent_chain_agreement} -> pass
    end,
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(check_parent, #state{parent_hosts = ParentNodes,
                                parent_conn_mod = Mod,
                                parent_top = ParentTop,
                                fetch_interval = FetchInterval,
                                rpc_seed = Seed} = State) ->
    %% Parallel fetch top block from all configured parent chain nodes
    ParentTop1 =
        case fetch_parent_tops(Mod, ParentNodes, Seed) of
            {ok, ParentTop, _} ->
                %% No change, just check again later
                ParentTop;
            {ok, NewParentTop, _Node} ->
                %% Fetch the commitment Txs in the parent block from a node
                %% that had the majority answer
                aec_parent_chain_cache:post_block(NewParentTop),
                %_Commitments = fetch_commitments(Mod, Node, Seed,
                %                                 aec_parent_chain_block:hash(NewParentTop))
                %% Commitments may include varying view on what is the latest 
                %%   block.
                %% Commitments include:
                %%   [{Committer, Committers view of child chain top hash}]
                %% - Run the algorithm to derive the consensus top block
                %% - Call the smart contract to elect new leader.
                %% - Notify conductor of new status
                NewParentTop;
            {error, no_parent_chain_agreement} ->
                lager:warning("Parent nodes are unable to reach consensus", []),
                ParentTop
        end,
    if is_integer(FetchInterval) ->
        erlang:send_after(FetchInterval, self(), check_parent);
        true -> ok
    end,
    {noreply, State#state{rpc_seed = increment_seed(Seed),
                          parent_top = ParentTop1}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
fetch_parent_tops(Mod, ParentNodes, Seed) ->
    FetchFun =
        fun(Host, Port, User, Password) ->
            Mod:get_latest_block(Host, Port, User, Password, Seed)
        end,
    Fun = fun(Parent) ->fetch_block(FetchFun, Parent) end,
    {Good, Errors} = aeu_lib:pmap(Fun, ParentNodes, 10000),
    responses_consensus(Good, Errors, length(ParentNodes)).

fetch_block_by_hash(Hash, Mod, ParentNodes, Seed) ->
    FetchFun =
        fun(Host, Port, User, Password) ->
            Mod:get_header_by_hash(Hash, Host, Port, User, Password, Seed)
        end,
    Fun = fun(Parent) -> fetch_block(FetchFun, Parent) end,
    {Good, Errors} = aeu_lib:pmap(Fun, ParentNodes, 10000),
    responses_consensus(Good, Errors, length(ParentNodes)).

fetch_block_by_height(Height, Mod, ParentNodes, Seed) ->
    FetchFun =
        fun(Host, Port, User, Password) ->
            Mod:get_header_by_height(Height, Host, Port, User, Password, Seed)
        end,
    Fun = fun(Parent) -> fetch_block(FetchFun, Parent) end,
    {Good, Errors} = aeu_lib:pmap(Fun, ParentNodes, 10000),
    responses_consensus(Good, Errors, length(ParentNodes)).

fetch_block(FetchFun, #{host := Host, port := Port,
                        user := User, password := Password} = Node) ->
    case FetchFun(Host, Port, User, Password) of
        {ok, BlockHash, PrevHash, Height} ->
            Top = aec_parent_chain_block:new(BlockHash, Height, PrevHash),
            {ok, {Top, Node}};
        Err ->
            Err
    end.

%% Check:
%% * multiple successful replies
%% * majority agree on a block
responses_consensus(Good0, _Errors, TotalCount) ->
    MinRequired = TotalCount div 2,
    Good = [{Top, Node} || {ok, {Top, Node}} <- Good0],
    Counts = lists:foldl(fun({Top, _Node}, Acc) ->
                            Fun = fun(V) -> V + 1 end,
                            maps:update_with(Top, Fun, 1, Acc)
                        end, #{}, Good),
    NotFoundsCnt = length([1 || {error, not_found} <- Good0]),
    case maps:size(Counts) =:= 0 of
        true when NotFoundsCnt > MinRequired ->
            {error, not_found};
        true ->
            {error, no_parent_chain_agreement};
        false ->
            {MostReturnedTop, Qty} = lists:last(lists:keysort(2, maps:to_list(Counts))),
            %% Need Qty to be > half of the total number of configured nodes ??
            if Qty > MinRequired ->
                {_, Node} = lists:keyfind(MostReturnedTop, 1, Good),
                {ok, MostReturnedTop, Node};
            true ->
                case NotFoundsCnt > MinRequired of
                    true -> {error, not_found};
                    false -> {error, no_parent_chain_agreement}
                end
        end
    end.

%%fetch_commitments(Mod, #{host := Host, port := Port,
%%                        user := User, password := Password}, Seed, NewParentHash) ->
%%    Mod:get_commitment_tx_in_block(Host, Port, User, Password, Seed, NewParentHash).

increment_seed(<<Num:?SEED_BYTES/unsigned-integer-unit:8>>) ->
    <<(Num + 1):?SEED_BYTES/unsigned-integer-unit:8>>;
increment_seed(Bin) when is_binary(Bin) ->
    crypto:strong_rand_bytes(?SEED_BYTES).

handle_fetch_block(Fun, Arg,
            #state{ parent_hosts = ParentNodes,
                    parent_conn_mod = Mod,
                    rpc_seed = Seed}) ->
    %% Parallel fetch top block from all configured parent chain nodes
    case Fun(Arg, Mod, ParentNodes, Seed) of
        {ok, Block, _} -> {ok, Block};
        {error, not_found} -> {error, not_found};
        {error, no_parent_chain_agreement} = Err ->
            %% TODO: decide what to do: this is likely happening because of
            %% rapid parent chain reorganizations
            lager:warning("Parent nodes are unable to reach consensus", []),
            Err
    end.
