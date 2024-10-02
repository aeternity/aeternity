%%% -*- erlang-indent-level:4; indent-tabs-mode:nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity
%%% @doc
%%% Manage interaction with hyperchain parent chain
%%% @end
%%%-------------------------------------------------------------------

-module(aec_parent_connector).

%% Functionality
%% Holds parent chain connection state and contains various utilities for
%% interacting withvthe PC. Wraps parent implementation specific stuff
%% and delegates the actual calling to aehttpc modules for each specific
%% parent implementation (ae, btc, doge).

-behaviour(gen_server).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([start_link/6, stop/0]).

%% Use in test only
-ifdef(TEST).
-export([start_link/0]).
-export([trigger_fetch/0]).
-endif.


-export([%% async getting of blocks
         request_block_by_hash/1,
         request_block_by_height/1,
         request_top/0,
         %% blocking getting of blocks
         fetch_block_by_hash/1,
         fetch_block_by_height/1,
        %% internal state getters
        get_network_id/0,
        get_parent_conn_mod/0,
        get_sign_module/0
        ]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SEED_BYTES, 16).

%% Loop state
-record(state,
    {
        parent_conn_mod = aehttpc_btc,
        network_id,
        sign_module,
        fetch_interval = 10000, % Interval for parent top change checks
        parent_hosts = [],
        hcpc = #{}, % Mapping from hyperchain staker pubkey to parent chain pubkey
        parent_top = not_yet_fetched :: not_yet_fetched | aec_parent_chain_block:block(),
        rpc_seed = crypto:strong_rand_bytes(?SEED_BYTES) % BTC Api only
    }).
-type state() :: #state{}.


%%%=============================================================================
%%% API
%%%=============================================================================
-ifdef(TEST).
-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link() ->
    FetchInterval = 10000,
    ParentHosts = [#{host     => <<"127.0.0.1">>,
                     port     => 3013,
                     scheme   => "http",
                     user     => "test",
                     password => "Pass"
                    }],
    ParentConnMod = aehttpc_aeternity,
    start_link(ParentConnMod, FetchInterval, ParentHosts, <<"local_testnet">>,
              aec_preset_keys, []).
-endif.

%% Start the parent connector process
%% ParentConnMod :: atom() - module name of the http client module aehttpc_btc | aehttpc_aeternity
%% FetchInterval :: integer() | on_demand - millisecs between parent chain checks or when asked (useful for test)
%% ParentHosts :: [#{host => Host, port => Port, scheme => Scheme, user => User, password => Pass}]
%% NetworkID :: binary() - the parent chain's network id
%% SignModule :: atom() - module name of the module that keeps the keys for the parent chain transactions to be signed
%% HCPCPairs :: [{binary(), binary()}] - mapping from hyperchain address to child chain address
%% Recipient :: binary() - the parent chain address to which the commitments must be sent to
-spec start_link(atom(), integer() | on_demand, [map()], binary(), atom(), [{binary(), binary()}]) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(ParentConnMod, FetchInterval, ParentHosts, NetworkId, SignModule, HCPCPairs) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ParentConnMod, FetchInterval, ParentHosts, NetworkId, SignModule, HCPCPairs], []).

stop() ->
    gen_server:stop(?MODULE).

-ifdef(TEST).
trigger_fetch() ->
    gen_server:call(?SERVER, trigger_fetch).
-endif.

request_block_by_hash(Hash) ->
    gen_server:cast(?SERVER, {request_block_by_hash, Hash}).

request_block_by_height(Height) ->
    gen_server:cast(?SERVER, {request_block_by_height, Height}).

request_top() ->
    ?SERVER ! check_parent.

%% this blocks the caller process, use with caution
-spec fetch_block_by_hash(binary()) -> {ok, aec_parent_chain_block:block()}
                                     | {error, not_found | no_parent_chain_agreement}.
fetch_block_by_hash(Hash) ->
    gen_server:call(?SERVER, {fetch_block_by_hash, Hash}).

%% this blocks the caller process, use with caution
-spec fetch_block_by_height(non_neg_integer()) -> {ok, aec_parent_chain_block:block()}
                                                | {error, not_found | no_parent_chain_agreement}.
fetch_block_by_height(Height) ->
    gen_server:call(?SERVER, {fetch_block_by_height, Height}).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init([any()]) -> {ok, #state{}}.
init([ParentConnMod, FetchInterval, ParentHosts, NetworkId, SignModule, HCPCPairs]) ->
    {ok, #state{parent_conn_mod = ParentConnMod,
                fetch_interval = FetchInterval,
                parent_hosts = ParentHosts,
                network_id = NetworkId,
                sign_module = SignModule,
                hcpc = maps:from_list(HCPCPairs)}}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call(trigger_fetch, _From, State) ->
    self() ! check_parent,
    Reply = ok,
    {reply, Reply, State};
handle_call({fetch_block_by_hash, Hash}, _From, State) ->
    Reply = handle_fetch_block(fun fetch_block_by_hash/5, Hash, State),
    {reply, Reply, State};
handle_call({fetch_block_by_height, Height}, _From, State) ->
    Reply = handle_fetch_block(fun fetch_block_by_height/5, Height, State),
    {reply, Reply, State};
handle_call(get_network_id, _From, #state{network_id = NetworkId} = State) ->
    {reply, NetworkId, State};
handle_call(get_sign_module, _From, #state{sign_module = SM} = State) ->
    {reply, SM, State};
handle_call(get_parent_conn_mod, _From, #state{parent_conn_mod = ParentConnMod} = State) ->
    {reply, ParentConnMod, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({request_block_by_hash, Hash}, State) ->
    case handle_fetch_block(fun fetch_block_by_hash/5, Hash, State) of
        {ok, Block} -> aec_parent_chain_cache:post_block(Block);
        {error, not_found} -> pass;
        {error, no_parent_chain_agreement} -> pass
    end,
    {noreply, State};
handle_cast({request_block_by_height, Height}, State) ->
    case handle_fetch_block(fun fetch_block_by_height/5, Height, State) of
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
        case fetch_parent_tops(Mod, ParentNodes, Seed, State) of
            {ok, ParentTop, _} ->
                %% No change, just check again later
                ParentTop;
            {ok, NewParentTop, _Node} ->
                aec_parent_chain_cache:post_block(NewParentTop),
                NewParentTop;
            {error, not_found} ->
                lager:warning("Parent nodes did not respond?", []),
                ParentTop;
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
fetch_parent_tops(Mod, ParentNodes, Seed, State) ->
    FetchFun =
        fun(NodeSpec) ->
            Mod:get_latest_block(NodeSpec, Seed)
        end,
    Fun = fun(Parent) -> fetch_block(FetchFun, Parent, State) end,
    {Good, Errors} = aeu_lib:pmap(Fun, ParentNodes, 10000),
    responses_consensus(Good, Errors, length(ParentNodes)).

fetch_block_by_hash(Hash, Mod, ParentNodes, Seed, State) ->
    FetchFun =
        fun(NodeSpec) ->
            Mod:get_header_by_hash(Hash, NodeSpec, Seed)
        end,
    Fun = fun(Parent) -> fetch_block(FetchFun, Parent, State) end,
    {Good, Errors} = aeu_lib:pmap(Fun, ParentNodes, 10000),
    responses_consensus(Good, Errors, length(ParentNodes)).

fetch_block_by_height(Height, Mod, ParentNodes, Seed, State) ->
    FetchFun =
        fun(NodeSpec) ->
            Mod:get_header_by_height(Height, NodeSpec, Seed)
        end,
    Fun = fun(Parent) -> fetch_block(FetchFun, Parent, State) end,
    {Good, Errors} = aeu_lib:pmap(Fun, ParentNodes, 10000),
    responses_consensus(Good, Errors, length(ParentNodes)).

fetch_block(FetchFun, NodeSpec,
            #state{parent_conn_mod = _Mod,
                   rpc_seed = _Seed}) ->
    case FetchFun(NodeSpec) of
        {ok, BlockHash, PrevHash, Height, Time} ->
            Block = aec_parent_chain_block:new(BlockHash, Height, PrevHash, Time),
            {ok, {Block, NodeSpec}};
        Err ->
            Err
    end.

%% Check:
%% * multiple successful replies
%% * majority agree on a block
responses_consensus(Good0, _Errors, TotalCount) ->
    MinRequired = TotalCount div 2,
    Good = [{Res, Node} || {ok, {Res, Node}} <- Good0],
    Counts = lists:foldl(fun({Res, _Node}, Acc) ->
                            Fun = fun(V) -> V + 1 end,
                            maps:update_with(Res, Fun, 1, Acc)
                        end, #{}, Good),
    NotFoundsCnt = length([1 || {error, not_found} <- Good0]),
    case maps:size(Counts) =:= 0 of
        true when NotFoundsCnt > MinRequired ->
            {error, not_found};
        true ->
            {error, no_parent_chain_agreement};
        false ->
            {MostReturnedResult, Qty} =
                maps:fold(
                    fun(K, V, {_,Max}) when V > Max -> {K,V};
                       (_K, _V, Acc) -> Acc
                    end,
                    {undefined, 0},
                    Counts),
            %% Need Qty to be > half of the total number of configured nodes ??
            if Qty > MinRequired ->
                {_, Node} = lists:keyfind(MostReturnedResult, 1, Good),
                {ok, MostReturnedResult, Node};
            true ->
                case NotFoundsCnt > MinRequired of
                    true -> {error, not_found};
                    false -> {error, no_parent_chain_agreement}
                end
        end
    end.

increment_seed(<<Num:?SEED_BYTES/unsigned-integer-unit:8>>) ->
    <<(Num + 1):?SEED_BYTES/unsigned-integer-unit:8>>;
increment_seed(Bin) when is_binary(Bin) ->
    crypto:strong_rand_bytes(?SEED_BYTES).

handle_fetch_block(Fun, Arg,
            #state{ parent_hosts = ParentNodes,
                    parent_conn_mod = Mod,
                    rpc_seed = Seed} = State) ->
    %% Parallel fetch top block from all configured parent chain nodes
    case Fun(Arg, Mod, ParentNodes, Seed, State) of
        {ok, Block, _} -> {ok, Block};
        {error, not_found} -> {error, not_found};
        {error, no_parent_chain_agreement} = Err ->
            %% TODO: decide what to do: this is likely happening because of
            %% rapid parent chain reorganizations
            lager:warning("Parent nodes are unable to reach consensus", []),
            Err
    end.

get_network_id() ->
    gen_server:call(?SERVER, get_network_id).

get_parent_conn_mod() ->
    gen_server:call(?SERVER, get_parent_conn_mod).

get_sign_module() ->
    gen_server:call(?SERVER, get_sign_module).
