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
        get_sign_module/0,
        get_parent_chain_type/0,
        %% Pinning
        pin_to_pc/3,
        pin_tx_to_cc/4,
        pin_contract_call/5,
        get_pinning_data/0,
        create_pin_tx/5,
        post_pin_tx/1,
        get_pin_by_tx_hash/1,
        encode_parent_pin_payload/1,
        decode_parent_pin_payload/1,
        encode_child_pin_payload/1,
        decode_child_pin_payload/1,
        is_pin/1,
        find_spends_to/1,
        has_parent_account/1
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

get_network_id() ->
    gen_server:call(?SERVER, get_network_id).

get_parent_conn_mod() ->
    gen_server:call(?SERVER, get_parent_conn_mod).

get_sign_module() ->
    gen_server:call(?SERVER, get_sign_module).

-spec get_parent_chain_type() -> {ok, atom()}.
get_parent_chain_type() ->
    gen_server:call(?SERVER, get_parent_chain_type).

-spec get_pinning_data() -> {ok, map()} | {error, atom()}.
get_pinning_data() ->
    gen_server:call(?SERVER, get_pinning_data).

pin_to_pc(Who, Amount, Fee) ->
    gen_server:call(?SERVER, {pin_to_pc, Who, Amount, Fee}).

pin_tx_to_cc(PinTx, Who, Amount, Fee) ->
    gen_server:call(?SERVER, {pin_to_pc, PinTx, Who, Amount, Fee}).

pin_contract_call(Contract, PinTx, Who, Amount, Fee) ->
    gen_server:call(?SERVER, {pin_contract_call, Contract, PinTx, Who, Amount, Fee}).


-spec create_pin_tx(binary(), binary(), integer(), integer(), binary()) -> aetx:tx().
create_pin_tx(SenderEnc, ReceiverPubkey, Amount, Fee, PinningData) ->
    gen_server:call(?SERVER, {create_pin_tx, SenderEnc, ReceiverPubkey, Amount, Fee, PinningData}).

post_pin_tx(Tx) ->
    gen_server:call(?SERVER, {post_pin_tx, Tx}).

get_pin_by_tx_hash(TxHash) ->
    gen_server:call(?SERVER, {get_pin_by_tx_hash, TxHash}).

encode_parent_pin_payload(Pin) ->
    gen_server:call(?SERVER, {encode_parent_pin_payload, Pin}).

decode_parent_pin_payload(PinPayload) ->
    gen_server:call(?SERVER, {decode_parent_pin_payload, PinPayload}).

encode_child_pin_payload(TxHash) ->
    gen_server:call(?SERVER, {encode_child_pin_payload, TxHash}).

decode_child_pin_payload(TxHash) ->
    gen_server:call(?SERVER, {decode_child_pin_payload, TxHash}).

has_parent_account(Account) ->
    gen_server:call(?SERVER, {has_parent_account, Account}).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init([any()]) -> {ok, #state{}}.
init([ParentConnMod, FetchInterval, ParentHosts, NetworkId, SignModule, HCPCMap]) ->
    {ok, #state{parent_conn_mod = ParentConnMod,
                fetch_interval = FetchInterval,
                parent_hosts = ParentHosts,
                network_id = NetworkId,
                sign_module = SignModule,
                hcpc = HCPCMap}}.

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
handle_call(get_parent_conn_mod, _From, #state{parent_conn_mod = Mod} = State) ->
    {reply, Mod, State};
handle_call(get_parent_chain_type, _From, #state{parent_conn_mod = Mod} = State) ->
    Reply = Mod:get_chain_type(),
    {reply, Reply, State};
handle_call(get_pinning_data, _From, #state{parent_conn_mod = Mod, network_id = NetworkID} = State) ->
    Reply = get_pinning_data_(Mod, NetworkID),
    {reply, Reply, State};
handle_call({pin_to_pc, Who, Amount, Fee}, _From, #state{parent_conn_mod = Mod, parent_hosts = ParentHosts, hcpc = HCPCMap, sign_module = SignModule, network_id = NetworkID} = State) ->
    Reply = case maps:is_key(Who, HCPCMap) of
        true ->
            {ok, PinningData} = get_pinning_data_(Mod, NetworkID),
            handle_parent_pin_calls(Mod, pin_to_pc, {PinningData, maps:get(Who, HCPCMap), Amount, Fee, NetworkID, SignModule}, ParentHosts);
        false -> {error, {no_pc_account_for_staker, Who}}
    end,
    {reply, Reply, State};
handle_call({pin_tx_to_cc, PinTx, Who, Amount, Fee}, _From, #state{parent_conn_mod = Mod, sign_module = SignModule} = State) ->
    Reply = case SignModule:is_key_present(Who) of
        true ->
            Mod:pin_tx_to_cc(PinTx, Who, Amount, Fee, SignModule);
        false -> {error, {no_cc_account_for_staker, Who}} % Could this actually happen????
    end,
    {reply, Reply, State};
handle_call({pin_contract_call, Contract, PinTx, Who, Amount, Fee}, _From, #state{parent_conn_mod = Mod, sign_module = SignModule} = State) ->
    Reply =  Mod:pin_contract_call(Contract, PinTx, Who, Amount, Fee, SignModule),
    {reply, Reply, State};
handle_call({create_pin_tx, SenderEnc, ReceiverPubkey, Amount, Fee, PinningData},
             _From,
             #state{parent_conn_mod = Mod, parent_hosts = ParentHosts} = State) ->
    Reply = handle_parent_pin_calls(Mod, create_pin_tx, {SenderEnc, ReceiverPubkey, Amount, Fee, PinningData}, ParentHosts),
    {reply, Reply, State};
handle_call({post_pin_tx, Tx}, _From, #state{parent_conn_mod = Mod, parent_hosts = ParentHosts} = State) ->
    Reply = handle_parent_pin_calls(Mod, post_pin_tx, Tx, ParentHosts),
    {reply, Reply, State};
handle_call({get_pin_by_tx_hash, Tx}, _From, #state{parent_conn_mod = Mod, parent_hosts = ParentHosts} = State) ->
    Reply = handle_parent_pin_calls(Mod, get_pin_by_tx_hash, Tx, ParentHosts),
    {reply, Reply, State};
handle_call({encode_parent_pin_payload, Pin}, _From, #state{parent_conn_mod = Mod} = State) ->
    Reply = handle_conn_mod_calls(Mod, encode_parent_pin_payload, Pin),
    {reply, Reply, State};
handle_call({decode_parent_pin_payload, PinPayload}, _From, #state{parent_conn_mod = Mod} = State) ->
    Reply = handle_conn_mod_calls(Mod, decode_parent_pin_payload, PinPayload),
    {reply, Reply, State};
handle_call({encode_child_pin_payload, TxHash}, _From, #state{parent_conn_mod = Mod} = State) ->
    Reply = handle_conn_mod_calls(Mod, encode_child_pin_payload, TxHash),
    {reply, Reply, State};
handle_call({decode_child_pin_payload, EncTxHash}, _From, #state{parent_conn_mod = Mod} = State) ->
    Reply = handle_conn_mod_calls(Mod, decode_child_pin_payload, EncTxHash),
    {reply, Reply, State};
handle_call({is_pin, EncTxHash}, _From, #state{parent_conn_mod = Mod} = State) ->
    Reply = handle_conn_mod_calls(Mod, is_pin, EncTxHash),
    {reply, Reply, State};
handle_call({has_parent_account, Account}, _From, #state{hcpc = HCPCMap} = State) ->
    Reply = maps:is_key(Account, HCPCMap),
    {reply, Reply, State};
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




is_pin(Pin) ->
    gen_server:call(?SERVER, {is_pin, Pin}).

% PINREFAC is this (aec_p_c) the correct place for this and the following CC pin related ones?
get_pinning_data_(Mod, NetworkID) ->
    {ok, #{epoch := Epoch,
           first := First,
           last  := Last}} = aec_chain_hc:epoch_info(),
    lager:debug("Get pin data for epoch ~p for leader of block ~p", [Epoch - 1, Last]),
    {ok, BlockHash} = aec_chain_state:get_key_block_hash_at_height(First - 1),
    {ok, ChainType} = Mod:get_chain_type(),
    PrevEpoch = Epoch - 1,
    Height = First - 1,
    case aec_consensus_hc:leader_for_height(Last) of
      {ok, Leader} ->
        {ok, #{epoch             => PrevEpoch,
               height            => Height,
               block_hash        => BlockHash,
               parent_payload    => Mod:encode_parent_pin_payload(#{epoch => PrevEpoch, height => Height, block_hash => BlockHash}),
               last_leader       => Leader,
               parent_type       => ChainType,
               parent_network_id => NetworkID}};
      error ->
          %% schedule not yet cached
          {error, last_leader_unknown}
    end.

find_spends_to(Account) ->
    {ok, #{last := Last, first := First}} = aec_chain_hc:epoch_info(),
    Blocks = aec_chain_hc:get_micro_blocks_between(First, Last-1),
    lists:flatten([ pick_pin_spends_to(Account, aec_blocks:txs(B)) || B <- Blocks ]).


pick_pin_spends_to(Account, Txs) ->
    BareTxs = [ aetx_sign:tx(T) || T <- Txs],
    InnerTxs = [ aetx:specialize_type(Tx) || Tx <- BareTxs,  spend_tx == aetx:tx_type(Tx)],
    [ aec_spend_tx:payload(T) || {_, T} <- InnerTxs, aeser_id:specialize(aec_spend_tx:recipient_id(T)) == {account,Account}, is_pin(aec_spend_tx:payload(T))].

%% handle (pin) calls to the parent connector module and ensure we don't throw anything
%% FUTURE Should be reasonably easy to loop over NodeSpecs until one call suceeds.
%%        For now, we just pick the first one.

handle_parent_pin_calls(Mod, Fun, Args, NodeSpecs) ->
    [NodeSpec|_] = NodeSpecs,
    try
        Mod:Fun(Args, NodeSpec)
    catch
        Type:Err ->
            lager:debug("PINNING: caught bad pin parent call: ~p : ~p", [Type, Err]),
            {error, Err}
    end.

handle_conn_mod_calls(Mod, Fun, Args) ->
    try
        Mod:Fun(Args)
    catch
        Type:Err ->
            lager:debug("PINNING: caught bad connector call: ~p :  ~p", [Type, Err]),
            {error, Err}
    end.

