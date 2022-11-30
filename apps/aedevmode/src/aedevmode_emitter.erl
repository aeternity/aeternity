%% -*- erlang-indent-mode: 4; indent-tabs-mode: nil -*-
-module(aedevmode_emitter).
-behavior(gen_server).

-export([start_link/0]).

-export([
          emit_keyblocks/1
        , emit_microblock/0
        , mine_until_txs_on_chain/2
        ]).

-export([
          set_keyblock_interval/1
        , set_microblock_interval/1
        , auto_emit_microblocks/1
        , get_keyblock_interval/0
        , get_microblock_interval/0
        , get_auto_emit_microblocks/0
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-record(st, { keyblock_interval = 0
            , kb_tref
            , microblock_interval = 0
            , mb_tref
            , auto_emit = false }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

emit_keyblocks(N) when is_integer(N), N > 0 ->
    gen_server:call(?MODULE, {emit_keyblocks, N}).

set_keyblock_interval(I) when is_integer(I), I >= 0 ->
    gen_server:call(?MODULE, {set_keyblock_interval, I}).

set_microblock_interval(I) when is_integer(I), I >= 0 ->
    gen_server:call(?MODULE, {set_microblock_interval, I}).

auto_emit_microblocks(Bool) when is_boolean(Bool) ->
    gen_server:call(?MODULE, {auto_emit_microblocks, Bool}).

get_keyblock_interval() ->
    gen_server:call(?MODULE, get_keyblock_interval).

get_microblock_interval() ->
    gen_server:call(?MODULE, get_microblock_interval).

get_auto_emit_microblocks() ->
    gen_server:call(?MODULE, get_auto_emit_microblocks).

emit_microblock() ->
    gen_server:call(?MODULE, emit_microblock).

mine_until_txs_on_chain(TxHashes, Max) when is_list(TxHashes),
                                            is_integer(Max),
                                            Max > 0 ->
    gen_server:call(?MODULE, {mine_until_txs_on_chain, TxHashes, Max}).

init([]) ->
    aec_events:subscribe(tx_created),
    aec_events:subscribe(tx_received),
    aec_events:subscribe(top_changed),
    case aec_chain:top_height() of
        0 ->
            aec_conductor:consensus_request(emit_kb);
        _ ->
            ok
    end,
    {ok, #st{ keyblock_interval   = cfg(<<"keyblock_interval">>)
            , microblock_interval = cfg(<<"microblock_interval">>)
            , auto_emit           = cfg(<<"auto_emit_microblocks">>) }}.

cfg(K) ->
    {ok, V} = aeu_env:find_config([<<"dev_mode">>, K], [user_config, schema_default]),
    V.

handle_call({set_keyblock_interval, I}, _From, St) ->
    case I of
        _ when is_integer(I), I >= 0 ->
            St1 = restart_keyblock_timer(St#st{keyblock_interval = I}),
            {reply, ok, St1};
        _ ->
            {reply, {error, invalid}, St}
    end;
handle_call({set_microblock_interval, I}, _From, St) ->
    case I of
        _ when is_integer(I), I >= 0 ->
            St1 = restart_microblock_timer(St#st{microblock_interval = I}),
            {reply, ok, St1};
        _ ->
            {reply, {error, invalid}, St}
    end;
handle_call({auto_emit_microblocks, Bool}, _From, St) ->
    case Bool of
        _ when is_boolean(Bool) ->
            St1 = check_if_existing_txs(Bool, St),
            {reply, ok, St1#st{auto_emit = Bool}};
        _ ->
            {reply, {error, invalid}, St}
    end;
handle_call(get_keyblock_interval, _From, #st{keyblock_interval = I} = St) ->
    {reply, I, St};
handle_call(get_microblock_interval, _From, #st{microblock_interval = I} = St) ->
    {reply, I, St};
handle_call(get_auto_emit_microblocks, _From, #st{auto_emit = Bool} = St) ->
    {reply, Bool, St};
handle_call({emit_keyblocks, N}, _From, St) ->
    St1 = emit_keyblocks_(N, St),
    {reply, ok, St1};
handle_call(emit_microblock, _From, St) ->
    {reply, ok, emit_microblock_(St)};
handle_call({mine_until_txs_on_chain, TxHashes, Max}, _From, St) ->
    {Reply, St1} = mine_until_txs_on_chain_(TxHashes, Max, St),
    {reply, Reply, St1};
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({gproc_ps_event, Event, #{info := STx}}, St)
  when Event == tx_created; Event == tx_received ->
    Hashes = [aetx_sign:hash(STx) | flush_tx_events()],
    lager:info("Tx pool events hashes: ~p", [[encode_hash(H) || H <- Hashes]]),
    case St#st.auto_emit of
        true ->
            {_, St1} = mine_until_txs_on_chain_(Hashes, 10, St),
            {noreply, St1};
        false ->
            {noreply, St}
    end;
handle_info({gproc_ps_event, top_changed, #{info := Info}}, St) ->
    Type = maps:get(block_type, Info),
    St1 = restart_block_timer(Type, St),
    case Info of
        #{block_type := key, height := Height} ->
            lager:info("New key block - height: ~p", [Height]);
        #{block_type := micro} ->
            info_micro_block(Info)
    end,
    {noreply, St1};
handle_info({timeout, _, emit_kb_ping}, St) ->
    {noreply, emit_keyblock(St)};
handle_info({timeout, _, emit_mb_ping}, St) ->
    {noreply, emit_microblock_(St)};
handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

info_micro_block(#{block_type := micro, block_hash := BHash, height := Height}) ->
    case aec_chain:get_block(BHash) of
        {ok, Block} ->
            Txs = aec_blocks:txs(Block),
            lager:info("New micro block - height: ~p, ~p txs",
                       [Height, length(Txs)]);
        {error, Reason} ->
            %% Shouldn't happen
            lager:info("New micro block - height: ~p, CANNOT READ (~p)",
                       [Height, Reason])
    end.

flush_tx_events() ->
    receive
        {gproc_ps_event, Event, #{info := STx}} when Event == tx_created;
                                                     Event == tx_received ->
            [aetx_sign:hash(STx) | flush_tx_events()]
    after 0 ->
            []
    end.

encode_hash(Hash) ->
    aeser_api_encoder:encode(tx_hash, Hash).

emit_keyblock(St) ->
    aec_conductor:consensus_request(emit_kb),
    St.

emit_keyblocks_(N, St) ->
    aec_conductor:consensus_request({mine_blocks, N, key}),
    St.

emit_microblock_(St) ->
    aec_conductor:consensus_request(emit_mb),
    St.

check_if_existing_txs(true, #st{auto_emit = false} = St) ->
    case aec_tx_pool:peek(100) of
        {ok, [_|_] = Txs} ->
            lager:info("~p txs already in mempool", [length(Txs)]),
            Hashes = [aetx_sign:hash(STx) || STx <- Txs],
            {_, St1} = mine_until_txs_on_chain_(Hashes, 50, St),
            St1;
        _ ->
            St
    end;
check_if_existing_txs(_, St) ->
    St.

mine_until_txs_on_chain_(TxHashes, Max, St) ->
    Reply = aec_conductor:consensus_request(
              {mine_blocks_until_txs_on_chain, TxHashes, Max}),
    {Reply, St}.

restart_block_timer(key, St) ->
    restart_keyblock_timer(St);
restart_block_timer(micro, St) ->
    restart_microblock_timer(St).

restart_keyblock_timer(#st{keyblock_interval = 0} = St) ->
    St;
restart_keyblock_timer(#st{keyblock_interval = I, kb_tref = TRef0} = St) ->
    cancel_timer(TRef0),
    TRef = erlang:start_timer(I * 1000, self(), emit_kb_ping),
    St#st{kb_tref = TRef}.

restart_microblock_timer(#st{microblock_interval = 0} = St) ->
    St;
restart_microblock_timer(#st{microblock_interval = I, mb_tref = TRef0} = St) ->
    cancel_timer(TRef0),
    TRef = erlang:start_timer(I * 1000, self(), emit_mb_ping),
    St#st{mb_tref = TRef}.

cancel_timer(undefined) ->
    ok;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).
