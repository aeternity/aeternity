%%%=============================================================================
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%%    Test utils for State Channel Chain Watcher
%%% @end
%%%=============================================================================

-module(aesc_chain_watcher_SUITE).

-export([
          all/0
        , groups/0
        , suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_group/2
        , end_per_group/2
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([
          reg_unreg/1
        , set_up_channel/1
        , deposit/1
        , flush_cache/1
        , cleanup/1
        ]).

%% Watcher callbacks
-export([
          minimum_depth_achieved/4
        , channel_changed_on_chain/2
        , channel_closing_on_chain/2
        , channel_closed_on_chain/2
        , channel_unlocked/2
        ]).

%% Test helpers (called using `apply/3`)
-export([
          reg_watch/1
        , reg_close/2
        , close_watch/2
        , min_depth_watch/4
        ]).


-include_lib("common_test/include/ct.hrl").
-include_lib("aeutils/include/aeu_stacktrace.hrl").

-define(LOG(_Fmt, _Args), log(_Fmt, _Args, ?LINE, true)).
-define(LOG(_D, _Fmt, _Args), log(_Fmt, _Args, ?LINE, _D)).

-define(TIMEOUT, 3000).
-define(TYPE, {type, ?LINE}).   % unique type info for min-depth callback

all() ->
    [{group, all_tests}].

groups() ->
    [
     %% A separate watcher started for each sub-group
      {all_tests, [sequence], [ {group, admin}
                              , {group, channel_lifecycle}
                              , {group, multiple_channels}
                              , {group, cache_flushing} ]}
    , {admin, [sequence], [ reg_unreg ]}
    , {channel_lifecycle, [sequence], [ set_up_channel
                                      , deposit
                                      , cleanup ]}
    , {multiple_channels, [sequence], [ set_up_channel
                                      , set_up_channel
                                      , set_up_channel
                                      , deposit
                                      , cleanup ]}
    , {cache_flushing, [sequence], [ set_up_channel
                                   , deposit
                                   , flush_cache
                                   , deposit
                                   , cleanup ]}
    ].

suite() ->
    [].

%% ======================================================================
%% Test cases

reg_unreg(_Config) ->
    Client = spawn_link(fun() ->
                                ok = reg_watch(chid(1)),
                                receive die -> ok end
                        end),
    timer:sleep(50),
    kill_client(Client),
    ok.

cleanup(_Config) ->
    AllClients = get_all_clients(),
    ?LOG("AllClients = ~p", [AllClients]),
    [kill_client(C) || C <- AllClients].

kill_client(Client) ->
    true = pid_exists(Client),
    unlink(Client),
    MRef = erlang:monitor(process, Client),
    exit(Client, kill),
    receive
        {'DOWN', MRef, _, _, _} ->
            _ = sys:get_status(aesc_chain_watcher),
            _ = sys:get_status(aesc_chain_watcher),
            case pid_exists(Client) of
                false ->
                    ok;
                true ->
                    ?LOG("Pid ~p still exists: ~p",
                         [Client, [{T, ets:tab2list(T)} ||
                                      T <- aesc_chain_watcher:table_names()]]),
                    error({still_exists, Client})
            end
    end.

set_up_channel(Config) ->
    #{ channel_id := ChId
     , tx_hash    := TxHash
     , signed_tx  := SignedTx } = CreateTx = create_tx(Config),
    ok = push(CreateTx),
    mempool = chain_req({find_tx_with_location, TxHash}),
    ClientIxs = [1,2,3],
    [C1,C2,C3] = Cs = [spawn_client(N, ChId) || N <- ClientIxs],
    Watchers = [C1, C2],
    [ok,ok] = [set_reg_watch(C, ChId) || C <- Watchers],
    ok = set_reg_close(C3, ChId, 3),
    %%
    add_microblock(),
    ok = watchers_notified(
           fun({channel_changed_on_chain, I}, _C) ->
                   #{tx := SignedTx} = I,
                   ok
           end, Watchers),
    %%
    %% Set a min-depth watch. Verify that the event comes at the right height,
    %% and only to those that requested it
    %%
    ok = set_min_depth_watch(C1, ChId, TxHash, 3, MType1 = {type,?LINE}),
    verify_min_depth([{C1, MType1}], ChId, TxHash, 3, Cs),
    chain_req({channel_setup, ChId, #{ client_ixs     => ClientIxs
                                     , clients        => Cs
                                     , watchers       => Watchers
                                     , close_watchers => [C3] }}),
    ok.

deposit(Config) ->
    ChSetup = get_channel_setup(),
    maps:fold(
      fun(ChId, Setup, ok) ->
              deposit_(ChId, Setup, Config)
      end, ok, ChSetup),
    ok.

deposit_(ChId, Setup, Config) ->
    Cs = maps:get(clients, Setup),
    Watchers = maps:get(watchers, Setup),
    #{ tx_hash   := TxHash
     , signed_tx := SignedTx } = DepositTx = deposit_tx(ChId, Config),
    push(DepositTx),
    C1 = hd(Cs),
    ok = set_min_depth_watch(C1, ChId, TxHash, MinDepth = 3, MType1 = ?TYPE),
    assert_no_events(Cs),
    add_microblock(),
    ok = watchers_notified(
           fun({channel_changed_on_chain, I}, _C) ->
                   #{tx := SignedTx} = I,
                   ok
           end, Watchers),
    verify_min_depth([{C1,MType1}], ChId, TxHash, MinDepth, Cs -- [C1]),
    ok.

flush_cache(_Config) ->
    I = aesc_chain_watcher:get_cache_reinit_interval(),
    [ add_keyblock() || _ <- lists:seq(1, I+1) ],
    AllClients = get_all_clients(),
    assert_no_events(AllClients),
    ok.

verify_min_depth(MDWs, ChId, TxHash, MinDepth, Cs) ->
    MDWPids = [ client_pid(W) || W <- MDWs],
    OtherCs = Cs -- MDWPids,
    [ assert_no_events(fun add_keyblock/0, Cs)
      || _ <- lists:seq(1, MinDepth-1) ],
    add_keyblock(),
    ok = watchers_notified(
           fun({minimum_depth_achieved, Info}, {_, T}) ->
                   {{ChId, T, TxHash}, Info} = {Info, Info},
                   ok
           end, MDWs),
    assert_no_events(OtherCs),
    ok.

%% ======================================================================
%% Watcher callbacks

channel_changed_on_chain(Client, Info) ->
    event(Client, {channel_changed_on_chain, Info}).

channel_closed_on_chain(Client, Info) ->
    event(Client, {channel_closed_on_chain, Info}).

channel_closing_on_chain(Client, Info) ->
    event(Client, {channel_closing_on_chain, Info}).

channel_unlocked(Client, Info) ->
    event(Client, {channel_unlocked, Info}).

minimum_depth_achieved(Client, ChId, Req, Info) ->
    event(Client, {minimum_depth_achieved, {ChId, Req, Info}}).

event(Client, E) ->
    ?LOG("From watcher (Client: ~p): ~p", [Client, E]),
    Client ! {from_watcher, E},
    ok.

await_event(Timeout) ->
    receive
        {from_watcher, E} ->
            {ok, E}
    after Timeout ->
            timeout
    end.

%% ======================================================================
%% Helper functions

assert_no_events(F, Cs) when is_function(F, 0) ->
    F(),
    assert_no_events(Cs).

assert_no_events(Cs) ->
    Timeouts = [client_req(C, {await_event, 0}) || C <- Cs],
    case all_are_timeouts(Timeouts) of
        true ->
            true;
        false ->
            error({expected_all_timeouts, Timeouts})
    end.

all_are_timeouts(L) ->
    lists:all(fun(X) -> X == timeout end, L).

watchers_notified(Check, Cs) ->
    lists:foreach(
      fun(C) ->
              CPid = client_pid(C),
              case client_req(CPid, {await_event, ?TIMEOUT}) of
                  {ok, Evt} ->
                      Error = fun(E) ->
                                      error({notification_error, [ {client, C}
                                                                 , {event, Evt}
                                                                 , {error, E} ]})
                              end,
                      try Check(Evt, C) of
                          ok -> ok;
                          {error,_} = E ->
                              Error(E)
                      catch
                          error:E ->
                              Error(E)
                      end;
                  timeout ->
                      error({notification_error, [ {client, C}
                                                 , {error, timeout} ]})
              end
      end, Cs),
    ?LOG("Watchers notified: ~p", [Cs]),
    ok.

client_pid(C) when is_pid(C) ->
    C;
client_pid({C,_}) when is_pid(C) ->
    C.

get_all_clients() ->
    get_all_clients(get_channel_setup()).

get_all_clients(ChSetup) when is_map(ChSetup) ->
    maps:fold(
      fun(_, Setup, Acc) ->
              Acc ++ maps:get(clients, Setup, [])
      end, [], ChSetup).

get_channel_setup() ->
    chain_req(channel_setup).

set_reg_watch(C, ChId) ->
    client_apply(C, ?MODULE, reg_watch, [ChId]).

set_reg_close(C, ChId, MinDepth) ->
    client_apply(C, ?MODULE, reg_close, [ChId, MinDepth]).

set_min_depth_watch(C, ChId, TxHash, MinDepth, Type) ->
    client_apply(C, ?MODULE, min_depth_watch,
                 [ChId, TxHash, MinDepth, Type]).

reg_watch(ChId) ->
    aesc_chain_watcher:register(
      ChId, ?MODULE, [aesc_chain_watcher:watch_req()]).

reg_close(ChId, MinDepth) ->
    aesc_chain_watcher:register(
      ChId, ?MODULE, [aesc_chain_watcher:close_req(MinDepth)]).

close_watch(ChId, MinDepth) ->
    aesc_chain_watcher:request(
      ChId, aesc_chain_watcher:close_req(MinDepth)).

min_depth_watch(ChId, TxHash, Depth, ReqType) ->
    aesc_chain_watcher:request(
      ChId, aesc_chain_watcher:min_depth_req(TxHash, Depth, ReqType)).

pid_exists(Pid) ->
    Tabs = aesc_chain_watcher:table_names(),
    try lists:foldl(
          fun(Tab, Acc) ->
                  ets:foldl(
                    fun(Obj, Acc1) ->
                            case pid_in_obj(Obj, Pid) of
                                true ->
                                    throw(true);
                                false ->
                                    Acc1
                            end
                    end, Acc, Tab)
          end, false, Tabs)
    catch
        throw:true ->
            true
    end.

pid_in_obj(Pid, Pid) ->
    true;
pid_in_obj([_|_] = L, Pid) ->
    lists:any(fun(X) -> pid_in_obj(X, Pid) end, L);
pid_in_obj(Tuple, Pid) when is_tuple(Tuple) ->
    pid_in_obj(tuple_to_list(Tuple), Pid);
pid_in_obj(Map, Pid) when is_map(Map) ->
    pid_in_obj(maps:to_list(Map), Pid);
pid_in_obj(_, _) ->
    false.

chid(1) ->
    <<"chid-1..........................">>;
chid(2) ->
    <<"chid-2..........................">>;
chid(3) ->
    <<"chid-3..........................">>;
chid(4) ->
    <<"chid-4..........................">>.

spawn_client(N, ChId) ->
    spawn(fun() ->
                  gproc:reg(client_regname(N, ChId)),
                  client_loop()
          end).

client_regname(N, ChId) ->
    {n, l, {?MODULE, client, N, ChId}}.

client_apply(Pid, M, F, A) when is_pid(Pid) ->
    client_req(Pid, {apply, M, F, A}).

client_req(Pid, Req) when is_pid(Pid) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {self(), Req},
    receive
        {Pid, Reply} ->
            Reply;
        {'DOWN', MRef, _, _, Reason} ->
            ?LOG("Client ~p died: ~p", [Pid, Reason]),
            error({client_died, [Pid, Reason]})
    after 5000 ->
            error(timeout)
    end.

client_loop() ->
    receive
        {From, {await_event, Timeout}} when is_pid(From) ->
            ?LOG("await_event (~p)", [Timeout]),
            Result = await_event(Timeout),
            From ! {self(), Result},
            client_loop();
        {From, Req} when is_pid(From) ->
            Reply = handle_client_req(Req),
            From ! {self(), Reply},
            client_loop()
    end.

handle_client_req(R) ->
    try handle_client_req_(R)
    catch error:Err ->
            ?LOG("CAUGHT error:~p / ~p", [Err, erlang:get_stacktrace()]),
            error(Err)
    end.

handle_client_req_({apply, M, F, A}) ->
    apply(M, F, A).

log(Fmt, Args, L, #{debug := true}) ->
    log(Fmt, Args, L, true);
log(Fmt, Args, L, true) ->
    ct:log("~p at ~p: " ++ Fmt, [self(), L | Args]);
log(_, _, _, _) ->
    ok.

%% ======================================================================
%% Block generation
%% Chain represented as a LIFO list of #{hash, header, txs} entries
%% wrapped inside a #{blocks, miner => #{privkey,pubkey}} map
new_chain() ->
    Miner = new_keypair(),
    Hdr = genesis_header(),
    {ok, Hash} = aec_headers:hash_header(Hdr),
    #{ miner   => Miner
     , mempool => []
     , nonces  => #{}
     , blocks  => [#{hash => Hash, header => genesis_header(), txs => []}] }.

%% Called from the chain process
add_keyblock_(#{blocks := Blocks, miner := #{pubkey := Miner}} = Chain) ->
    ?LOG("add_keyblock()", []),
    #{hash := PrevHash, header := TopHdr} = hd(Blocks),
    PrevKeyHash = aec_headers:prev_key_hash(TopHdr),
    Height = aec_headers:height(TopHdr),
    NewHdr = aec_headers:new_key_header(
               Height+1, PrevHash, PrevKeyHash, root_hash(),
               Miner, Miner, 0, 0, 0, 0, default, 0),
    {ok, BlockHash} = aec_headers:hash_header(NewHdr),
    announce(Chain#{ blocks => [#{ hash   => BlockHash
                                 , prev   => PrevHash
                                 , header => NewHdr
                                 , txs    => [] } | Blocks] }).

%% Called from the chain process
add_microblock_(#{mempool := Pool, blocks := Blocks} = Chain) ->
    Txs = lists:reverse(Pool),
    ?LOG("add_microblock(Txs = ~p", [Txs]),
    #{hash := PrevHash, header := TopHdr} = hd(Blocks),
    PrevKeyHash = aec_headers:prev_key_hash(TopHdr),
    Height = aec_headers:height(TopHdr),
    NewHdr = aec_headers:new_micro_header(
               Height + 1, PrevHash, PrevKeyHash,
               root_hash(), 0, txs_hash(), pof_hash(), 0),
    {ok, BlockHash} = aec_headers:hash_header(NewHdr),
    Block = maybe_update_trees(#{ hash   => BlockHash
                                , prev   => PrevHash
                                , header => NewHdr
                                , txs    => Txs }, Chain),
    ?LOG("Microblock = ~p", [Block]),
    announce(Chain#{ blocks => [Block | Blocks], mempool => [] }).

%% Announce top_changed and tx events
announce(#{ blocks := [#{ hash   := TopHash
                        , prev   := PrevHash
                        , header := Hdr
                        , txs    := Txs } | _] = Blocks} = Chain) ->
    Height = length(Blocks) + 1,
    Type = aec_headers:type(Hdr),
    Origin = origin(Type),
    Info = #{ block_hash   => TopHash
            , block_type   => Type
            , block_origin => Origin
            , prev_hash    => PrevHash
            , height       => Height },
    send_tx_events(Txs, Info),
    ?LOG("Publishing top_changed, I = ~p", [Info]),
    aec_events:publish(top_changed, Info),
    Chain.

origin(key) ->
    block_created;
origin(micro) ->
    micro_block_created.

create_and_sign_tx(#{mod := Mod} = TxInfo) ->
    try
        {ok, Tx} = Mod:new(maps:merge(#{fee => 1, nonce => 1},
                                      maps:remove(mod, TxInfo))),
        ?LOG("Tx = ~p", [Tx]),
        SignedTx = aetx_sign:new(Tx, []),
        TxHash = aetx_sign:hash(SignedTx),
        {ok, ChId} = aesc_utils:channel_pubkey(SignedTx),
        TxInfo#{ tx_hash    => TxHash
               , signed_tx  => SignedTx
               , channel_id => ChId }
    catch
        error:Err ->
            ?LOG("CAUGHT error:~p / ~p", [Err, erlang:get_stacktrace()]),
            error(Err)
    end.


send_tx_events(Txs, #{ block_hash   := BlockHash
                     , block_origin := Origin }) ->
    lists:foreach(
      fun(#{ signed_tx  := SignedTx
           , tx_hash    := TxHash
           , channel_id := ChId }) ->
              {TxType, _} = aetx:specialize_type(
                              aetx_sign:innermost_tx(SignedTx)),
              case is_channel_tx_type(TxType) of
                  true ->
                      Evt = {channel, ChId},
                      Info = #{ type         => TxType
                              , tx_hash      => TxHash
                              , block_hash   => BlockHash
                              , block_origin => Origin },
                      ?LOG("Publish tx_event ~p, I = ~p", [Evt, Info]),
                      aec_events:publish({tx_event, Evt}, Info);
                  false ->
                      skip
              end
      end, Txs).

is_channel_tx_type(T) when T == channel_create_tx
                         ; T == channel_deposit_tx
                         ; T == channel_withdraw_tx
                         ; T == channel_force_progress_tx
                         ; T == channel_close_mutual_tx
                         ; T == channel_close_solo_tx
                         ; T == channel_slash_tx
                         ; T == channel_settle_tx
                         ; T == channel_snapshot_solo_tx
                         ; T == channel_offchain_tx ->
    true;
is_channel_tx_type(_) ->
    false.

maybe_update_trees(#{txs := []} = Block, _Chain) ->
    Block;
maybe_update_trees(#{txs := Txs} = Block, #{blocks := Blocks}) ->
    Trees = case trees(Blocks) of
                {ok, Ts} ->
                    Ts;
                error ->
                    #{}
            end,
    NewTrees = lists:foldl(
                 fun(Tx, Ts) ->
                         update_trees(Tx, Ts)
                 end, Trees, Txs),
    Block#{ trees => NewTrees }.

trees(Blocks) ->
    case lists:dropwhile(
           fun(Block) ->
                   not maps:is_key(trees, Block)
           end, Blocks) of
        [#{ trees := Trees }|_] ->
            {ok, Trees};
        [] ->
            error
    end.

update_trees(#{ mod := aesc_create_tx
              , channel_id := ChId } = Tx, Trees) ->
    case maps:is_key({channel, ChId}, Trees) of
        true ->
            error({channel_exists, ChId});
        false ->
            Trees#{ {channel, ChId} => new_channel(Tx) }
    end;
update_trees(#{ mod        := aesc_deposit_tx
              , channel_id := ChId
              , amount     := Amount
              , round      := Round
              , state_hash := StateHash }, Trees) ->
    Ch = maps:get({channel, ChId}, Trees),
    Ch1 = aesc_channels:deposit(Ch, Amount, Round, StateHash),
    Trees#{ {channel, ChId} => Ch1 }.

blocks_until_hash(Hash, Blocks) ->
    lists:dropwhile(
      fun(#{hash := H}) ->
              H =/= Hash
      end, Blocks).

top_block_hash(#{blocks := [#{hash := Hash}|_]}) ->
    Hash.

get_block_state(Hash, #{blocks := Blocks}) ->
    trees(blocks_until_hash(Hash, Blocks)).

hash_is_in_main_chain(Hash, TopHash, #{blocks := Blocks}) ->
    case blocks_until_hash(TopHash, Blocks) of
        [] ->
            false;
        Blocks1 ->
            case lists_mapfind(Hash, hash, Blocks1) of
                false ->
                    false;
                B when is_map(B) ->
                    true
            end
    end.

get_header(Hash, #{blocks := Blocks}) ->
    case blocks_until_hash(Hash, Blocks) of
        [#{header := Header}|_] ->
            {ok, Header};
        [] ->
            error
    end.

get_channel(ChId, #{blocks := Blocks}) ->
    case trees(Blocks) of
        {ok, Trees} ->
            get_channel_(ChId, Trees);
        error ->
            undefined
    end.

get_channel_(Id, Trees) ->
    case maps:find({channel,Id}, Trees) of
        {ok, _} = Ok ->
            Ok;
        error ->
            undefined
    end.

find_block_tx_hashes(Hash, #{blocks := Blocks}) ->
    case blocks_until_hash(Hash, Blocks) of
        [#{txs := Txs}|_] ->
            TxHashes = [ H || #{tx_hash := H} <- Txs ],
            {value, TxHashes};
        [] ->
            not_found
    end.

find_signed_tx(Hash, #{blocks := Blocks}) ->
    case find_signed_tx_(Blocks, Hash) of
        {value, STx, _Block} ->
            {value, STx};
        none ->
            none
    end.

find_signed_tx_([#{txs := Txs} = Block|T], Hash) ->
    case lists_mapfind(Hash, tx_hash, Txs) of
        #{signed_tx := STx} ->
            {value, STx, Block};
        false ->
            find_signed_tx_(T, Hash)
    end;
find_signed_tx_([], _) ->
    none.

find_tx_with_location(Hash, #{blocks := Blocks, mempool := Pool}) ->
    case find_signed_tx_(Blocks, Hash) of
        {value, STx, #{hash := BlockHash}} ->
            {BlockHash, STx};
        none ->
            case lists_mapfind(Hash, tx_hash, Pool) of
                false ->
                    none;
                _ ->
                    mempool
            end
    end.

genesis_header() ->
    aec_block_genesis:genesis_header().

root_hash() ->
    <<"root-hash.......................">>.

txs_hash() ->
    <<"tx-hash.........................">>.

pof_hash() ->
    <<"pof-hash........................">>.

state_hash() ->
    <<"state-hash......................">>.

new_keypair() ->
    #{public := PK, secret := SK} = enacl:sign_keypair(),
    #{pubkey => PK, privkey => SK}.

%% Tx creation - most values don't matter for the tests, but must be present
%% in order to create the transaction objects
%%
create_tx(Config) ->
    I = ?config(initiator, Config),
    R = ?config(responder, Config),
    Nonce = next_nonce(I),
    TxI = #{ mod              => aesc_create_tx
           , initiator_id     => I
           , initiator_amount => 10
           , responder_id     => R
           , responder_amount => 10
           , channel_reserve  => 10
           , lock_period      => 3
           , fee              => 1
           , state_hash       => state_hash()
           , nonce            => Nonce },
    create_and_sign_tx(TxI).

deposit_tx(ChId, Config) ->
    Round = channel_round(ChId),
    I = ?config(initiator, Config),
    Nonce = next_nonce(I),
    TxI = #{ mod            => aesc_deposit_tx
           , channel_id     => aeser_id:create(channel, ChId)
           , from_id        => I
           , amount         => 10
           , fee            => 1
           , state_hash     => state_hash()
           , round          => Round
           , nonce          => Nonce },
    create_and_sign_tx(TxI).

new_channel(#{ channel_id       := _ChId
             , initiator_id     := InitiatorId
             , initiator_amount := InitiatorAmt
             , responder_id     := ResponderId
             , responder_amount := ResponderAmt
             , channel_reserve  := ChanReserve
             , lock_period      := LockPeriod
             , state_hash       := StateHash
             , nonce            := Nonce } = _CreateTx) ->
    Protocol = 5,    %% LIMA protocol, not that it's likely to matter here
    Initiator = aeser_id:specialize(InitiatorId, account),
    Responder = aeser_id:specialize(ResponderId, account),
    %% TODO: assert that the above ChId is the same as for the channel object
    aesc_channels:new(Initiator, InitiatorAmt,
                      Responder, ResponderAmt,
                      aec_accounts:new(Initiator, InitiatorAmt),
                      aec_accounts:new(Responder, ResponderAmt),
                      ChanReserve, _Delegates = [], StateHash,
                      LockPeriod, Nonce, Protocol, _Round = 1).

channel_round(ChId) ->
    case chain_req({get_channel, ChId}) of
        undefined ->
            error({unknown_channel, ChId});
        {ok, Ch} ->
            aesc_channels:round(Ch)
    end.

next_nonce(Acct) ->
    chain_req({next_nonce, Acct}).

%% Chain simulator requests

push(Tx) ->
    chain_req({push, Tx}).

add_keyblock() ->
    chain_req(add_key).

add_microblock() ->
    chain_req(add_micro).

start_chain_process() ->
    P = proc_lib:spawn(
          fun() ->
                  gproc:reg({n,l,{?MODULE, chain_process}}),
                  Chain = new_chain(),
                  ?LOG("Initial chain: ~p", [Chain]),
                  chain_process_loop(Chain)
          end),
    {ok, P}.

stop_chain_process() ->
    case gproc:where({n,l,{?MODULE, chain_process}}) of
        undefined ->
            ?LOG("No chain process running!!", []),
            ok;
        P when is_pid(P) ->
            MRef = monitor(process, P),
            exit(P, kill),
            receive
                {'DOWN', MRef, _, _, _} ->
                    ok
            end
    end.

chain_process_loop(Chain) ->
    receive
        {From, Req} ->
            {Reply, Chain1} = handle_chain_req(Req, Chain),
            From ! {self(), Reply},
            chain_process_loop(Chain1)
    end.

handle_chain_req(R, Chain) ->
    try handle_chain_req_(R, Chain)
    catch error:Err ->
            ?LOG("CAUGHT error:~p / ~p", [Err, erlang:get_stacktrace()]),
            error(Err)
    end.

handle_chain_req_(add_micro, Chain) ->
    {ok, add_microblock_(Chain)};
handle_chain_req_(add_key, Chain) ->
    {ok, add_keyblock_(Chain)};
handle_chain_req_({push, Tx}, #{mempool := Pool} = Chain) ->
    %% TODO: not yet asserting increasing nonces
    {ok, Chain#{mempool => [Tx|Pool]}};
handle_chain_req_({next_nonce, Acct}, #{nonces := Nonces} = Chain) ->
    N = maps:get(Acct, Nonces, 0),
    NewN = N + 1,
    {NewN, Chain#{nonces => Nonces#{ Acct => NewN }}};
handle_chain_req_(top_block_hash, Chain) ->
    {top_block_hash(Chain), Chain};
handle_chain_req_({get_block_state, Hash}, Chain) ->
    {get_block_state(Hash, Chain), Chain};
handle_chain_req_({get_header, Hash}, Chain) ->
    {get_header(Hash, Chain), Chain};
handle_chain_req_({get_channel, ChId}, Chain) ->
    {get_channel(ChId, Chain), Chain};
handle_chain_req_({find_block_tx_hashes, Hash}, Chain) ->
    {find_block_tx_hashes(Hash, Chain), Chain};
handle_chain_req_({find_signed_tx, Hash}, Chain) ->
    {find_signed_tx(Hash, Chain), Chain};
handle_chain_req_({find_tx_with_location, Hash}, Chain) ->
    {find_tx_with_location(Hash, Chain), Chain};
handle_chain_req_({hash_is_in_main_chain, Hash, TopHash}, Chain) ->
    {hash_is_in_main_chain(Hash, TopHash, Chain), Chain};
%%
handle_chain_req_(channel_setup, Chain) ->
    {maps:get(channel_setup, Chain, #{}), Chain};
handle_chain_req_({channel_setup, ChId, Setup}, Chain) ->
    ChSetup = maps:get(channel_setup, Chain, #{}),
    {ok, Chain#{channel_setup => ChSetup#{ ChId => Setup }}}.

chain_req(Req) ->
    chain_req_(Req, get_chain_process()).

get_chain_process() ->
    gproc:where({n, l, {?MODULE, chain_process}}).

chain_req_(Req, ChainP) when is_pid(ChainP) ->
    ?LOG("chain_req(~p)", [Req]),
    MRef = monitor(process, ChainP),
    ChainP ! {self(), Req},
    receive
        {ChainP, Reply} ->
            demonitor(MRef),
            ?LOG("Reply = ~p", [Reply]),
            Reply;
        {'DOWN', MRef, _, _, Reason} ->
            error(Reason)
    after 5000 ->
            error(timeout)
    end.

%% like lists:keyfind/3, but comparing map keys
lists_mapfind(Val, Key, [H|T]) ->
    case maps:find(Key, H) of
        {ok, Val} ->
            H;
        _ ->
            lists_mapfind(Val, Key, T)
    end;
lists_mapfind(_, _, []) ->
    false.

%% ======================================================================
%% Test environment setup

init_per_suite(Config0) ->
    DataDir = ?config(data_dir, Config0),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    Config = [ {top_dir, TopDir}
             , {test_module, ?MODULE}
             , {symlink_name, "latest.watch"} | Config0],
    aecore_suite_utils:make_shortcut(Config),
    {ok, StartedApps} = application:ensure_all_started(gproc),
    ok = mnesia:start(),
    {ok, Apps1} = application:ensure_all_started(lager),
    {ok, Apps2} = application:ensure_all_started(crypto),
    {ok, Apps3} = application:ensure_all_started(enacl),
    #{pubkey := Initiator} = new_keypair(),
    #{pubkey := Responder} = new_keypair(),
    setup_meck(),
    [ {started_apps, StartedApps ++ [mnesia] ++ Apps1 ++ Apps2 ++ Apps3}
    , {initiator, aeser_id:create(account, Initiator)}
    , {responder, aeser_id:create(account, Responder)}
    | Config ].

end_per_suite(Config) ->
    remove_meck(),
    StartedApps = proplists:get_value(started_apps, Config),
    [ok = application:stop(A) || A <- lists:reverse(StartedApps)],
    ok.

init_per_group(GrpName, Config) ->
    ?LOG("init_per_group(~p, Config)", [GrpName]),
    setup_lager(GrpName, Config),
    case GrpName of
        all_tests ->
            Config;
        _ ->
            {ok, ChainP} = start_chain_process(),
            {ok, Watcher} = start_chain_watcher(),
            [{watcher, Watcher}, {chain_process, ChainP} | Config]
    end.

end_per_group(GrpName, Config) ->
    ?LOG("end_per_group(~p, Config)", [GrpName]),
    case GrpName of
        all_tests ->
            ok;
        _ ->
            ok = stop_chain_watcher(Config),
            ok = stop_chain_process(),
            ok
    end.

init_per_testcase(_Test, Config) ->
    ?LOG("~p: Config = ~p", [_Test, Config]),
    case ?config(saved_config, Config) of
        {FromTestCase, SavedConfig} ->
            ?LOG("Reusing config from ~p", [FromTestCase]),
            lists:foldl(
              fun({K, V}, Acc) ->
                      lists:keystore(K, 1, Acc, {K, V})
              end, lists:keydelete(saved_config, 1, Config),
              SavedConfig);
        undefined ->
            Config
    end.

end_per_testcase(_Test, _Config) ->
    ok.

setup_lager(Grp, Config) ->
    LogDir = aecore_suite_utils:shortcut_dir(Config),
    ?LOG("LogDir = ~p", [LogDir]),
    LogFile = filename:join(LogDir, atom_to_list(Grp) ++ ".log"),
    application:stop(lager),
    application:set_env(
      lager, handlers,
      [{lager_file_backend, [{file, LogFile}, {level, debug}]}]),
    application:start(lager).

setup_meck() ->
    meck:expect(aec_chain, find_tx_with_location, 1,
                fun(Hash) ->
                        chain_req({find_tx_with_location, Hash})
                end),
    meck:expect(aec_chain_state, hash_is_in_main_chain, 2,
                fun(Hash, TopHash) ->
                        chain_req({hash_is_in_main_chain, Hash, TopHash})
                end),
    meck:expect(aec_chain, get_block_state, 1,
                fun(Hash) ->
                        chain_req({get_block_state, Hash})
                end),
    meck:expect(aec_chain, get_channel, 2,
                fun(Id, Trees) ->
                        get_channel_(Id, Trees)
                end),
    meck:expect(aec_chain, get_header, 1,
                fun(BHash) ->
                        chain_req({get_header, BHash})
                end),
    meck:expect(aec_db, find_block_tx_hashes, 1,
                fun(Hash) ->
                        chain_req({find_block_tx_hashes, Hash})
                end),
    meck:expect(aec_db, find_signed_tx, 1,
                fun(TxHash) ->
                        chain_req({find_signed_tx, TxHash})
                end),
    ok.

remove_meck() ->
    meck:unload(aec_chain),
    ok.

start_chain_watcher() ->
    Me = self(),
    {Parent, MRef} =
        spawn_monitor(
          fun() ->
                  {ok, W} = aesc_chain_watcher:start_link(),
                  erlang:monitor(process, W),
                  Me ! {self(), ok},
                  receive
                      {'DOWN', _, process, W, Reason} ->
                          ?LOG("Watcher died Reason = ~p", [Reason]),
                          exit(Reason);
                      {From, shutdown} ->
                          ?LOG("got shutdown request", []),
                          unlink(W),
                          exit(W, shutdown),
                          receive
                              {'DOWN', _, process, W, shutdown} ->
                                  ?LOG("watcher terminated", []),
                                  From ! {self(), ok}
                          after 5000 ->
                                  ?LOG("shutdown timeout. Msgs = ~p",
                                       [element(2, process_info(self(), messages))])
                          end
                  end
          end),
    receive
        {Parent, ok} ->
            demonitor(MRef),
            {ok, Parent};
        {'DOWN', MRef, _, _, Reason} ->
            error(Reason)
    after 5000 ->
            error(timeout)
    end.

stop_chain_watcher(Config) ->
    {_, WatcherParent} = lists:keyfind(watcher, 1, Config),
    ?LOG("WatcherParent alive? ~p", [erlang:is_process_alive(WatcherParent)]),
    WatcherParent ! {self(), shutdown},
    receive
        {WatcherParent, ok} ->
            ok
    after 5000 ->
            ?LOG("stop timeout. Parent = ~p", [process_info(WatcherParent)]),
            error(timeout)
    end.

%%
%% ======================================================================
