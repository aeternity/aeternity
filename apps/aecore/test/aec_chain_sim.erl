%%% -*- mode: erlang; erlang-indent-level: 4; erlang-tabs-mode: nil -*-
-module(aec_chain_sim).

%%% ======================================================================
%%%
%%% Chain simulator for Aeternity node testing
%%% Author: Ulf Wiger <ulf@wiger.net>
%%%
%%% This module was initially an integrated part of aesc_chain_watcher_SUITE.erl
%%% and is particularly meant to simplify testing of forking scenarios.
%%% The simulator installs a number of mocks, diverting calls to chain API
%%% functions to the chain simulator process. The chain simulator provides an
%%% API for updating the chain, including support for adding key- and microblocks
%%% to a given fork point, and triggering fork switches.
%%%
%%% The chain simulator contains the following functions:
%%% - block lists: the simulated chain. Blocks are 'proper', in the sense that
%%%   they pass validation.
%%% - State trees. Currently, the only tree objects handled are channels. The
%%%   state tree is a map, where elements are stored as `{Type, Id} => Object`
%%% - Mempool: no particular transaction ordering, ttl or gc. Simply a list of
%%%   txs
%%% - Nonces: a map of `Acct => Nonce' pairs + support for incrementing an acct
%%%   nonce
%%% - Dictionary: for storing and retreiving (mainly) test-related state
%%% - Event reporting: the simulator uses the standard aec_events reporting
%%%   mechanism.
%%%
%%% TODO: Lots, if the code is meant to be used for anything other than the chain
%%% watcher suite. Add mocks, checks and update functions as needed for
%%% incremental improvement.
%%%
%%% ======================================================================

-export([ start/0                      %% () -> {ok, pid()}
        , start/1                      %% (Opts :: map()) -> {ok, pid()}
        , stop/0 ]).                   %% () -> ok

-export([ next_nonce/1                 %% (Acct) -> integer()
        , push/1                       %% (Tx) -> ok
        , add_keyblock/0               %% () -> {ok, Block}
        , add_keyblock/1               %% (ForkId) -> {ok, Block}
        , add_microblock/0             %% () -> {ok, Block}
        , add_microblock/1             %% (ForkId) -> {ok, Block}
        , clone_microblock_on_fork/2   %% (BlockHash, ForkId) -> {ok, Block}
        , fork_from_hash/2             %% (ForkId, FromHash) -> {ok, Block}
        , fork_switch/1                %% (ForkId) -> ok
        , dict_set/2                   %% (Key, Value) -> ok
        , dict_get/2 ]).               %% (Key, Default) -> Value

-export([ setup_meck/0                 %% () -> ok
        , remove_meck/0                %% () -> ok
        , get_chain_process/0          %% () -> pid() | undefined
        ]).

%% Gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-record(st, { opts = #{}
            , mref
            , dict = #{}
            , chain }).

-type fork_id()    :: main | term().
-type block_hash() :: aec_blocks:block_header_hash().

-type sim_microblock() :: #{ hash   => block_hash()
                           , prev   => block_hash()
                           , header => aec_headers:micro_header()
                           , txs    => [ aetx_sign:signed_tx() ]
                           }.

-type sim_keyblock() :: #{ hash   => block_hash()
                         , prev   => block_hash()
                         , header => aec_headers:micro_header()
                         }.

%% TODO: Use CT logging or system logging - possibly configurable
-define(LOG(Fmt, Args), io:fwrite("~w:~w/~w - " ++ Fmt, [?MODULE, ?FUNCTION_NAME, ?LINE | Args])).

%%% @equiv start(#{}).
%%
start() ->
    start(#{}).

-spec start(map()) -> {ok, pid()}.
%%
%% Starts the simulator
%% Supported options:
%% - monitor => pid(): The simulator will terminate if the monitored process dies
%%
start(Opts) when is_map(Opts) ->
    gen_server:start(?MODULE, Opts, []).

-spec stop() -> ok.
%%
%% Stops the simulator
%%
stop() ->
    case get_chain_process() of
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


%% Chain simulator requests

-spec next_nonce(Acct :: aec_keys:pubkey()) -> integer().
%%
%% Increments the nonce of Acct
%%
next_nonce(Acct) ->
    chain_req({next_nonce, Acct}).

-spec push(aetx_sign:signed_tx()) -> ok.
%%
%% Pushes the signed tx to the simulated mempool
%%
push(Tx) ->
    chain_req({push, Tx}).

-spec add_keyblock() -> sim_keyblock().
%%
%% Equivalent to add_keyblock(main)
%%
add_keyblock() ->
    add_keyblock(main).

-spec add_keyblock( ForkId :: fork_id() ) -> {ok, sim_keyblock()}.
%%
%% Adds a keyblock. If ForkId == main, the keyblock is added to the main fork.
%% If ForkId is the id of an existing fork, created previously with
%% fork_from_hash/2, the keyblock is added to that fork. If the given ForkId
%% is not known, the simulator terminates.
%%
add_keyblock(ForkId) ->
    chain_req({add_key, ForkId}).

-spec add_microblock() -> {ok, sim_microblock()}.
%%
%% Equivalent to add_microblock(main)
%%
add_microblock() ->
    add_microblock(main).

-spec add_microblock(ForkId :: fork_id()) -> {ok, sim_microblock()}.
%%
%% Adds a microblock to the given ForkId (the main fork if ForkId == main).
%% All transactions in the mempool are added to the block.
%%
add_microblock(ForkId) ->
    chain_req({add_micro, ForkId}).

-spec clone_microblock_on_fork(block_hash(), fork_id()) -> {ok, sim_microblock()}.
%%
%% This is a cheating way of simulating transactions being evicted from the main chain and
%% picked up in a new microblock. The cheat is that the original block remains on the chain.
%% This function should be used with care, and ideally replaced by a better solution.
%%
clone_microblock_on_fork(Hash, ForkId) ->
    chain_req({clone_micro_on_fork, Hash, ForkId}).

-spec fork_from_hash(ForkId :: fork_id(), FromHash :: block_hash()) -> {ok, sim_keyblock()}.
%%
%% Creates a new ForkId and adds a keyblock to FromHash
%% Fails (simulator terminates) if ForkId exists, or if FromHash is not a block
%% on the main fork.
%%
fork_from_hash(ForkId, FromHash) when is_binary(FromHash) ->
    chain_req({fork_from_hash, ForkId, FromHash}).

-spec fork_switch(ForkId :: fork_id()) -> ok.
%%
%% Makes the existing fork ForkId the new main fork, evicting all blocks
%% succeeding the common ancestor. The evicted transactions are returned to the
%% simulated mempool.
%%
fork_switch(ForkId) ->
    chain_req({fork_switch, ForkId}).

-spec dict_set(Key :: term(), Value :: term()) -> ok.
%%
%% Stores `Key => Value' in the "user dictionary" managed by the chain simulator.
%%
dict_set(Key, Value) ->
    chain_req({dict_set, Key, Value}).

-spec dict_get(Key :: term(), Default :: term()) -> term().
%%
%% Returns the value associated with Key in the user dictionary, or Default if
%% the key is not found.
%%
dict_get(Key, Default) ->
    chain_req({dict_get, Key, Default}).

-spec setup_meck() -> ok.
%%
%% Installs the mocks needed for the simulator. The assumption is that the mocks
%% are not already installed.
%%
setup_meck() ->
    meck:expect(aec_chain_state, hash_is_in_main_chain, 2,
                fun(Hash, TopHash) ->
                        chain_req({hash_is_in_main_chain, Hash, TopHash})
                end),
    meck:expect(aec_chain_state, find_common_ancestor, 2,
                fun(Hash1, Hash2) ->
                        chain_req({find_common_ancestor, Hash1, Hash2})
                end),
    meck:expect(aec_chain, find_tx_with_location, 1,
                fun(Hash) ->
                        chain_req({find_tx_with_location, Hash})
                end),
    meck:expect(aec_chain, get_block_state, 1,
                fun(Hash) ->
                        chain_req({get_block_state, Hash})
                end),
    meck:expect(aec_chain, get_channel, 1,
                fun(Id) ->
                        chain_req({get_channel, Id})
                end),
    meck:expect(aec_chain, get_channel, 2,
                fun(Id, Trees) ->
                        get_channel_from_trees_(Id, Trees)
                end),
    meck:expect(aec_chain, top_block_hash, 0,
                fun() ->
                        chain_req(top_block_hash)
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

-spec remove_meck() -> ok.
%%
%% Removes the mocks installed by the simulator.
%%
remove_meck() ->
    meck:unload([ aec_chain
                , aec_chain_state
                , aec_db ]),
    ok.

%% gen_server implementation

init(Opts) when is_map(Opts) ->
    gproc:reg({n,l,{?MODULE, chain_process}}),
    Chain = new_chain(),
    ?LOG("Initial chain: ~p", [Chain]),
    {ok, maybe_monitor(#st{opts = Opts, chain = Chain})}.

maybe_monitor(#st{opts = #{monitor := Pid}} = St) when is_pid(Pid) ->
    MRef = erlang:monitor(process, Pid),
    St#st{mref = MRef};
maybe_monitor(St) ->
    St.

handle_call({add_micro, ForkId}, _From, #st{chain = Chain} = St) ->
    {Res, Chain1} = add_microblock_(ForkId, Chain),
    {reply, Res, St#st{chain = Chain1}};
handle_call({clone_micro_on_fork, Hash, ForkId}, _From, #st{chain = Chain} = St) ->
    {Res, Chain1} = clone_micro_on_fork_(Hash, ForkId, Chain),
    {reply, Res, St#st{chain = Chain1}};
handle_call({add_key, ForkId}, _From, #st{chain = Chain} = St) ->
    {Res, Chain1} = add_keyblock_(ForkId, Chain),
    {reply, Res, St#st{chain = Chain1}};
handle_call({fork_from_hash, ForkId, FromHash}, _From, #st{chain = Chain} = St) ->
    {Res, Chain1} = fork_from_hash_(ForkId, FromHash, Chain),
    {reply, Res, St#st{chain = Chain1}};
handle_call({fork_switch, ForkId}, _From, #st{chain = Chain} = St) ->
    {Res, Chain1} = fork_switch_(ForkId, Chain),
    {reply, Res, St#st{chain = Chain1}};
handle_call({push, Tx}, _From, #st{chain = #{mempool := Pool} = Chain} = St) ->
    %% TODO: not yet asserting increasing nonces
    {reply, ok, St#st{chain = Chain#{mempool => [Tx|Pool]}}};
handle_call({next_nonce, Acct}, _From, #st{chain = #{nonces := Nonces} = Chain} = St) ->
    N = maps:get(Acct, Nonces, 0),
    NewN = N + 1,
    {reply, NewN, St#st{chain = Chain#{nonces => Nonces#{ Acct => NewN }}}};
handle_call(top_block_hash, _From, #st{chain = Chain} = St) ->
    {reply, top_block_hash_(Chain), St};
handle_call({get_block_state, Hash}, _From, #st{chain = Chain} = St) ->
    {reply, get_block_state_(Hash, Chain), St};
handle_call({get_header, Hash}, _From, #st{chain = Chain} = St) ->
    {reply, get_header_(Hash, Chain), St};
handle_call({get_channel, ChId}, _From, #st{chain = Chain} = St) ->
    {reply, get_channel_(ChId, Chain), St};
handle_call({find_block_tx_hashes, Hash}, _From, #st{chain = Chain} = St) ->
    {reply, find_block_tx_hashes_(Hash, Chain), St};
handle_call({find_signed_tx, Hash}, _From, #st{chain = Chain} = St) ->
    {reply, find_signed_tx_(Hash, Chain), St};
handle_call({find_tx_with_location, Hash}, _From, #st{chain = Chain} = St) ->
    {reply, find_tx_with_location_(Hash, Chain), St};
handle_call({hash_is_in_main_chain, Hash, TopHash}, _From, #st{chain = Chain} = St) ->
    {reply, hash_is_in_main_chain_(Hash, TopHash, Chain), St};
handle_call({find_common_ancestor, Hash1, Hash2}, _From, #st{chain = Chain} = St) ->
    {reply, find_common_ancestor_(Hash1, Hash2, Chain), St};
%%
handle_call({dict_get, Key, Default}, _From, #st{dict = D} = St) ->
    {reply, maps:get(Key, D, Default), St};
handle_call({dict_set, Key, Value}, _From, #st{dict = D} = St) ->
    {reply, ok, St#st{dict = D#{Key => Value}}}.

handle_cast(_Msg, Chain) ->
    {noreply, Chain}.

handle_info({'DOWN', MRef, _, Pid, Reason}, #st{mref = MRef} = St) ->
    ?LOG("Received 'DOWN' from monitored ~p: ~p", [Pid, Reason]),
    {stop, normal, St};
handle_info(_Msg, Chain) ->
    {noreply, Chain}.

terminate(_Reason, _Chain) ->
    ok.

code_change(_FromVsn, C, _Extra) ->
    {ok, C}.

%% Called from the chain process
add_microblock_(ForkId, #{mempool := Pool} = Chain) ->
    Txs = lists:reverse(Pool),
    add_microblock_(ForkId, Txs, Chain#{mempool => []}).

add_microblock_(ForkId, Txs, #{forks := Forks} = Chain) ->
    ?LOG("add_microblock(Txs = ~p", [Txs]),
    #{blocks := Blocks} = F = maps:get(ForkId, Forks),
    #{hash := PrevHash, header := TopHdr} = hd(Blocks),
    ?LOG("PrevHash = ~p", [PrevHash]),
    PrevKeyHash = aec_headers:prev_key_hash(TopHdr),
    Height = aec_headers:height(TopHdr),
    NewHdr = aec_headers:new_micro_header(
               Height, PrevHash, PrevKeyHash,
               root_hash(), 0, txs_hash(), pof_hash(), 0),
    {ok, BlockHash} = aec_headers:hash_header(NewHdr),
    Block = maybe_update_trees(ForkId, #{ hash   => BlockHash
                                        , prev   => PrevHash
                                        , header => NewHdr
                                        , txs    => Txs }, Chain),
    ?LOG("Microblock = ~p", [Block]),
    NewFork = F#{blocks => [Block | Blocks]},
    ?LOG("NewFork(~p): ~p", [ForkId, NewFork]),
    NewForks = Forks#{ForkId => NewFork},
    NewChain = announce(ForkId, Txs, Chain#{ forks => NewForks}),
    {{ok, Block}, NewChain}.

clone_micro_on_fork_(Hash, ForkId, #{forks := Forks} = Chain) ->
    #{blocks := Blocks} = maps:get(main, Forks),
    case lists_mapfind(Hash, hash, Blocks) of
        false ->
            error({no_such_hash, Hash});
        #{txs := Txs} ->
            add_microblock_(ForkId, Txs, Chain)
    end.

fork_from_hash_(ForkId, Hash, #{ forks   := Forks } = Chain) ->
    case maps:is_key(ForkId, Forks) of
        true ->
            error({fork_id_exists, ForkId});
        false ->
            #{blocks := Blocks} = maps:get(main, Forks),
            case blocks_until_hash(Hash, Blocks) of
                [] ->
                    error({no_such_hash, Hash});
                BlocksUntilHash ->
                    NewFork = #{ fork_point => Hash
                               , blocks     => BlocksUntilHash },
                    NewForks = Forks#{ForkId => NewFork},
                    %% Automatically add a keyblock as instigator of the fork.
                    add_keyblock_(ForkId, Chain#{forks => NewForks})
            end
    end.

fork_switch_(ForkId, #{forks := Forks, mempool := Pool, orphans := Orphans} = Chain) ->
    #{ main   := #{blocks := Blocks} = M
     , ForkId := #{fork_point := ForkPoint, blocks := FBlocks}} = Forks,
    Evict = lists:takewhile(
              fun(#{hash := H}) ->
                      H =/= ForkPoint
              end, Blocks),
    ReturnTxs = lists:flatten([ Txs || #{txs := Txs} <- lists:reverse(Evict)]),
    ?LOG("Evicting txs: ~p", [ReturnTxs]),
    NewPool = lists:reverse(ReturnTxs) ++ Pool,
    NewForks = maps:remove(ForkId, Forks#{main => M#{blocks => FBlocks}}),
    NewChain = Chain#{ forks => NewForks
                     , mempool => NewPool
                     , orphans => Evict ++ Orphans },
    {ok, announce(main, [], NewChain)}.

%% Announce top_changed and tx events
announce(ForkId, Txs, #{ forks := Forks } = Chain) ->
    #{ ForkId := #{ blocks := [#{ hash := TopHash
                                , prev := PrevHash
                                , header := Hdr } | _] = Blocks} } = Forks,
    Height = length(Blocks) + 1,
    Type = aec_headers:type(Hdr),
    Origin = origin(Type),
    Info = #{ block_hash   => TopHash
            , block_type   => Type
            , block_origin => Origin
            , prev_hash    => PrevHash
            , height       => Height },
    send_tx_events(Txs, Info),
    case ForkId of
        main ->
            ?LOG("Publishing top_changed, I = ~p", [Info]),
            aec_events:publish(top_changed, Info);
        _ ->
            ignore
    end,
    Chain.

origin(key) ->
    block_created;
origin(micro) ->
    micro_block_created.

%% ======================================================================
%% Block generation
%% Chain represented as a LIFO list of #{hash, header, txs} entries
%% wrapped inside a #{blocks, miner => #{privkey,pubkey}} map
new_chain() ->
    Miner = new_keypair(),
    Hdr = genesis_header(),
    {ok, Hash} = aec_headers:hash_header(Hdr),
    Blocks = [#{hash => Hash, header => genesis_header(), txs => []}],
    #{ miner   => Miner
     , mempool => []
     , orphans => []
     , nonces  => #{}
     , forks   => #{ main => #{ fork_point => Hash
                              , blocks     => Blocks } }}.

%% Called from the chain process
add_keyblock_(ForkId, #{forks := Forks, miner := #{pubkey := Miner}} = Chain) ->
    ?LOG("add_keyblock(~p)", [ForkId]),
    #{ ForkId := #{blocks := Blocks} = F } = Forks,
    #{hash := PrevHash, header := TopHdr} = hd(Blocks),
    PrevKeyHash = aec_headers:prev_key_hash(TopHdr),
    Height = aec_headers:height(TopHdr),
    NewHdr = aec_headers:new_key_header(
               Height+1, PrevHash, PrevKeyHash, root_hash(),
               Miner, Miner, 0, 0, 0, 0, default, 0),
    {ok, BlockHash} = aec_headers:hash_header(NewHdr),
    Block = #{ hash   => BlockHash
             , prev   => PrevHash
             , header => NewHdr
             , txs    => [] },
    NewChain = Chain#{ forks => Forks#{ ForkId => F#{blocks => [Block | Blocks]} } },
    announce(ForkId, [], NewChain),
    {{ok, Block},  NewChain}.

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

maybe_update_trees(_, #{txs := []} = Block, _Chain) ->
    Block;
maybe_update_trees(ForkId, #{txs := Txs} = Block, Chain) ->
    Blocks = blocks(ForkId, Chain),
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

top_block_hash_(Chain) ->
    [#{hash := Hash}|_] = blocks(main, Chain),
    Hash.

get_block_state_(Hash, Chain) ->
    trees(blocks_until_hash(Hash, blocks(Chain))).

hash_is_in_main_chain_(Hash, TopHash, Chain) ->
    case blocks_until_hash(TopHash, blocks(Chain)) of
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

find_common_ancestor_(Hash1, Hash2, #{forks := Forks, orphans := Orphans}) ->
    case {search_forks_for_hash(Hash1, Forks, Orphans),
          search_forks_for_hash(Hash2, Forks, Orphans)} of
        {[_|_] = Blocks1, [_|_] = Blocks2} ->
            find_common_(Blocks1, Blocks2);
        _ ->
            {error, not_found}
    end.

search_forks_for_hash(H, Forks, Orphans) ->
    case search_forks_for_hash_(H, Forks) of
        none ->
            ?LOG("~p not in forks", [H]),
            case blocks_until_hash(H, Orphans) of
                [#{prev := Prev}|_] ->
                    ?LOG("~p found in orphans; trying its Prev (~p)", [H, Prev]),
                    search_forks_for_hash(Prev, Forks, Orphans);
                [] ->
                    ?LOG("~p also not an orphan", [H]),
                    none
            end;
        [_|_] = Result ->
            ?LOG("Found ~p in forks", [H]),
            Result
    end.

search_forks_for_hash_(H, Forks) ->
    try maps:fold(
          fun(ForkId, #{blocks := Blocks}, none) ->
                  case blocks_until_hash(H, Blocks) of
                      [] ->
                          none;
                      Found ->
                          throw({ForkId, Found})
                  end
          end, none, Forks)
    catch
        throw:{_FId, Result} ->
            Result
    end.

find_common_([#{hash := H1}|T1], Blocks2) ->
    case lists_mapfind(H1, hash, Blocks2) of
        false ->
            find_common_(T1, Blocks2);
        _Found ->
            {ok, H1}
    end;
find_common_([], _) ->
    {error, not_found}.

get_header_(Hash, Chain) ->
    case blocks_until_hash(Hash, blocks(Chain)) of
        [#{header := Header}|_] ->
            {ok, Header};
        [] ->
            error
    end.

get_channel_(ChId, Chain) ->
    case trees(blocks(Chain)) of
        {ok, Trees} ->
            get_channel_from_trees_(ChId, Trees);
        error ->
            undefined
    end.

get_channel_from_trees_(Id, Trees) ->
    case maps:find({channel,Id}, Trees) of
        {ok, _} = Ok ->
            Ok;
        error ->
            undefined
    end.

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
    aesc_channels:new(aec_accounts:new(Initiator, InitiatorAmt), InitiatorAmt,
                      aec_accounts:new(Responder, ResponderAmt), ResponderAmt,
                      ChanReserve, _Delegates = [], StateHash,
                      LockPeriod, Nonce, Protocol, _Round = 1).

find_block_tx_hashes_(Hash, Chain) ->
    case blocks_until_hash(Hash, blocks(Chain)) of
        [#{txs := Txs}|_] ->
            TxHashes = [ H || #{tx_hash := H} <- Txs ],
            {value, TxHashes};
        [] ->
            not_found
    end.

find_signed_tx_(Hash, Chain) ->
    case find_signed_tx_in_blocks_(blocks(Chain), Hash) of
        {value, STx, _Block} ->
            {value, STx};
        none ->
            none
    end.

find_signed_tx_in_blocks_([#{txs := Txs} = Block|T], Hash) ->
    case lists_mapfind(Hash, tx_hash, Txs) of
        #{signed_tx := STx} ->
            {value, STx, Block};
        false ->
            find_signed_tx_in_blocks_(T, Hash)
    end;
find_signed_tx_in_blocks_([], _) ->
    none.

find_tx_with_location_(Hash, #{forks := Forks, mempool := Pool}) ->
    %% When aec_chain_state processes a fork switch, it changes tx_location from
    %% the evicted blocks to the mempool. So this function should only return a
    %% block has if the tx is found on the main chain.
    #{blocks := Blocks} = maps:get(main, Forks),
    case find_signed_tx_in_blocks_(Blocks, Hash) of
        {value, STx, #{hash := BlockHash}} ->
            {BlockHash, STx};
        none ->
            case lists_mapfind(Hash, tx_hash, Pool) of
                false ->
                    none;
                #{signed_tx := STx} ->
                    {mempool, STx}
            end
    end.

blocks(Chain) ->
    blocks(main, Chain).

blocks(ForkId, #{forks := Forks}) ->
    #{blocks := Blocks} = maps:get(ForkId, Forks),
    Blocks.

genesis_header() ->
    aec_block_genesis:genesis_header().

root_hash() ->
    <<"root-hash.......................">>.

txs_hash() ->
    <<"tx-hash.........................">>.

pof_hash() ->
    <<"pof-hash........................">>.

chain_req(Req) ->
    chain_req_(Req, get_chain_process()).

get_chain_process() ->
    gproc:where({n, l, {?MODULE, chain_process}}).

chain_req_(Req, ChainP) when is_pid(ChainP) ->
    ?LOG("chain_req(~p)", [Req]),
    gen_server:call(ChainP, Req).

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

new_keypair() ->
    #{public := PK, secret := SK} = enacl:sign_keypair(),
    #{pubkey => PK, privkey => SK}.

