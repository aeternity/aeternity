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
        , new_account/1                %% (Balance) -> KeyPair
        , new_account/2                %% (ForkId, Balance) -> KeyPair
        , get_balance/1                %% (Acct) -> Balance
        , get_balance/2                %% (ForkId, Acct) -> Balance
        , push/1                       %% (Tx) -> ok
        , find_signed_tx/1             %% (TxHash) -> {value, STx)} | none
        , top_block_hash/0             %% () -> Hash
        , block_by_hash/1              %% (BlockHash) -> {ok, Block}
        , sign_and_push/2              %% (Acct, Tx) -> ok
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

-type simulator() :: default | parent_chain.

-type fork_id()    :: main | term().
-type block_hash() :: aec_blocks:block_header_hash().


%% TODO: Use CT logging or system logging - possibly configurable
-define(LOG(Fmt, Args), io:fwrite(user, "~w:~w/~w - " ++ Fmt, [?MODULE, ?FUNCTION_NAME, ?LINE | Args])).

%%% @equiv start(#{}).
%%
start() ->
    start(#{}).

-spec start(map()) -> {ok, pid()}.
%%
%% Starts the simulator
%% Supported options:
%% - monitor => pid(): The simulator will terminate if the monitored process dies
%% - simulator => simulator(): The simulator mode (parent chain for Hyperchains or default)
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


-spec simulator(map()) -> simulator().
%%
%% Simulator type. default is used mostly in test suites.
%% parent_chain is used to run the process as attached "parent chain"
%% to reproduce various parent chain scenarios for Hyperchains;
%%
simulator(Opts) ->
    Type = maps:get(simulator, Opts, default),
    true = (Type == default orelse Type == parent_chain),
    Type.

%%%===================================================================
%%% Chain simulator requests
%%%===================================================================

-spec next_nonce(Acct :: aec_keys:pubkey()) -> integer().
%%
%% Increments the nonce of Acct
%%
next_nonce(Acct) ->
    chain_req({next_nonce, Acct}).

-spec new_account(Balance :: non_neg_integer()) -> aec_keys:pubkey().
%%
%% Equivalent to new_account(main, Balance)
%%
new_account(Balance) ->
    new_account(main, Balance).

-spec new_account(fork_id(), Balance :: non_neg_integer()) -> aec_keys:pubkey().
%%
%% Creates new account with given balance
%%
new_account(ForkId, Balance) ->
    chain_req({new_account, ForkId, Balance}).

-spec get_balance(Acct :: non_neg_integer()) -> integer().
%%
%% Equivalent to get_balance(main, Acct)
%%
get_balance(Acct) ->
    get_balance(main, Acct).

-spec get_balance(fork_id(), Acct :: non_neg_integer()) -> integer().
%%
%% Queries the chain for account's balance
%%
get_balance(ForkId, Acct) ->
    chain_req({get_balance, ForkId, Acct}).


-spec push(aetx_sign:signed_tx()) -> ok.
%%
%% Pushes the signed tx to the simulated mempool
%%
push(Tx) ->
    chain_req({push, Tx}).

-spec sign_and_push(aec_keys:pubkey(), aetx_sign:signed_tx()) -> ok | {error, unknown_privkey}.
%%
%% Signs and pushes tx to the simulated mempool
%%
sign_and_push(Account, Tx) ->
    chain_req({sign_and_push, Account, Tx}).

-spec add_keyblock() -> aec_blocks:key_block().
%%
%% Equivalent to add_keyblock(main)
%%
add_keyblock() ->
    add_keyblock(main).

-spec block_by_hash(block_hash()) -> {ok, aec_blocks:block()}.
block_by_hash(BlockHash) ->
    chain_req({block_by_hash, BlockHash}).

-spec top_block_hash() -> binary().
top_block_hash() ->
    chain_req(top_block_hash).

-spec add_keyblock( ForkId :: fork_id() ) -> {ok, aec_blocks:key_block()}.
%%
%% Adds a keyblock. If ForkId == main, the keyblock is added to the main fork.
%% If ForkId is the id of an existing fork, created previously with
%% fork_from_hash/2, the keyblock is added to that fork. If the given ForkId
%% is not known, the simulator terminates.
%%
add_keyblock(ForkId) ->
    chain_req({add_key, ForkId}).

-spec add_microblock() -> {ok, aec_blocks:micro_block()}.
%%
%% Equivalent to add_microblock(main)
%%
add_microblock() ->
    add_microblock(main).

-spec add_microblock(ForkId :: fork_id()) -> {ok, aec_blocks:micro_block()}.
%%
%% Adds a microblock to the given ForkId (the main fork if ForkId == main).
%% All transactions in the mempool are added to the block.
%%
add_microblock(ForkId) ->
    chain_req({add_micro, ForkId}).

-spec clone_microblock_on_fork(block_hash(), fork_id()) -> {ok, aec_blocks:micro_block()}.
%%
%% This is a cheating way of simulating transactions being evicted from the main chain and
%% picked up in a new microblock. The cheat is that the original block remains on the chain.
%% This function should be used with care, and ideally replaced by a better solution.
%%
clone_microblock_on_fork(Hash, ForkId) ->
    chain_req({clone_micro_on_fork, Hash, ForkId}).

-spec fork_from_hash(ForkId :: fork_id(), FromHash :: block_hash()) -> {ok, aec_blocks:key_block()}.
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

-spec find_signed_tx(binary()) -> {value, binary()} | none.
%%
%% Returns the transaction hash associated with Key
%%
find_signed_tx(TxHash) ->
    chain_req({find_signed_tx, TxHash}).

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
    meck:expect(aec_chain, get_top_state, 0,
                fun() ->
                        chain_req(get_top_state)
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


%%%===================================================================
%$% gen_server implementation
%%%===================================================================

init(Opts) when is_map(Opts) ->
    gproc:reg({n,l,{?MODULE, chain_process}}),
    Chain = new_chain(),
    ?LOG("Initial chain (~p simulator): ~p", [simulator(Opts), Chain]),
    {ok, maybe_monitor(#st{opts = Opts, chain = Chain})}.

maybe_monitor(#st{opts = #{monitor := Pid}} = St) when is_pid(Pid) ->
    MRef = erlang:monitor(process, Pid),
    St#st{mref = MRef};
maybe_monitor(St) ->
    St.

handle_call({add_micro, ForkId}, _From, #st{opts = Opts, chain = Chain} = St) ->
    {Res, Chain1} = add_microblock_(ForkId, Chain, Opts),
    {reply, Res, St#st{chain = Chain1}};
handle_call({clone_micro_on_fork, Hash, ForkId}, _From, #st{opts = Opts, chain = Chain} = St) ->
    {Res, Chain1} = clone_micro_on_fork_(Hash, ForkId, Chain, Opts),
    {reply, Res, St#st{chain = Chain1}};
handle_call({new_account, ForkId, Balance}, _From, #st{chain = Chain} = St) ->
    {Res, Chain1} = new_account_(ForkId, Balance, Chain),
    {reply, Res, St#st{chain = Chain1}};
handle_call({add_key, ForkId}, _From, #st{opts = Opts, chain = Chain} = St) ->
    {Res, Chain1} = add_keyblock_(ForkId, Chain, Opts),
    {reply, Res, St#st{chain = Chain1}};
handle_call({get_balance, ForkId, Acct}, _From, #st{chain = Chain} = St) ->
    Res = get_balance_(ForkId, Acct, Chain),
    {reply, Res, St};
handle_call({fork_from_hash, ForkId, FromHash}, _From, #st{opts = Opts, chain = Chain} = St) ->
    {Res, Chain1} = fork_from_hash_(ForkId, FromHash, Chain, Opts),
    {reply, Res, St#st{chain = Chain1}};
handle_call({fork_switch, ForkId}, _From, #st{opts = Opts, chain = Chain} = St) ->
    {Res, Chain1} = fork_switch_(ForkId, Chain, Opts),
    {reply, Res, St#st{chain = Chain1}};
handle_call({push, Tx}, _From, #st{chain = #{mempool := Pool} = Chain} = St) ->
    %% TODO: not yet asserting increasing nonces
    {reply, ok, St#st{chain = Chain#{mempool => [Tx|Pool]}}};
handle_call( {sign_and_push, PK, Tx}, _From
           , #st{chain = #{ mempool := Pool, key_pairs := KP} = Chain
                } = St) ->
    %% TODO: not yet asserting increasing nonces
    case KP of
        #{PK := SK} ->
            {reply, ok, St#st{chain = Chain#{mempool => [aec_test_utils:sign_tx(Tx, SK)|Pool]}}};
        _ -> {reply, {error, unknown_privkey}, Chain}
    end;
handle_call({next_nonce, Acct}, _From, #st{chain = #{nonces := Nonces} = Chain} = St) ->
    N = maps:get(Acct, Nonces, 0),
    NewN = N + 1,
    {reply, NewN, St#st{chain = Chain#{nonces => Nonces#{ Acct => NewN }}}};
handle_call(top_block_hash, _From, #st{chain = Chain} = St) ->
    {reply, top_block_hash_(Chain), St};
handle_call({block_by_hash, Hash},_From, #st{chain = Chain} = St) ->
    {reply, get_block_(Hash, Chain), St};
handle_call({get_block_state, Hash}, _From, #st{chain = Chain} = St) ->
    {reply, get_block_state_(Hash, Chain), St};
handle_call(get_top_state, _From, #st{chain = Chain} = St) ->
    {reply, get_top_state_(Chain), St};
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


%%%===================================================================
%%% gen_server requests implementations
%%%===================================================================

new_account_(ForkId, Balance, #{forks := Forks} = Chain) ->
    #{pubkey := PK} = KP = new_keypair(),
    Acct = aec_accounts:new(PK, Balance),
    #{ ForkId := #{blocks := [TopBlock|RestBlocks] = Blocks} = F } = Forks,
    Trees = case trees(Blocks) of
                {ok, Ts} ->
                    Ts;
                error ->
                    aec_trees:new_without_backend()
            end,
    NewChain = insert_key_pair(KP, Chain),
    NewTrees = aec_trees:set_accounts(Trees, aec_accounts_trees:enter(Acct, aec_trees:accounts(Trees))),
    NewBlocks = [TopBlock#{trees => NewTrees}|RestBlocks],
    {{ok, KP}, NewChain#{forks => Forks#{ForkId => F#{blocks => NewBlocks}}}}.

get_balance_(ForkId, PK, Chain) ->
    Trees = case trees(blocks(ForkId, Chain)) of
                {ok, Ts} ->
                    Ts;
                error ->
                    aec_trees:new_without_backend()
            end,
    case aec_accounts_trees:lookup(PK, aec_trees:accounts(Trees)) of
        none -> 0;
        {value, Acct} ->
            aec_accounts:balance(Acct)
    end.

add_microblock_(ForkId, #{mempool := Pool} = Chain, Opts) ->
    Txs = lists:reverse(Pool),  % because LIFO
    add_microblock_(ForkId, Txs, Chain#{mempool => []}, Opts).

add_microblock_(ForkId, Txs, #{forks := Forks} = Chain, Opts) ->
    ?LOG("add_microblock(Txs = ~p", [Txs]),
    #{blocks := Blocks} = F = maps:get(ForkId, Forks),
    #{block := B} = hd(Blocks),
    TopHdr = aec_blocks:to_header(B),
    {ok, PrevHash} = aec_headers:hash_header(TopHdr),
    ?LOG("PrevHash = ~p", [PrevHash]),
    PrevKeyHash = case aec_blocks:is_key_block(B) of
                      true -> PrevHash;
                      false -> aec_headers:prev_key_hash(TopHdr)
                  end,
    Height = aec_headers:height(TopHdr),
    NewHdr = aec_headers:new_micro_header(
               Height, PrevHash, PrevKeyHash,
               root_hash(), 0, txs_hash(), pof_hash(), 0),
    Block = aec_blocks:new_micro_from_header(NewHdr, Txs, no_fraud),
    {BlockEntry, Evs} = with_trees(ForkId, Txs, Block, Chain),
    ?LOG("Microblock = ~p", [Block]),
    NewFork = F#{blocks => [BlockEntry | Blocks]},
    ?LOG("NewFork(~p): ~p", [ForkId, NewFork]),
    NewForks = Forks#{ForkId => NewFork},
    NewChain = announce(ForkId, Txs, Evs, Chain#{forks => NewForks}, Opts),
    #{block := FinalMicroBlock} = BlockEntry,
    {{ok, FinalMicroBlock}, NewChain}.

clone_micro_on_fork_(Hash, ForkId, #{forks := Forks} = Chain, Opts) ->
    #{blocks := Blocks} = maps:get(main, Forks),
    BlockHash = fun(#{block := B}) ->
                    {ok, H} = aec_headers:hash_header(aec_blocks:to_header(B)),
                    H
                end,
    case lists_funfind(Hash, BlockHash, Blocks) of
        false ->
            error({no_such_hash, Hash});
        #{block := B} ->
            Txs = aec_blocks:txs(B),
            add_microblock_(ForkId, Txs, Chain, Opts)
    end.

fork_from_hash_(ForkId, Hash, #{ forks   := Forks } = Chain, Opts) ->
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
                    add_keyblock_(ForkId, Chain#{forks => NewForks}, Opts)
            end
    end.

fork_switch_(ForkId, #{forks := Forks, mempool := Pool, orphans := Orphans} = Chain, Opts) ->
    #{ main   := #{blocks := Blocks} = M
     , ForkId := #{fork_point := ForkPoint, blocks := FBlocks}} = Forks,
    Evict = lists:takewhile(
              fun(#{block := B}) ->
                      {ok, H} = aec_headers:hash_header(aec_blocks:to_header(B)),
                      H =/= ForkPoint
              end, Blocks),
    ReturnTxs = lists:flatten([ aec_blocks:txs(B)
        || #{block := B} <- lists:reverse(Evict),
        not aec_blocks:is_key_block(B)]),
    ?LOG("Evicting txs: ~p", [ReturnTxs]),
    NewPool = lists:reverse(ReturnTxs) ++ Pool,
    NewForks = maps:remove(ForkId, Forks#{main => M#{blocks => FBlocks}}),
    NewChain = Chain#{ forks => NewForks
                     , mempool => NewPool
                     , orphans => Evict ++ Orphans },
    {ok, announce(main, [], [], NewChain, Opts)}.

%% Announce top_changed and tx events
announce(ForkId, Txs, Events, #{ forks := Forks } = Chain, Opts) ->
    #{ ForkId := #{ blocks := [#{ block := TopBlock } | _] = Blocks} } = Forks,
    SimulatorT = simulator(Opts),
    Height = length(Blocks) + 1,
    Hdr = aec_blocks:to_header(TopBlock),
    {ok, TopHash} = aec_headers:hash_header(Hdr),
    PrevHash = aec_headers:prev_hash(Hdr),
    Type = aec_headers:type(Hdr),
    Origin = origin(Type),
    Info = #{ pid => self(),
              block_hash   => TopHash
            , block_type   => Type
            , block_origin => Origin
            , prev_hash    => PrevHash
            , height       => Height },
    if SimulatorT == parent_chain ->
            aec_events:publish({parent_chain, top_changed}, Info#{txs => Txs});
       true ->
            send_tx_events(Events, TopHash, Info),
            (ForkId == main) andalso
                begin ?LOG("Publishing top_changed, I = ~p", [Info]),
                      aec_events:publish(top_changed, Info)
                end
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
    {Gen, Tre} = aec_block_genesis:genesis_block_with_state(),
    {ok, Hash} = aec_headers:hash_header(aec_blocks:to_header(Gen)),
    #{ miner     => Miner
     , mempool   => []
     , orphans   => []
     , nonces    => #{}
     , key_pairs => #{}
     , forks     => #{ main => #{ fork_point => Hash
                                , blocks     => [#{block => Gen, trees => Tre}] } }}.

%% Called from the chain process
add_keyblock_(ForkId, #{forks := Forks, miner := #{pubkey := Miner}} = Chain, Opts) ->
    ?LOG("add_keyblock(~p)", [ForkId]),
    #{ ForkId := #{blocks := Blocks} = F } = Forks,
    #{block := Block} = hd(Blocks),
    TopHdr = aec_blocks:to_header(Block),
    {ok, PrevHash} = aec_headers:hash_header(TopHdr),
    PrevKeyHash = aec_headers:prev_key_hash(TopHdr),
    Height = aec_headers:height(TopHdr),
    NewHdr = aec_headers:new_key_header(
               Height+1, PrevHash, PrevKeyHash, root_hash(),
               Miner, Miner, 0, 0, 0, 0, default, 0),
    NewBlock = #{block => aec_blocks:new_key_from_header(NewHdr)},
    NewChain = Chain#{ forks => Forks#{ ForkId => F#{blocks => [NewBlock | Blocks]} } },
    announce(ForkId, [], [], NewChain, Opts),
    {{ok, Block},  NewChain}.

send_tx_events(Events, Hash, Origin) ->
    ?LOG("send_tx_events(~p, ~p ~p)", [Events, Hash, Origin]),
    [aec_events:publish({tx_event, Event}, Info#{ block_hash => Hash
                                                , block_origin => Origin})
     || {Event, Info} <- Events].

with_trees(ForkId, Txs, Block, Chain) ->
    Blocks = blocks(ForkId, Chain),
    Trees = case trees(Blocks) of
                {ok, Ts} ->
                    Ts;
                error ->
                    aec_trees:new_without_backend()
            end,
    {NewTrees, Evs} = update_trees(Txs, Trees, aec_blocks:height(Block)),
    {#{ block => aec_blocks:set_root_hash(Block, aec_trees:hash(NewTrees)), trees => NewTrees }, Evs}.

trees(Blocks) ->
    case lists:dropwhile(
           fun(BlockEntry) ->
                   not maps:is_key(trees, BlockEntry)
           end, Blocks) of
        [#{ trees := Trees }|_] ->
            {ok, Trees};
        [] ->
            error
    end.

update_trees(Txs, Trees, Height) ->
    Env = aetx_env:tx_env(Height),
    {ok, _, [], NewTrees, Evs} = aec_trees:apply_txs_on_state_trees_strict(Txs, Trees, Env),
    {NewTrees, Evs}.

blocks_until_hash(Hash, Blocks) ->
    lists:dropwhile(
      fun(#{block := B}) ->
              {ok, H} = aec_headers:hash_header(aec_blocks:to_header(B)),
              H =/= Hash
      end, Blocks).

top_block_hash_(Chain) ->
    [#{block := B}|_] = blocks(main, Chain),
    {ok, H} = aec_headers:hash_header(aec_blocks:to_header(B)),
    H.

get_block_state_(Hash, Chain) ->
    trees(blocks_until_hash(Hash, blocks(Chain))).

get_top_state_(Chain) ->
    trees(blocks(main, Chain)).

hash_is_in_main_chain_(Hash, TopHash, Chain) ->
    BlockHash = fun(B) ->
                        {ok, H} = aec_headers:hash_header(aec_blocks:to_header(B)),
                        H
                end,
    case blocks_until_hash(TopHash, blocks(Chain)) of
        [] ->
            false;
        Blocks1 ->
            case lists_funfind(Hash, BlockHash, Blocks1) of
                false ->
                    false;
                _B ->
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
                [#{block := B}|_] ->
                    Prev = aec_headers:prev_hash(aec_blocks:to_header(B)),
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

find_common_([Block|T1], Blocks2) ->
    BlockHash = fun(#{block := B}) ->
                {ok, H} = aec_headers:hash_header(aec_blocks:to_header(B)),
                H
            end,
    H1 = BlockHash(Block),
    case lists_funfind(H1, BlockHash, Blocks2) of
        false ->
            find_common_(T1, Blocks2);
        _Found ->
            {ok, H1}
    end;
find_common_([], _) ->
    {error, not_found}.

get_header_(Hash, Chain) ->
    case blocks_until_hash(Hash, blocks(Chain)) of
        [#{block := B}|_] ->
            {ok, aec_blocks:to_header(B)};
        [] ->
            error
    end.

get_block_(Hash, Chain) ->
    case blocks_until_hash(Hash, blocks(Chain)) of
        [Block|_] ->
            {ok, Block};
        [] ->
            error
    end.

get_channel_(ChId, Chain) ->
    case trees(blocks(Chain)) of
        {ok, Trees} ->
            aec_chain:get_channel(ChId, Trees);
        error ->
            undefined
    end.

find_block_tx_hashes_(Hash, Chain) ->
    case blocks_until_hash(Hash, blocks(Chain)) of
        [#{block := Block}|_] ->
            Txs = case aec_blocks:is_key_block(Block) of
                      true -> [];
                      false -> aec_blocks:txs(Block)
                  end,
            TxHashes = [ aetx_sign:hash(Tx) || Tx <- Txs ],
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

find_signed_tx_in_blocks_(
  [#{block := Block}|T], Hash) ->
    case aec_blocks:is_key_block(Block) of
        true ->
            find_signed_tx_in_blocks_(T, Hash);
        false -> case lists_funfind(Hash, fun aetx_sign:hash/1, aec_blocks:txs(Block)) of
                     false ->
                         find_signed_tx_in_blocks_(T, Hash);
                     STx ->
                         {value, STx, Block}
                 end
    end;
find_signed_tx_in_blocks_([], _) ->
    none.

find_tx_with_location_(Hash, #{forks := Forks, mempool := Pool}) ->
    %% When aec_chain_state processes a fork switch, it changes tx_location from
    %% the evicted blocks to the mempool. So this function should only return a
    %% block has if the tx is found on the main chain.
    #{blocks := Blocks} = maps:get(main, Forks),
    case find_signed_tx_in_blocks_(Blocks, Hash) of
        {value, STx, B} ->
            {ok, BlockHash} = aec_headers:hash_header(aec_blocks:to_header(B)),
            {BlockHash, STx};
        none ->
            case lists_funfind(Hash, fun aetx_sign:hash/1, Pool) of
                false ->
                    none;
                STx ->
                    {mempool, STx}
            end
    end.

insert_key_pair( #{pubkey := PK, privkey := SK}
               , #{key_pairs := KPs} = Chain) ->
    Chain#{key_pairs => KPs#{PK => SK}}.

blocks(Chain) ->
    blocks(main, Chain).

blocks(ForkId, #{forks := Forks}) ->
    #{blocks := Blocks} = maps:get(ForkId, Forks),
    Blocks.

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
lists_funfind(Val, Fun, [H|T]) ->
    case Val =:= Fun(H) of
        true ->
            H;
        _ ->
            lists_funfind(Val, Fun, T)
    end;
lists_funfind(_, _, []) ->
    false.

new_keypair() ->
    #{public := PK, secret := SK} = enacl:sign_keypair(),
    #{pubkey => PK, privkey => SK}.

