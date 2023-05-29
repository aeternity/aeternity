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
%%% - Dictionary: for storing and retrieving (mainly) test-related state
%%% - Event reporting: the simulator uses the standard aec_events reporting
%%%   mechanism.
%%%
%%% TODO: Lots, if the code is meant to be used for anything other than the chain
%%% watcher suite. Add mocks, checks and update functions as needed for
%%% incremental improvement.
%%%
%%% ======================================================================

-export([ start/1                      %% (Opts :: map()) -> {ok, pid()}
        , stop/1]).                    %% (Name :: atom()) -> ok

-export([ next_nonce/2                 %% (Name, Acct) -> integer()
        , new_account/2                %% (Name, Balance) -> KeyPair
        , new_account/3                %% (Name, ForkId, Balance) -> KeyPair
        , add_existing_account/3       %% (Name, KeyPair, Balance) -> ok
        , add_existing_account/4       %% (Name, ForkId, KeyPair, Balance) -> ok
        , get_balance/2                %% (Name, Acct) -> Balance
        , get_balance/3                %% (Name, ForkId, Acct) -> Balance
        , get_next_nonce/2             %% (Name, Acct) -> integer()
        , get_next_nonce/3             %% (Name, ForkId, Acct) -> integer()
        , get_height/1                 %% (Name) -> Int
        , get_height/2                 %% (Name, ForkId) -> Int
        , push/2                       %% (Name, Tx) -> ok
        , find_signed_tx/2             %% (Name, TxHash) -> {value, STx)} | none
        , top_block_hash/1             %% (Name) -> Hash
        , top_key_block_hash/1         %% (Name) -> Hash
        , top_key_block/1              %% (Name) -> Block
        , block_by_hash/2              %% (Name, BlockHash) -> {ok, Block}
        , block_by_height/2            %% (Name, BlockHeight) -> {ok, Block}
        , get_key_block_hash_at_height/2
        , get_current_generation/1     %% (Name) -> {ok, Generation}
        , get_generation_by_hash/3     %% (Name, BlockHash, Direction) -> {ok, Generation}
        , sign_and_push/3              %% (Name, Acct, Tx) -> ok
        , get_call/3                   %% (Name, ContractKey, CallKey) -> Call
        , get_call/4                   %% (Name, ForkId, ContractKey, CallKey) -> Call
        , get_trees/1                  %% (Name) -> Trees
        , get_trees/2                  %% (Name, ForkId) -> {ok, Trees} | error
        , add_keyblock/1               %% (Name) -> {ok, Block}
        , add_keyblock/2               %% (Name, ForkId) -> {ok, Block}
        , add_microblock/1             %% (Name) -> {ok, Block}
        , add_microblock/2             %% (Name, ForkId) -> {ok, Block}
        , clone_microblock_on_fork/3   %% (Name, BlockHash, ForkId) -> {ok, Block}
        , fork_from_hash/3             %% (Name, ForkId, FromHash) -> {ok, Block}
        , fork_switch/2                %% (Name, ForkId) -> ok
        , dict_set/3                   %% (Name, Key, Value) -> ok
        , dict_get/3 ]).               %% (Name, Key, Default) -> Value

-export([ setup_meck/1                 %% (Name :: atom()) -> ok
        , remove_meck/0                %% () -> ok
        , get_chain_process/1          %% (Name :: atom()) -> pid() | undefined
        ]).

%% Gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

%% Parametrization
-export([ sim_type_param/1
        , genesis_state_param/1
        ]).


-record(st, { opts = #{}
            , mref
            , dict = #{}
            , chain }).

-type sim_type() :: default | parent_chain.

-type fork_id()    :: main | term().
-type block_hash() :: aec_blocks:block_header_hash().
-type generation() :: #{ key_block => aec_blocks:key_block(),
                         micro_blocks := [aec_blocks:micro_block()],
                         dir := backward | forward }.


%% TODO: Use CT logging or system logging - possibly configurable
-define(LOG(Fmt, Args), io:format("~w:~w/~w -\n" ++ Fmt ++ "\n\n", [?MODULE, ?FUNCTION_NAME, ?LINE | Args])).


-spec start(map()) -> {ok, pid()}.
%%
%% Starts the simulator
%% Supported options:
%% - monitor => pid(): The simulator will terminate if the monitored process dies
%% - sim_type => sim_type(): The simulator mode (parent chain for Hyperchains or default)
%% - name => gproc name for the simulator
%%
start(#{name := _} = Opts) when is_map(Opts) ->
    gen_server:start(?MODULE, Opts, []).

-spec stop(Name :: atom()) -> ok.
%%
%% Stops the simulator
%%
stop(Name) ->
    case get_chain_process(Name) of
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

%%%===================================================================
%%%  Simulator parametrization
%%%===================================================================
%% Simulator type. Ability to isntantiate sim process as a parent chain based model;
-spec sim_type_param(map()) -> sim_type().
sim_type_param(Opts) ->
    maps:get(sim_type, Opts, default).

%% Genesis header. Ability to instantiate parent chain by predefined genesis;
-spec genesis_state_param(map()) -> {aec_blocks:key_block(), aec_trees:trees()}.
genesis_state_param(Opts) ->
    {_Block, _Trees} = maps:get(genesis_state, Opts, aec_block_genesis:genesis_block_with_state()).

%%%===================================================================
%%%  Simulator access
%%%===================================================================
%% Chain simulator requests

-spec next_nonce(Name :: atom(), Acct :: aec_keys:pubkey()) -> integer().
%%
%% Increments the nonce of Acct
%%
next_nonce(Name, Acct) ->
    chain_req(Name, {next_nonce, Acct}).

-spec new_account(Name :: atom(), Balance :: non_neg_integer()) -> {ok, map()}.
%%
%% Equivalent to new_account(main, Balance)
%%
new_account(Name, Balance) ->
    new_account(Name, main, Balance).

-spec new_account(Name :: atom(), fork_id(), Balance :: non_neg_integer()) -> {ok, map()}.
%%
%% Creates new account with given balance
%%
new_account(Name, ForkId, Balance) ->
    chain_req(Name, {new_account, ForkId, Balance}).

-spec add_existing_account(Name :: atom(), PK :: map(), Balance :: non_neg_integer()) -> {ok, map()}.
%%
%% Adds an account with a known private . public key pair
%%
add_existing_account(Name, Acct, Balance) ->
    add_existing_account(Name, main, Acct, Balance).

-spec add_existing_account(Name :: atom(), fork_id(), Acct :: map(), Balance :: non_neg_integer()) -> {ok, map()}.
%%
%% Creates new account with given balance
%%
add_existing_account(Name, ForkId, Acct, Balance) ->
    chain_req(Name, {add_existing_account, ForkId, Acct, Balance}).

-spec get_balance(Name :: atom(), Acct :: non_neg_integer()) -> integer().
%%
%% Equivalent to get_balance(main, Acct)
%%
get_balance(Name, Acct) ->
    get_balance(Name, main, Acct).

-spec get_balance(Name :: atom(), fork_id(), Acct :: non_neg_integer()) -> integer().
%%
%% Queries the chain for account's balance
%%
get_balance(Name, ForkId, Acct) ->
    chain_req(Name, {get_balance, ForkId, Acct}).

get_next_nonce(Name, Acct) ->
    get_next_nonce(Name, main, Acct).

get_next_nonce(Name, ForkId, Acct) ->
    chain_req(Name, {get_next_nonce, ForkId, Acct}).

-spec get_height(Name :: atom()) -> integer().
%%
%% Equivalent to get_height(main)
%%
get_height(Name) ->
    get_height(Name, main).

-spec get_height(Name :: atom(), fork_id()) -> integer().
%%
%% Queries the chain for account's balance
%%
get_height(Name, ForkId) ->
    chain_req(Name, {get_height, ForkId}).

-spec push(Name :: atom(), aetx_sign:signed_tx()) -> ok.
%%
%% Pushes the signed tx to the simulated mempool
%%
push(Name, Tx) ->
    chain_req(Name, {push, Tx}).

-spec sign_and_push(Name :: atom(), aec_keys:pubkey(), aetx:tx()) -> ok | {error, unknown_privkey}.
%%
%% Signs and pushes tx to the simulated mempool
%%
sign_and_push(Name, Account, Tx) ->
    chain_req(Name, {sign_and_push, Account, Tx}).

-spec get_call(Name :: atom(), Contract :: aect_contracts:id(), Call :: aect_call:id()) -> aect_call:call().
%%
%% Equivalent to get_call(main, Contract, Call)
%%
get_call(Name, Contract, Call) ->
    get_call(Name, main, Contract, Call).

-spec get_call(Name :: atom(), ForkId :: fork_id(), Contract :: aect_contracts:id(), Call :: aect_call:id()) -> aect_call:call().
%%
%% Equivalent to get_call(main, Contract, Call)
%%
get_call(Name, ForkId, Contract, Call) ->
    chain_req(Name, {get_call, ForkId, Contract, Call}).

-spec get_trees(Name :: atom()) -> {ok, aec_trees:trees()} | error.
%%
%% Equivalent to get_trees(main)
%%
get_trees(Name) ->
    get_trees(Name, main).

-spec get_trees(Name :: atom(), fork_id()) -> {ok, aec_trees:trees()} | error.
%%
%% Returns state trees from the top of a given fork
%%
get_trees(Name, ForkId) ->
    chain_req(Name, {get_trees, ForkId}).

-spec add_keyblock(Name :: atom()) -> aec_blocks:key_block().
%%
%% Equivalent to add_keyblock(main)
%%
add_keyblock(Name) ->
    add_keyblock(Name, main).

-spec block_by_hash(Name :: atom(), block_hash()) -> {ok, aec_blocks:block()}.
block_by_hash(Name, BlockHash) ->
    chain_req(Name, {block_by_hash, BlockHash}).

-spec block_by_height(Name :: atom(), non_neg_integer()) -> {ok, aec_blocks:block()}.
block_by_height(Name, BlockHeight) ->
    chain_req(Name, {block_by_height, BlockHeight}).

-spec top_block_hash(Name :: atom()) -> binary().
top_block_hash(Name) ->
    chain_req(Name, top_block_hash).

-spec top_key_block_hash(Name :: atom()) -> binary().
top_key_block_hash(Name) ->
    chain_req(Name, top_key_block_hash).

-spec top_key_block(Name :: atom()) -> binary().
top_key_block(Name) ->
    chain_req(Name, top_key_block).

-spec get_key_block_hash_at_height(Name :: atom(), Height :: integer()) -> binary().
get_key_block_hash_at_height(Name, Height) ->
    chain_req(Name, {get_key_block_hash_at_height, Height}).

-spec get_current_generation(Name :: atom()) -> error | {'ok', generation()}.
get_current_generation(Name) ->
    chain_req(Name, get_current_generation).

-spec get_generation_by_hash(Name :: atom(), block_hash(), forward | backward) -> error | {'ok', generation()}.
get_generation_by_hash(Name, Hash, Dir) ->
    chain_req(Name, {get_generation_by_hash, Hash, Dir}).

-spec add_keyblock(Name :: atom(), ForkId :: fork_id() ) -> {ok, aec_blocks:key_block()}.
%%
%% Adds a keyblock. If ForkId == main, the keyblock is added to the main fork.
%% If ForkId is the id of an existing fork, created previously with
%% fork_from_hash/2, the keyblock is added to that fork. If the given ForkId
%% is not known, the simulator terminates.
%%
add_keyblock(Name, ForkId) ->
    chain_req(Name, {add_key, ForkId}).

-spec add_microblock(Name :: atom()) -> {ok, aec_blocks:micro_block()}.
%%
%% Equivalent to add_microblock(main)
%%
add_microblock(Name) ->
    add_microblock(Name, main).

-spec add_microblock(Name :: atom(), ForkId :: fork_id()) -> {ok, aec_blocks:micro_block()}.
%%
%% Adds a microblock to the given ForkId (the main fork if ForkId == main).
%% All transactions in the mempool are added to the block.
%%
add_microblock(Name, ForkId) ->
    chain_req(Name, {add_micro, ForkId}).

-spec clone_microblock_on_fork(Name :: atom(), block_hash(), fork_id()) -> {ok, aec_blocks:micro_block()}.
%%
%% This is a cheating way of simulating transactions being evicted from the main chain and
%% picked up in a new microblock. The cheat is that the original block remains on the chain.
%% This function should be used with care, and ideally replaced by a better solution.
%%
clone_microblock_on_fork(Name, Hash, ForkId) ->
    chain_req(Name, {clone_micro_on_fork, Hash, ForkId}).

-spec fork_from_hash(Name :: atom(), ForkId :: fork_id(), FromHash :: block_hash()) -> {ok, aec_blocks:key_block()}.
%%
%% Creates a new ForkId and adds a keyblock to FromHash
%% Fails (simulator terminates) if ForkId exists, or if FromHash is not a block
%% on the main fork.
%%
fork_from_hash(Name, ForkId, FromHash) when is_binary(FromHash) ->
    chain_req(Name, {fork_from_hash, ForkId, FromHash}).

-spec fork_switch(Name :: atom(), ForkId :: fork_id()) -> ok.
%%
%% Makes the existing fork ForkId the new main fork, evicting all blocks
%% succeeding the common ancestor. The evicted transactions are returned to the
%% simulated mempool.
%%
fork_switch(Name, ForkId) ->
    chain_req(Name, {fork_switch, ForkId}).

-spec dict_set(Name :: atom(), Key :: term(), Value :: term()) -> ok.
%%
%% Stores `Key => Value' in the "user dictionary" managed by the chain simulator.
%%
dict_set(Name, Key, Value) ->
    chain_req(Name, {dict_set, Key, Value}).

-spec dict_get(Name :: atom(), Key :: term(), Default :: term()) -> term().
%%
%% Returns the value associated with Key in the user dictionary, or Default if
%% the key is not found.
%%
dict_get(Name, Key, Default) ->
    chain_req(Name, {dict_get, Key, Default}).

-spec find_signed_tx(Name :: atom(), binary()) -> {value, binary()} | none.
%%
%% Returns the transaction hash associated with Key
%%
find_signed_tx(Name, TxHash) ->
    chain_req(Name, {find_signed_tx, TxHash}).

-spec setup_meck(Name :: atom()) -> ok.
%%
%% Installs the mocks needed for the simulator. The assumption is that the mocks
%% are not already installed.
%%
setup_meck(Name) ->
    aec_db:install_test_env(),
    meck:expect(aec_chain_state, hash_is_in_main_chain, 2,
                fun(Hash, TopHash) ->
                        chain_req(Name, {hash_is_in_main_chain, Hash, TopHash})
                end),
    meck:expect(aec_chain_state, find_common_ancestor, 2,
                fun(Hash1, Hash2) ->
                        chain_req(Name, {find_common_ancestor, Hash1, Hash2})
                end),
    meck:expect(aec_chain, get_top_state, 0,
                fun() ->
                        chain_req(Name, get_top_state)
                end),
    meck:expect(aec_chain, find_tx_with_location, 1,
                fun(Hash) ->
                        chain_req(Name, {find_tx_with_location, Hash})
                end),
    meck:expect(aec_chain, get_block_state, 1,
                fun(Hash) ->
                        chain_req(Name, {get_block_state, Hash})
                end),
    meck:expect(aec_chain, get_channel, 1,
                fun(Id) ->
                        chain_req(Name, {get_channel, Id})
                end),
    meck:expect(aec_chain, top_block_hash, 0,
                fun() ->
                        chain_req(Name, top_block_hash)
                end),
    meck:expect(aec_chain, top_key_block_hash, 0,
                fun() ->
                        chain_req(Name, top_key_block_hash)
                end),
    meck:expect(aec_chain, top_key_block, 0,
                fun() ->
                        chain_req(Name, top_key_block)
                end),
    meck:expect(aec_chain, get_header, 1,
                fun(BHash) ->
                        chain_req(Name, {get_header, BHash})
                end),
    meck:expect(aec_db, find_block_tx_hashes, 1,
                fun(Hash) ->
                        chain_req(Name, {find_block_tx_hashes, Hash})
                end),
    meck:expect(aec_db, find_signed_tx, 1,
                fun(TxHash) ->
                        chain_req(Name, {find_signed_tx, TxHash})
                end),
    ok.

-spec remove_meck() -> ok.
%%
%% Removes the mocks installed by the simulator.
%%
remove_meck() ->
    aec_db:uninstall_test_env(),
    meck:unload([ aec_chain
                , aec_chain_state
                , aec_db]),
    ok.

%%%===================================================================
%$% gen_server implementation
%%%===================================================================

init(#{name := Name} = Opts) when is_map(Opts) ->
    gproc:reg({n,l,{Name, chain_process}}),
    Chain = new_chain(Opts),
    ?LOG("Initial chain (~p sim_type_param): ~p", [sim_type_param(Opts), Chain]),
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
handle_call({add_existing_account, ForkId, Acct, Balance}, _From, #st{chain = Chain} = St) ->
    {Res, Chain1} = add_account_keypair_(ForkId, Acct, Balance, Chain),
    {reply, Res, St#st{chain = Chain1}};
handle_call({add_key, ForkId}, _From, #st{opts = Opts, chain = Chain} = St) ->
    {Res, Chain1} = add_keyblock_(ForkId, Chain, Opts),
    {reply, Res, St#st{chain = Chain1}};
handle_call({get_balance, ForkId, Acct}, _From, #st{chain = Chain} = St) ->
    Res = get_balance_(ForkId, Acct, Chain),
    {reply, Res, St};
handle_call({get_next_nonce, ForkId, Acct}, _From, #st{chain = Chain} = St) ->
    Res = get_next_nonce_(ForkId, Acct, Chain),
    {reply, Res, St};
handle_call({get_height, ForkId}, _From, #st{chain = Chain} = St) ->
    Res = get_height_(ForkId, Chain),
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
handle_call({get_call, ForkId, ContractId, CallId}, _From, #st{chain = Chain} = St) ->
    {reply, get_call_(ForkId, ContractId, CallId, Chain), St};
handle_call({get_trees, ForkId}, _From, #st{chain = Chain} = St) ->
    #{forks := #{ ForkId := #{blocks := Blocks}}} = Chain,
    {reply, trees(Blocks), St};
handle_call({next_nonce, Acct}, _From, #st{chain = #{nonces := Nonces} = Chain} = St) ->
    N = maps:get(Acct, Nonces, 0),
    NewN = N + 1,
    {reply, NewN, St#st{chain = Chain#{nonces => Nonces#{ Acct => NewN }}}};
handle_call(top_block_hash, _From, #st{chain = Chain} = St) ->
    {reply, top_block_hash_(Chain), St};
handle_call(top_key_block_hash, _From, #st{chain = Chain} = St) ->
    {reply, top_key_block_hash_(Chain), St};
handle_call(top_key_block, _From, #st{chain = Chain} = St) ->
    {reply, top_key_block_(Chain), St};
handle_call({block_by_hash, Hash},_From, #st{chain = Chain} = St) ->
    {reply, get_block_(Hash, Chain), St};
handle_call({block_by_height, Height},_From, #st{chain = Chain} = St) ->
    {reply, get_block__by_height_(Height, Chain), St};
handle_call(get_current_generation,  _From, #st{chain = Chain} = St) ->
    {reply, get_current_generation_(Chain), St};
handle_call({get_generation_by_hash, Hash, Dir},  _From, #st{chain = Chain} = St) ->
    {reply, get_generation_by_hash_(Hash, Dir, Chain), St};
handle_call({get_key_block_hash_at_height, Height}, _From, #st{chain = Chain} = St) ->
    {reply, get_key_block_hash_at_height_(Height, Chain), St};
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

new_account_(ForkId, Balance, Chain) ->
    #{pubkey := _PK} = KP = new_keypair(),
    add_account_keypair_(ForkId, KP, Balance, Chain).

add_account_keypair_(ForkId, #{pubkey := PK} = KP, Balance, #{forks := Forks} = Chain) ->
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

get_next_nonce_(ForkId, PK, Chain) ->
    case trees(blocks(ForkId, Chain)) of
        {ok, Ts} ->
            case aec_accounts_trees:lookup(PK, aec_trees:accounts(Ts)) of
                {value, Account} ->
                     {ok, aec_accounts:nonce(Account) + 1};
                none ->
                    {error, account_not_found}
            end;
        error ->
            {error, no_state_trees}
    end.

get_height_(ForkId, #{forks := Forks}) ->
    #{ ForkId := #{ blocks := Blocks} } = Forks,
    length([{} || #{block := B} <- Blocks, aec_blocks:is_key_block(B)]).

add_microblock_(ForkId, #{mempool := Pool} = Chain, Opts) ->
    Txs = lists:reverse(Pool),  % because LIFO
    add_microblock_(ForkId, Txs, Chain#{mempool => []}, Opts).

add_microblock_(ForkId, Txs, #{forks := Forks} = Chain, Opts) ->
    ?LOG("add_microblock(Txs = ~p", [[aetx_sign:hash(Tx) || Tx <- Txs]]),
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
    ?LOG("Microblock = ~p", [aec_headers:hash_header(aec_blocks:to_header(Block))]),
    NewFork = F#{blocks => [BlockEntry | Blocks]},
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
    ?LOG("Evicting txs: ~p", [[aetx_sign:hash(Tx) || Tx <- ReturnTxs]]),
    NewPool = lists:reverse(ReturnTxs) ++ Pool,
    NewForks = maps:remove(ForkId, Forks#{main => M#{blocks => FBlocks}}),
    NewChain = Chain#{ forks => NewForks
                     , mempool => NewPool
                     , orphans => Evict ++ Orphans },
    {ok, announce(main, [], [], NewChain, Opts)}.

%% Announce top_changed and tx events
announce(ForkId, Txs, Events, #{ forks := Forks } = Chain, Opts) ->
    #{ ForkId := #{ blocks := [#{ block := TopBlock } | _] = Blocks} } = Forks,
    SimulatorT = sim_type_param(Opts),
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
        %% TODO: To supply parameterized formatter;
        ParentBlock = Info#{txs => Txs, header => Hdr, hash => TopHash},
        aec_events:publish({parent_chain, top_changed}, ParentBlock);
        true ->
            send_tx_events(Events, TopHash, Info),
            (ForkId == main) andalso
                begin
                    ?LOG("Publishing top_changed, I = ~p", [Info]),
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
new_chain(Opt) ->
    {Gen, Tre} = genesis_state_param(Opt),
    Miner = deterministic_keypair(),
    {ok, Hash} = aec_headers:hash_header(aec_blocks:to_header(Gen)),
    Chain0 =
        #{ miner     => Miner
        ,  mempool   => []
        ,  orphans   => []
        ,  nonces    => #{}
        ,  key_pairs => #{}
        ,  forks     => #{ main => #{ fork_point => Hash
                                   ,  blocks     => [#{block => Gen, trees => Tre}] } }},
    Chain0.

%% Called from the chain process
add_keyblock_(ForkId, #{forks := Forks, miner := #{pubkey := Miner}} = Chain, Opts) ->
    ?LOG("add_keyblock(~p)", [ForkId]),
    #{ ForkId := #{blocks := Blocks} = F } = Forks,
    #{block := Block} = hd(Blocks),
    TopHdr = aec_blocks:to_header(Block),
    {ok, PrevHash} = aec_headers:hash_header(TopHdr),
    PrevKeyHash =
    case aec_headers:type(TopHdr) of
        key -> PrevHash;
        micro -> aec_headers:prev_key_hash(TopHdr)
    end,
    Height = aec_headers:height(TopHdr),
    NewHdr = aec_headers:new_key_header(
               Height+1, PrevHash, PrevKeyHash, root_hash(),
               Miner, Miner, 0, 0, 0, 0, default, 0),
    NewBlock = aec_blocks:new_key_from_header(NewHdr),
    NewChain = Chain#{ forks => Forks#{ ForkId => F#{blocks => [#{block => NewBlock} | Blocks]} } },
    announce(ForkId, [], [], NewChain, Opts),
    {{ok, NewBlock},  NewChain}.

send_tx_events(Events, Hash, Origin) ->
    ?LOG("send_tx_events(~p, ~p)", [Events, Hash]),
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
    Env0 = aetx_env:tx_env(Height),
    Env1 = aetx_env:set_dry_run(Env0, true),  % To expose error messages
    {ok, _, [], NewTrees, Evs} = aec_trees:apply_txs_on_state_trees_strict(Txs, Trees, Env1),
    {NewTrees, Evs}.

get_call_(ForkId, ContractId, CallId, #{forks := Forks}) ->
    #{ ForkId := #{blocks := Blocks}} = Forks,
    {ok, Trees} = trees(Blocks),
    Calls = aec_trees:calls(Trees),
    aect_call_state_tree:get_call(ContractId, CallId, Calls).

blocks_until_hash(Hash, Blocks) ->
    lists:dropwhile(
      fun(#{block := B}) ->
              {ok, H} = aec_headers:hash_header(aec_blocks:to_header(B)),
              H =/= Hash
      end, Blocks).

blocks_until_key(Blocks) ->
    lists:dropwhile(
      fun(#{block := B}) ->
              Type = aec_headers:type(aec_blocks:to_header(B)),
              Type =/= key
      end, Blocks).

blocks_until_height(Height, Blocks) ->
    lists:dropwhile(
      fun(#{block := B}) ->
              H = aec_blocks:height(B),
              H =/= Height
      end, Blocks).

blocks_at_height(Height, Blocks) ->
    SkippedHigher = lists:dropwhile(
      fun(#{block := B}) ->
              Height =/= aec_headers:height(aec_blocks:to_header(B))
      end, Blocks),
    lists:takewhile(
        fun(#{block := B}) ->
              Height == aec_headers:height(aec_blocks:to_header(B))
        end, SkippedHigher).

top_block_hash_(Chain) ->
    [#{block := B}|_] = blocks(main, Chain),
    {ok, H} = aec_headers:hash_header(aec_blocks:to_header(B)),
    H.

top_key_block_hash_(Chain) ->
    [#{block := B}|_] = blocks_until_key(blocks(main, Chain)),
    {ok, H} = aec_headers:hash_header(aec_blocks:to_header(B)),
    H.

top_key_block_(Chain) ->
    [#{block := B}|_] = blocks_until_key(blocks(main, Chain)),
    B.


top_block_node(Chain) ->
    [#{block := B}|_] = blocks_until_key(blocks(main, Chain)),
    Header = aec_blocks:to_header(B),
    {ok, Hash} = aec_headers:hash_header(Header),
    #{hash => Hash, header => Header}.

%%%===================================================================
%%% Generations
%%%===================================================================

-spec get_current_generation_(Chain :: term()) -> 'error' | {'ok', generation()}.
get_current_generation_(Chain) ->
    get_generation_(top_block_node(Chain), Chain).

get_generation_(#{hash := Hash, header := Header}, Chain) ->
    case aec_headers:type(Header) of
        key ->
            {ok, #{ key_block => aec_blocks:new_key_from_header(Header), micro_blocks => [], dir => forward }};
        micro ->
            Index = find_headers_and_hash_at_height_(aec_headers:height(Header), Chain),
            get_generation_with_header_index(Hash, Index, Chain)
    end;
get_generation_(Hash, Chain) when is_binary(Hash) ->
    case get_header_(Hash, Chain) of
        error -> error;
        {ok, Header} ->
            get_generation_(#{hash => Hash, header => Header}, Chain)
    end;
get_generation_(_,_) ->
    error.

get_generation_with_header_index(TopHash, Index, Chain) ->
    get_generation_with_header_index(TopHash, Index, [], Chain).

get_generation_with_header_index(TopHash, Index, MBs, Chain) ->
    H = maps:get(TopHash, Index),
    case aec_headers:type(H) of
        key -> {ok, #{ key_block => aec_blocks:new_key_from_header(H), micro_blocks => MBs, dir => forward }};
        micro ->
            Block = get_block_from_micro_header(TopHash, H, Chain),
            get_generation_with_header_index(aec_headers:prev_hash(H), Index, [Block | MBs], Chain)
    end.

-spec get_generation_by_hash_(binary(), forward | backward, map()) ->
        'error' | {'ok', generation()}.
get_generation_by_hash_(KeyBlockHash, Dir, Chain) ->
    [#{block := KeyBlock}|_] = blocks_until_hash(KeyBlockHash, blocks(Chain)),
    Hdr = aec_blocks:to_header(KeyBlock),
    case aec_headers:type(Hdr) of
        micro -> error;
        key when Dir == backward ->
            case get_generation_(aec_blocks:prev_hash(KeyBlock), Chain) of
                error         -> error;
                {ok, G = #{}} -> {ok, G#{ key_block => KeyBlock, dir => Dir }}
            end;
        key when Dir == forward ->
            case get_generation_by_height(aec_blocks:height(KeyBlock), forward, Chain) of
                {ok, G = #{ key_block := KeyBlock }} -> {ok, G};
                {ok, _G}                             -> error; %% KeyBlockHash not on chain!!
                error                                -> error
            end
    end.

-spec get_generation_by_height(aec_blocks:height(), forward | backward, map()) ->
        'error' | {'ok', generation()}.
get_generation_by_height(Height, backward, Chain) ->
    case get_key_block_by_height_(Height, Chain) of
        {error, _Reason} -> error;
        {ok, KeyBlock} ->
            case get_generation_(aec_blocks:prev_hash(KeyBlock), Chain) of
                error         -> error;
                {ok, G = #{}} -> {ok, G#{ key_block => KeyBlock, dir => backward }}
            end
    end;
get_generation_by_height(Height, forward, Chain) ->
    #{header := TopHeader} = TopNode = top_block_node(Chain),
    TopHeight = aec_headers:height(TopHeader),
    if TopHeight < Height  -> error;
       TopHeight == Height -> get_generation_(TopNode, Chain);
       true                ->
           case get_key_block_by_height_(Height + 1, Chain) of
               {error, _Reason} -> error;
               {ok, KeyBlock}   -> get_generation_(aec_blocks:prev_hash(KeyBlock), Chain)
           end
    end.

get_block_from_micro_header(Hash, MicroHeader, Chain) ->
    aec_headers:assert_micro_header(MicroHeader),
    case find_block_txs_(Hash, Chain) of
        {value, Txs} ->
            aec_blocks:new_micro_from_header(MicroHeader, Txs, no_fraud);
        not_found ->
            aec_blocks:new_micro_from_header(MicroHeader, [], no_fraud)
    end.

%% Only return nodes in the main chain. This reflects the behaviour of the same
%% function in aec_chain_state.erl
get_key_block_by_height_(Height, Chain) ->
    [_|_] = BlocksAtHeight = blocks_until_height(Height, blocks(main, Chain)),
    [#{block := KeyBlock}|_] = blocks_until_key(BlocksAtHeight),
    {ok, KeyBlock}.

get_key_block_hash_at_height_(Height, Chain) ->
    [_|_] = BlocksAtHeight = blocks_until_height(Height, blocks(main, Chain)),
    [#{block := KeyBlock}|_] = blocks_until_key(BlocksAtHeight),
    Header = aec_blocks:to_header(KeyBlock),
    aec_headers:hash_header(Header).

%% Search all forks at a height
find_headers_and_hash_at_height_(Height, #{forks := Forks}) ->
    maps:fold(fun(_ForkId, #{blocks := Blocks}, Acc) ->
                case blocks_at_height(Height, Blocks) of
                    [#{block := _Block}|_] = Bs ->
                        lists:foldl(fun(#{block := Block}, Acc1) ->
                                        Header = aec_blocks:to_header(Block),
                                        {ok, Hash} = aec_headers:hash_header(Header),
                                        maps:put(Hash, Header, Acc1)
                                    end, Acc, Bs);
                    _ ->
                        Acc
                end
              end, #{}, Forks).


get_block_state_(Hash, Chain) ->
    trees(blocks_until_hash(Hash, blocks(Chain))).

get_top_state_(Chain) ->
    trees(blocks(main, Chain)).

hash_is_in_main_chain_(Hash, TopHash, Chain) ->
    BlockHash = fun(B) ->
                        Block = maps:get(block, B),
                        {ok, H} = aec_headers:hash_header(aec_blocks:to_header(Block)),
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
        [BlockEntry|_] ->
            {ok, maps:get(block, BlockEntry)};
        [] ->
            error
    end.

get_block__by_height_(Height, Chain) ->
    case blocks_until_height(Height, blocks(Chain)) of
        [BlockEntry|_] ->
            {ok, maps:get(block, BlockEntry)};
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

find_block_txs_(Hash, Chain) ->
    case blocks_until_hash(Hash, blocks(Chain)) of
        [#{block := Block}|_] ->
            Txs = case aec_blocks:is_key_block(Block) of
                      true -> [];
                      false -> aec_blocks:txs(Block)
                  end,
            {value, Txs};
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

chain_req(Name, Req) ->
    chain_req_(get_chain_process(Name), Req).

get_chain_process(Name) ->
    gproc:where({n, l, {Name, chain_process}}).

chain_req_(ChainP, Req) when is_pid(ChainP) ->
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

deterministic_keypair() ->
    #{public := PK, secret := SK} = enacl:sign_seed_keypair(<<"asdf">>),
    #{pubkey => PK, privkey => SK}.

new_keypair() ->
    #{public := PK, secret := SK} = enacl:sign_keypair(),
    #{pubkey => PK, privkey => SK}.
