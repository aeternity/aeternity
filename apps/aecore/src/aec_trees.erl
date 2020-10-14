%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Block state Merkle trees.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_trees).

-include("blocks.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

%% API

-export([accounts/1,
         commit_to_db/1,
         hash/1,
         hash/2,
         new/0,
         new_without_backend/0,
         channels/1,
         ns/1,
         oracles/1,
         calls/1,
         contracts/1,
         set_accounts/2,
         set_calls/2,
         set_channels/2,
         set_contracts/2,
         set_ns/2,
         set_oracles/2,
         gc_cache/2,
         sum_total_coin/1
        ]).

-export([ deserialize_from_db/1
        , serialize_for_db/1
        % could get big, used in force progress
        , serialize_to_binary/1
        , serialize_to_client/1
        , deserialize_from_binary_without_backend/1
        ]).

-export([ensure_account/2]).

-export([apply_txs_on_state_trees/3,
         apply_txs_on_state_trees/4,
         apply_txs_on_state_trees_strict/3,
         grant_fee/3,
         perform_pre_transformations/3
        ]).

%% Proof of inclusion
-export([add_poi/4,
         lookup_poi/3,
         deserialize_poi/1,
         new_poi/1,
         poi_hash/1,
         poi_calls_hash/1,
         serialize_poi/1,
         verify_poi/4
        ]).

-export([record_fields/1]).

-ifdef(TEST).
-export([internal_serialize_poi_fields/1,
         internal_serialize_poi_from_fields/1
        ]).
-endif.

-record(trees, {
          accounts  :: aec_accounts_trees:tree(),
          calls     :: aect_call_state_tree:tree(),
          channels  :: aesc_state_tree:tree(),
          contracts :: aect_state_tree:tree(),
          ns        :: aens_state_tree:tree(),
          oracles   :: aeo_state_tree:tree()}).

-record(poi, {
          accounts  :: part_poi(),
          calls     :: part_poi(),
          channels  :: part_poi(),
          contracts :: part_poi(),
          ns        :: part_poi(),
          oracles   :: part_poi()
         }).

-opaque trees() :: #trees{}.
-opaque poi() :: #poi{}.

-type part_poi() :: 'empty' | {'poi', aec_poi:poi()}.
-type tree_type() :: 'accounts'
                   | 'calls'
                   | 'channels'
                   | 'contracts'
                   | 'ns'
                   | 'oracles'.

-export_type([ trees/0
             , poi/0
             ]).

-define(VERSION, 1).

%% ==================================================================
%% Tracing support
record_fields(trees) -> record_info(fields, trees);
record_fields(poi  ) -> record_info(fields, poi);
record_fields(_    ) -> {check_mods, [ aeu_mp_trees
                                     , aec_accounts_trees
                                     , aect_state_tree
                                     , aect_call_state_tree
                                     , aec_state_tree
                                     , aens_state_tree
                                     , aeo_state_tree ]}.
%% ==================================================================

%%%%=============================================================================
%% API
%%%=============================================================================

-spec new() -> trees().
new() ->
    #trees{accounts  = aec_accounts_trees:empty_with_backend(),
           calls     = aect_call_state_tree:empty_with_backend(),
           channels  = aesc_state_tree:empty_with_backend(),
           contracts = aect_state_tree:empty_with_backend(),
           ns        = aens_state_tree:empty_with_backend(),
           oracles   = aeo_state_tree:empty_with_backend()
          }.

-spec new_without_backend() -> trees().
new_without_backend() ->
    #trees{accounts  = aec_accounts_trees:empty(),
           calls     = aect_call_state_tree:empty(),
           channels  = aesc_state_tree:empty(),
           contracts = aect_state_tree:empty(),
           ns        = aens_state_tree:empty(),
           oracles   = aeo_state_tree:empty()
          }.

-spec new_poi(trees()) -> poi().
new_poi(Trees) ->
    internal_new_poi(Trees).

-spec add_poi(tree_type(), aec_keys:pubkey(), trees(), poi()) -> {'ok', poi()}
                                                                     | {'error', term()}.
add_poi(accounts, PubKey, Trees, #poi{} = Poi) ->
    internal_add_accounts_poi(PubKey, accounts(Trees), Poi);
add_poi(contracts, Pubkey, Trees, #poi{} = Poi) ->
    internal_add_contracts_poi(Pubkey, contracts(Trees), Poi);
add_poi(Type,_PubKey,_Trees, #poi{} =_Poi) ->
    error({nyi, Type}).

-spec lookup_poi(tree_type(), aec_keys:pubkey(), poi()) ->
                        {'ok', Object} | {'error', 'not_found'} when
      Object :: aec_accounts:account()
              | aect_contracts:contract().
lookup_poi(accounts, PubKey, #poi{} = Poi) ->
    internal_lookup_accounts_poi(PubKey, Poi);
lookup_poi(contracts, PubKey, #poi{} = Poi) ->
    internal_lookup_contracts_poi(PubKey, Poi).

-spec poi_hash(poi()) -> state_hash().
poi_hash(#poi{} = Poi) ->
    internal_poi_hash(Poi).

-spec poi_calls_hash(poi()) -> binary().
poi_calls_hash(Poi) ->
    part_poi_hash(Poi#poi.calls).

-spec serialize_poi(poi()) -> binary().
serialize_poi(#poi{} = Poi) ->
    internal_serialize_poi(Poi).

-spec deserialize_poi(binary()) -> poi().
deserialize_poi(Bin) when is_binary(Bin) ->
    internal_deserialize_poi(Bin).

-spec verify_poi(tree_type(), aec_keys:pubkey(), Object, poi()) ->
                        'ok' | {'error', term()} when
      Object :: aec_accounts:account()
              | aect_contracts:contract().
verify_poi(accounts, PubKey, Account, #poi{} = Poi) ->
    internal_verify_accounts_poi(PubKey, Account, Poi);
verify_poi(contracts, PubKey, Contract, #poi{} = Poi) ->
    internal_verify_contracts_poi(PubKey, Contract, Poi);
verify_poi(Type,_PubKey,_Account, #poi{} =_Poi) ->
    error({nyi, Type}).

-spec commit_to_db(trees()) -> trees().
commit_to_db(Trees) ->
    %% Make this in a transaction to get atomicity.
    aec_db:ensure_transaction(fun() -> internal_commit_to_db(Trees) end).

hash(Trees) ->
    internal_hash(Trees).

hash(Trees, {calls, CallsHash}) ->
    internal_hash(Trees, {calls_hash, CallsHash}).

-spec accounts(trees()) -> aec_accounts_trees:tree().
accounts(Trees) ->
    Trees#trees.accounts.

-spec set_accounts(trees(), aec_accounts_trees:tree()) -> trees().
set_accounts(Trees, Accounts) ->
    Trees#trees{accounts = Accounts}.

-spec channels(trees()) -> aesc_state_tree:tree().
channels(Trees) ->
    Trees#trees.channels.

-spec set_channels(trees(), aesc_state_tree:tree()) -> trees().
set_channels(Trees, Channels) ->
    Trees#trees{channels = Channels}.

-spec ns(trees()) -> aens_state_tree:tree().
ns(Trees) ->
    Trees#trees.ns.

-spec set_ns(trees(), aens_state_tree:tree()) -> trees().
set_ns(Trees, Names) ->
    Trees#trees{ns = Names}.

-spec oracles(trees()) -> aeo_state_tree:tree().
oracles(Trees) ->
    Trees#trees.oracles.

-spec set_oracles(trees(), aeo_state_tree:tree()) -> trees().
set_oracles(Trees, Oracles) ->
    Trees#trees{oracles = Oracles}.

-spec gc_cache(trees(), [accounts | contracts]) -> trees().
gc_cache(Trees, TreesToGC) ->
    lists:foldl(
        fun(accounts, AccumTrees) ->
            Accounts0 = accounts(AccumTrees),
            Accounts = aec_accounts_trees:gc_cache(Accounts0),
            set_accounts(AccumTrees, Accounts);
           (contracts, AccumTrees) ->
            Contracts0 = contracts(AccumTrees),
            Contracts = aect_state_tree:gc_cache(Contracts0),
            set_contracts(AccumTrees, Contracts)
        end,
        Trees,
        TreesToGC).

-spec perform_pre_transformations(trees(), aetx_env:env(),
                                  aec_hard_forks:protocol_vsn() | undefined) -> trees().
perform_pre_transformations(Trees, TxEnv, PrevProtocol) ->
    Height = aetx_env:height(TxEnv),
    Protocol = aetx_env:consensus_version(TxEnv),
    Trees0 = aect_call_state_tree:prune(Height, Trees),
    Trees1 = aeo_state_tree:prune(Height, Trees0),
    Trees2 = aens_state_tree:prune(Height, Trees1),
    perform_pre_transformations(Trees2, TxEnv, Protocol, PrevProtocol).

perform_pre_transformations(Trees, _TxEnv, _Protocol, undefined) ->
    %% Genesis block.
    Trees;
perform_pre_transformations(Trees, _TxEnv, Protocol, Protocol) ->
    %% No version change in block.
    Trees;
perform_pre_transformations(Trees, _TxEnv, Protocol, _PrevProtocol)
  when Protocol > ?LIMA_PROTOCOL_VSN ->
    %% Trees shouldn't need transformations after Lima.
    Trees;
perform_pre_transformations(Trees, TxEnv, Protocol, PrevProtocol)
  when Protocol > PrevProtocol ->
    %% Fork.
    apply_pre_transformations(Protocol, Trees, TxEnv).

apply_pre_transformations(?MINERVA_PROTOCOL_VSN, Trees, _TxEnv) ->
    aec_block_fork:apply_minerva(Trees);
apply_pre_transformations(?FORTUNA_PROTOCOL_VSN, Trees, _TxEnv) ->
    aec_block_fork:apply_fortuna(Trees);
apply_pre_transformations(?LIMA_PROTOCOL_VSN, Trees, TxEnv) ->
    aec_block_fork:apply_lima(Trees, TxEnv).

-spec calls(trees()) -> aect_call_state_tree:tree().
calls(Trees) ->
    Trees#trees.calls.

-spec set_calls(trees(), aect_call_state_tree:tree()) -> trees().
set_calls(Trees, Calls) ->
    Trees#trees{calls = Calls}.

-spec contracts(trees()) -> aect_state_tree:tree().
contracts(Trees) ->
    Trees#trees.contracts.

-spec set_contracts(trees(), aect_state_tree:tree()) -> trees().
set_contracts(Trees, Contracts) ->
    Trees#trees{contracts = Contracts}.

-spec sum_total_coin(trees()) -> #{ 'accounts'         => non_neg_integer()
                                  , 'contracts'        => non_neg_integer()
                                  , 'contract_oracles' => non_neg_integer()
                                  , 'oracles'          => non_neg_integer()
                                  , 'oracle_queries'   => non_neg_integer()
                                  , 'locked'           => non_neg_integer()
                                  , 'auctions'         => non_neg_integer()
                                  }.
sum_total_coin(Trees) ->
    Accounts  = accounts(Trees),
    Channels  = channels(Trees),
    Contracts = contracts(Trees),
    Oracles   = oracles(Trees),
    Names     = ns(Trees),
    LockAccount   = aec_governance:locked_coins_holder_account(),
    AIterator     = aec_accounts_trees:mtree_iterator(Accounts),
    FirstAccount  = aeu_mtrees:iterator_next(AIterator),
    CIterator     = aesc_state_tree:mtree_iterator(Channels),
    FirstChannel  = aeu_mtrees:iterator_next(CIterator),
    ChannelAmount = sum_channels(FirstChannel, 0),
    AuctionIter   = aens_state_tree:auction_iterator(Names),
    FirstAuction  = aens_state_tree:auction_iterator_next(AuctionIter),
    SumAuctions   = sum_auctions(FirstAuction, 0),
    Sum = #{ 'accounts'         => 0
           , 'channels'         => ChannelAmount
           , 'contracts'        => 0
           , 'contract_oracles' => 0
           , 'oracles'          => 0
           , 'oracle_queries'   => 0
           , 'locked'           => 0
           , 'auctions'         => SumAuctions
           },
    sum_accounts(FirstAccount, LockAccount, Contracts, Oracles, Sum).

sum_accounts('$end_of_table',_LockAccount,_Contracts,_Oracles, Acc) ->
    Acc;
sum_accounts({LockAccount, SerAccount, Iter}, LockAccount, Contracts, Oracles, Acc) ->
    Account = aec_accounts:deserialize(LockAccount, SerAccount),
    Balance = aec_accounts:balance(Account),
    Acc1 = maps:update_with(locked, fun(X) -> X + Balance end, Acc),
    Next = aeu_mtrees:iterator_next(Iter),
    sum_accounts(Next, LockAccount, Contracts, Oracles, Acc1);
sum_accounts({Pubkey, SerAccount, Iter}, LockAccount, Contracts, Oracles, Acc) ->
    Account = aec_accounts:deserialize(Pubkey, SerAccount),
    Balance = aec_accounts:balance(Account),
    {IsContract, Contract} =
        case aect_state_tree:lookup_contract(Pubkey, Contracts, [no_store]) of
            none -> {false, undefined};
            {value, C} -> {true, C}
        end,
    IsOracle   = aeo_state_tree:is_oracle(Pubkey, Oracles),
    Tag = if
              IsContract and IsOracle -> contract_oracles;
              IsContract              -> contracts;
              IsOracle                -> oracles;
              true                    -> accounts
          end,
    Acc1 = maps:update_with(Tag, fun(X) -> X + Balance end, Acc),
    Acc2 = case IsOracle of
               true  -> sum_implicit_oracle_query_fees(Pubkey, Oracles, Acc1);
               false -> Acc1
           end,
    Acc3 = case IsContract of
               true ->
                   Deposit = aect_contracts:deposit(Contract),
                   maps:update_with(contracts, fun(X) -> X + Deposit end, Acc2);
               false ->
                   Acc2
           end,
    Next = aeu_mtrees:iterator_next(Iter),
    sum_accounts(Next, LockAccount, Contracts, Oracles, Acc3).

sum_implicit_oracle_query_fees(Pubkey, Oracles, Acc) ->
    Balance = sum_implicit_oracle_query_fees(Pubkey, Oracles, '$first', 0),
    maps:update_with(oracle_queries, fun(X) -> X + Balance end, Acc).

sum_implicit_oracle_query_fees(Pubkey, Oracles, Last, Acc) ->
    case aeo_state_tree:get_oracle_queries(Pubkey, Last, open, 10, Oracles) of
        [] -> Acc;
        List ->
            Acc1 = lists:foldl(fun(Query, X) ->
                                       X + aeo_query:fee(Query)
                               end, Acc, List),
            Last1 = aeo_query:id(lists:last(List)),
            sum_implicit_oracle_query_fees(Pubkey, Oracles, Last1, Acc1)
    end.

sum_channels('$end_of_table', Acc) ->
    Acc;
sum_channels({ChannelPubkey, SerChannel, Iter}, Acc) ->
    Channel = aesc_channels:deserialize(ChannelPubkey, SerChannel),
    Acc1 = Acc + aesc_channels:channel_amount(Channel),
    sum_channels(aeu_mtrees:iterator_next(Iter), Acc1).

sum_auctions('$end_of_table', Acc) ->
    Acc;
sum_auctions({AuctionHash, SerAuction, Iter}, Acc) ->
    Auction = aens_auctions:deserialize(AuctionHash, SerAuction),
    Acc1 = Acc + aens_auctions:name_fee(Auction),
    sum_auctions(aens_state_tree:auction_iterator_next(Iter), Acc1).

%%%=============================================================================
%%% Serialization for db storage
%%%=============================================================================

-define(AEC_TREES_VERSION, 0).

-spec deserialize_from_db(binary()) -> trees().
deserialize_from_db(Bin) when is_binary(Bin) ->
    [ {contracts_hash, Contracts}
    , {calls_hash, Calls}
    , {channels_hash, Channels}
    , {ns_hash, NS}
    , {ns_cache_hash, NSCache}
    , {oracles_hash, Oracles}
    , {oracles_cache_hash, OraclesCache}
    , {accounts_hash, Accounts}
    ] = lists:map(fun db_deserialize_hash/1,
                  aeser_chain_objects:deserialize(
                    trees_db,
                    ?AEC_TREES_VERSION,
                    db_serialization_template(?AEC_TREES_VERSION),
                    Bin
                   )),
    #trees{ contracts = aect_state_tree:new_with_backend(Contracts)
          , calls     = aect_call_state_tree:new_with_backend(Calls)
          , channels  = aesc_state_tree:new_with_backend(Channels)
          , ns        = aens_state_tree:new_with_backend(NS, NSCache)
          , oracles   = aeo_state_tree:new_with_backend(Oracles, OraclesCache)
          , accounts  = aec_accounts_trees:new_with_backend(Accounts)
          }.

-spec serialize_for_db(trees()) -> binary().
serialize_for_db(#trees{} = Trees) ->
    aeser_chain_objects:serialize(
      trees_db,
      ?AEC_TREES_VERSION,
      db_serialization_template(?AEC_TREES_VERSION),
      lists:map(fun db_serialize_hash/1,
                [ {contracts_hash,     contracts_hash(Trees)}
                , {calls_hash,         calls_hash(Trees)}
                , {channels_hash,      channels_hash(Trees)}
                , {ns_hash,            ns_hash(Trees)}
                , {ns_cache_hash,      ns_cache_hash(Trees)}
                , {oracles_hash,       oracles_hash(Trees)}
                , {oracles_cache_hash, oracles_cache_hash(Trees)}
                , {accounts_hash,      accounts_hash(Trees)}
                ])
     ).

db_serialization_template(?AEC_TREES_VERSION) ->
    [ {contracts_hash,     [binary]}
    , {calls_hash,         [binary]}
    , {channels_hash,      [binary]}
    , {ns_hash,            [binary]}
    , {ns_cache_hash,      [binary]}
    , {oracles_hash,       [binary]}
    , {oracles_cache_hash, [binary]}
    , {accounts_hash,      [binary]}
    ].

db_serialize_hash({Field, {ok, Hash}}) -> {Field, [Hash]};
db_serialize_hash({Field, {error, empty}}) -> {Field, []}.

db_deserialize_hash({Field, [Hash]}) -> {Field, Hash};
db_deserialize_hash({Field, []}) -> {Field, empty}.


-spec serialize_to_binary(trees()) -> binary().
serialize_to_binary(#trees{} = Trees) ->
    #trees{ contracts = Contracts
          , calls     = Calls
          , channels  = Channels
          , ns        = NS
          , oracles   = Oracles
          , accounts  = Accounts
          } = Trees,
    aeser_chain_objects:serialize(
      state_trees,
      ?AEC_TREES_VERSION,
      binary_serialization_template(?AEC_TREES_VERSION),
      [ {contracts,     aect_state_tree:to_binary_without_backend(Contracts)}
      , {calls,         aect_call_state_tree:to_binary_without_backend(Calls)}
      , {channels,      aesc_state_tree:to_binary_without_backend(Channels)}
      , {ns,            aens_state_tree:to_binary_without_backend(NS)}
      , {oracles,       aeo_state_tree:to_binary_without_backend(Oracles)}
      , {accounts,      aec_accounts_trees:to_binary_without_backend(Accounts)}
      ]).

-spec serialize_to_client(trees()) -> binary().
serialize_to_client(#trees{} = Trees) ->
    TreesBinary = serialize_to_binary(Trees),
    aeser_api_encoder:encode(state_trees, TreesBinary).

-spec deserialize_from_binary_without_backend(binary()) -> trees().
deserialize_from_binary_without_backend(Bin) ->
    [ {contracts, Contracts}
    , {calls, Calls}
    , {channels, Channels}
    , {ns, NS}
    , {oracles, Oracles}
    , {accounts, Accounts}
    ] = aeser_chain_objects:deserialize(
            state_trees,
            ?AEC_TREES_VERSION,
            binary_serialization_template(?AEC_TREES_VERSION),
            Bin),
    #trees{ contracts = aect_state_tree:from_binary_without_backend(Contracts)
          , calls     = aect_call_state_tree:from_binary_without_backend(Calls)
          , channels  = aesc_state_tree:from_binary_without_backend(Channels)
          , ns        = aens_state_tree:from_binary_without_backend(NS)
          , oracles   = aeo_state_tree:from_binary_without_backend(Oracles)
          , accounts  = aec_accounts_trees:from_binary_without_backend(Accounts)
          }.

binary_serialization_template(?AEC_TREES_VERSION) ->
    [ {contracts,     binary}
    , {calls,         binary}
    , {channels,      binary}
    , {ns,            binary}
    , {oracles,       binary}
    , {accounts,      binary}
    ].

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

internal_hash(Trees) ->
    internal_hash(Trees, {calls_hash, pad_empty(calls_hash(Trees))}).

internal_hash(Trees, {calls_hash, CallsRootHash}) ->
    %% Note that all hash sizes are checked in pad_empty/2
    Bin = <<?VERSION:64,
            (pad_empty(accounts_hash(Trees)))  /binary,
            CallsRootHash                      /binary,
            (pad_empty(channels_hash(Trees)))  /binary,
            (pad_empty(contracts_hash(Trees))) /binary,
            (pad_empty(ns_hash(Trees)))        /binary,
            (pad_empty(oracles_hash(Trees)))   /binary
          >>,
    aec_hash:hash(state_trees, Bin).

accounts_hash(Trees) ->
    aec_accounts_trees:root_hash(accounts(Trees)).

calls_hash(Trees) ->
    aect_call_state_tree:root_hash(calls(Trees)).

channels_hash(Trees) ->
    aesc_state_tree:root_hash(channels(Trees)).

contracts_hash(Trees) ->
    aect_state_tree:root_hash(contracts(Trees)).

oracles_hash(Trees) ->
    aeo_state_tree:root_hash(oracles(Trees)).

oracles_cache_hash(Trees) ->
    aeo_state_tree:cache_root_hash(oracles(Trees)).

ns_hash(Trees) ->
    aens_state_tree:root_hash(ns(Trees)).

ns_cache_hash(Trees) ->
    aens_state_tree:cache_root_hash(ns(Trees)).

pad_empty({ok, H}) when is_binary(H), byte_size(H) =:= ?STATE_HASH_BYTES -> H;
pad_empty({error, empty}) -> <<0:?STATE_HASH_BYTES/unit:8>>.

internal_commit_to_db(Trees) ->
    Trees#trees{ contracts = aect_state_tree:commit_to_db(contracts(Trees))
               , calls     = aect_call_state_tree:commit_to_db(calls(Trees))
               , channels  = aesc_state_tree:commit_to_db(channels(Trees))
               , ns        = aens_state_tree:commit_to_db(ns(Trees))
               , oracles   = aeo_state_tree:commit_to_db(oracles(Trees))
               , accounts  = aec_accounts_trees:commit_to_db(accounts(Trees))
               }.

apply_txs_on_state_trees(SignedTxs, Trees, Env) ->
    apply_txs_on_state_trees(SignedTxs, [], [], Trees, Env, []).

apply_txs_on_state_trees_strict(SignedTxs, Trees, Env) ->
    apply_txs_on_state_trees(SignedTxs, [], [], Trees, Env, [strict, tx_events]).

apply_txs_on_state_trees(SignedTxs, Trees, Env, Opts) ->
    apply_txs_on_state_trees(SignedTxs, [], [], Trees, Env, Opts).

apply_txs_on_state_trees([], ValidTxs, InvalidTxs, Trees,Env,Opts) ->
    lager:debug("Opts = ~p", [Opts]),
    Events = case proplists:get_bool(tx_events, Opts) of
                 true -> aetx_env:events(Env);
                 false -> []
             end,
    lager:debug("tx_events: ~p", [Events]),
    %% if Events =/= [] -> lager:debug("tx_events: ~p", [Events]);
    %%    true -> ok
    %% end,
    {ok, lists:reverse(ValidTxs), lists:reverse(InvalidTxs), Trees, Events};
apply_txs_on_state_trees([SignedTx | Rest], ValidTxs, InvalidTxs, Trees, Env, Opts) ->
    Strict     = proplists:get_value(strict, Opts, false),
    DontVerify = proplists:get_value(dont_verify_signature, Opts, false),
    Protocol   = aetx_env:consensus_version(Env),
    case verify_signature(SignedTx, Trees, Protocol, DontVerify) of
        ok ->
            Env1 = aetx_env:set_signed_tx(Env, {value, SignedTx}),
            Tx = aetx_sign:tx(SignedTx),
            try aetx:process(Tx, Trees, Env1) of
                {ok, Trees1, Env20} ->
                    Env21 = aetx_env:update_env(Env20, Env),
                    Valid1 = [SignedTx | ValidTxs],
                    apply_txs_on_state_trees(Rest, Valid1, InvalidTxs, Trees1, Env21, Opts);
                {error, Reason} when Strict ->
                    lager:debug("Tx ~p cannot be applied due to an error ~p", [Tx, Reason]),
                    {error, Reason};
                {error, Reason} when not Strict ->
                    lager:debug("Tx ~p cannot be applied due to an error ~p", [Tx, Reason]),
                    Invalid1 = [SignedTx | InvalidTxs],
                    apply_txs_on_state_trees(Rest, ValidTxs, Invalid1, Trees, Env, Opts)
            catch
                Type:What when Strict ->
                    Reason = {Type, What},
                    lager:error("Tx ~p cannot be applied due to an error ~p", [Tx, Reason]),
                    {error, Reason};
                Type:What when not Strict ->
                    Reason = {Type, What},
                    lager:debug("Tx ~p cannot be applied due to an error ~p", [Tx, Reason]),
                    Invalid1 = [SignedTx| InvalidTxs],
                    apply_txs_on_state_trees(Rest, ValidTxs, Invalid1, Trees, Env, Opts)
            end;
        {error, signature_check_failed} = E when Strict ->
            lager:debug("Signed tx ~p is not correctly signed.", [SignedTx]),
            E;
        {error, signature_check_failed} when not Strict ->
            lager:debug("Signed tx ~p is not correctly signed.", [SignedTx]),
            Invalid1 = [SignedTx | InvalidTxs],
            apply_txs_on_state_trees(Rest, ValidTxs, Invalid1, Trees, Env, Opts)
    end.

verify_signature(_, _, _, true)               -> ok;
verify_signature(STx, Trees, Protocol, false) -> aetx_sign:verify(STx, Trees, Protocol).

-spec grant_fee(aec_keys:pubkey(), trees(), non_neg_integer()) -> trees().
grant_fee(BeneficiaryPubKey, Trees0, Fee) ->
    Trees1 = ensure_account(BeneficiaryPubKey, Trees0),
    AccountsTrees1 = accounts(Trees1),

    {value, Account1} = aec_accounts_trees:lookup(BeneficiaryPubKey, AccountsTrees1),
    {ok, Account} = aec_accounts:earn(Account1, Fee),

    AccountsTrees = aec_accounts_trees:enter(Account, AccountsTrees1),
    set_accounts(Trees1, AccountsTrees).

-spec ensure_account(aec_keys:pubkey(), trees()) -> trees().
ensure_account(AccountPubkey, Trees0) ->
    AccountsTrees0 = aec_trees:accounts(Trees0),
    case aec_accounts_trees:lookup(AccountPubkey, AccountsTrees0) of
        {value, _Account} ->
            Trees0;
        none ->
            %% Add newly referenced account (w/0 amount) to the state
            Account = aec_accounts:new(AccountPubkey, 0),
            AccountsTrees = aec_accounts_trees:enter(Account, AccountsTrees0),
            aec_trees:set_accounts(Trees0, AccountsTrees)
    end.

%%%=============================================================================
%%% Proof of Inclusion (PoI)
%%%=============================================================================

internal_new_poi(Trees) ->
    #poi{ accounts  = new_part_poi(pad_empty(accounts_hash(Trees)))
        , calls     = new_part_poi(pad_empty(calls_hash(Trees)))
        , channels  = new_part_poi(pad_empty(channels_hash(Trees)))
        , contracts = new_part_poi(pad_empty(contracts_hash(Trees)))
        , ns        = new_part_poi(pad_empty(ns_hash(Trees)))
        , oracles   = new_part_poi(pad_empty(oracles_hash(Trees)))
        }.

internal_poi_hash(#poi{} = POI) ->
    %% Note that all hash sizes are checked in pad_empty/2
    Bin = <<?VERSION:64,
            (part_poi_hash(POI#poi.accounts)) /binary,
            (part_poi_hash(POI#poi.calls))    /binary,
            (part_poi_hash(POI#poi.channels)) /binary,
            (part_poi_hash(POI#poi.contracts))/binary,
            (part_poi_hash(POI#poi.ns))       /binary,
            (part_poi_hash(POI#poi.oracles))  /binary
          >>,
    aec_hash:hash(state_trees, Bin).

part_poi_hash(empty) -> <<0:?STATE_HASH_BYTES/unit:8>>;
part_poi_hash({poi, Poi}) -> aec_poi:root_hash(Poi).

internal_add_accounts_poi(_Pubkey,_Trees, #poi{accounts = empty}) ->
    {error, not_present};
internal_add_accounts_poi(Pubkey, Trees, #poi{accounts = {poi, APoi}} = Poi) ->
    case aec_accounts_trees:add_poi(Pubkey, Trees, APoi) of
        {ok, NewAPoi} ->
            {ok, Poi#poi{accounts = {poi, NewAPoi}}};
        {error, _} = E -> E
    end.

internal_add_contracts_poi(_ContractPubKey, _Trees, #poi{contracts = empty}) ->
    {error, not_present};
internal_add_contracts_poi(ContractPubKey, Trees, #poi{contracts = {poi, CPoi}} = Poi) ->
    case aect_state_tree:add_poi(ContractPubKey, Trees, CPoi) of
        {ok, NewAPoi} ->
            {ok, Poi#poi{contracts = {poi, NewAPoi}}};
        {error, _} = E -> E
    end.

internal_lookup_accounts_poi(_Pubkey, #poi{accounts = empty}) ->
    {error, not_found};
internal_lookup_accounts_poi(Pubkey, #poi{accounts = {poi, APoi}} = _Poi) ->
    case aec_accounts_trees:lookup_poi(Pubkey, APoi) of
        {ok, Account} ->
            {ok, Account};
        {error, not_found} = E -> E
    end.

internal_lookup_contracts_poi(_Pubkey, #poi{contracts = empty}) ->
    {error, not_found};
internal_lookup_contracts_poi(Pubkey, #poi{contracts = {poi, CPoi}} = _Poi) ->
    case aect_state_tree:lookup_poi(Pubkey, CPoi) of
        {ok, Contract} -> {ok, Contract};
        {error, not_found} = E -> E
    end.

internal_verify_accounts_poi(_AccountPubkey,_Account, #poi{accounts = empty}) ->
    {error, empty_accounts_poi};
internal_verify_accounts_poi(AccountPubkey, Account, #poi{accounts = {poi, APoi}}) ->
    aec_accounts_trees:verify_poi(AccountPubkey, Account, APoi).

internal_verify_contracts_poi(_ContractPubkey, _Contract, #poi{contracts = empty}) ->
    {error, empty_contracts_poi};
internal_verify_contracts_poi(ContractPubKey, Contract, #poi{contracts = {poi, CPoi}}) ->
    aect_state_tree:verify_poi(ContractPubKey, Contract, CPoi).

new_part_poi(<<0:?STATE_HASH_BYTES/unit:8>>) ->
    empty;
new_part_poi(<<_:?STATE_HASH_BYTES/unit:8>> = Hash) ->
    {poi, aec_poi:new(Hash)}.

-define(POI_VSN, 1).

internal_serialize_poi(Poi) ->
    Fields = internal_serialize_poi_fields(Poi),
    internal_serialize_poi_from_fields(Fields).

internal_serialize_poi_fields(#poi{ accounts  = Accounts
                                  , calls     = Calls
                                  , channels  = Channels
                                  , contracts = Contracts
                                  , ns        = Ns
                                  , oracles   = Oracles
                                  }) ->
    [ {accounts  , poi_serialization_format(Accounts)}
    , {calls     , poi_serialization_format(Calls)}
    , {channels  , poi_serialization_format(Channels)}
    , {contracts , poi_serialization_format(Contracts)}
    , {ns        , poi_serialization_format(Ns)}
    , {oracles   , poi_serialization_format(Oracles)}
    ].

internal_serialize_poi_from_fields(Fields) ->
    aeser_chain_objects:serialize(trees_poi,
                                       ?POI_VSN,
                                       internal_serialize_poi_template(?POI_VSN),
                                       Fields).

poi_serialization_format(empty) -> [];
poi_serialization_format({poi, Poi}) -> [aec_poi:serialization_format(Poi)].

from_poi_serialization_format([]) -> empty;
from_poi_serialization_format([Poi]) -> {poi, aec_poi:from_serialization_format(Poi)}.

internal_deserialize_poi(Bin) ->
    Template = internal_serialize_poi_template(?POI_VSN),

    [ {accounts  , Accounts}
    , {calls     , Calls}
    , {channels  , Channels}
    , {contracts , Contracts}
    , {ns        , Ns}
    , {oracles   , Oracles}
    ] = aeser_chain_objects:deserialize(trees_poi, ?POI_VSN, Template, Bin),

    #poi{ accounts  = from_poi_serialization_format(Accounts)
        , calls     = from_poi_serialization_format(Calls)
        , channels  = from_poi_serialization_format(Channels)
        , contracts = from_poi_serialization_format(Contracts)
        , ns        = from_poi_serialization_format(Ns)
        , oracles   = from_poi_serialization_format(Oracles)
        }.


internal_serialize_poi_template(?POI_VSN) ->
    PoiTemplate = aec_poi:serialization_format_template(),
    [{X, [PoiTemplate]}
     || X <- [ accounts
             , calls
             , channels
             , contracts
             , ns
             , oracles
             ]].
