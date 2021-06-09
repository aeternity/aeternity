%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aeprimop_state).

-export([ delete_x/3
        , delete_account/2
        , delete_contract/2
        , final_trees/1
        , find_account/2
        , find_auth_call/3
        , find_channel/2
        , find_commitment/2
        , find_name_auction/2
        , find_contract_without_store/2
        , find_name/2
        , find_oracle/2
        , find_oracle_query/3
        , get_account/2
        , get_auth_call/3
        , get_channel/2
        , get_commitment/3
        , get_name_auction/3
        , get_contract/2
        , get_contract_no_cache/2
        , get_contract_without_store/2
        , get_name/2
        , get_oracle/3
        , get_oracle_query/3
        , get_var/3
        , new/2
        , put_account/2
        , put_auth_call/2
        , put_call/2
        , put_channel/2
        , put_commitment/2
        , put_name_auction/2
        , put_contract/2
        , put_name/2
        , put_oracle/2
        , put_oracle_query/2
        ]).

-export([ cache_write_through/1
        , set_var/4
        , tx_env/1
        ]).

-include("aeprimop_state.hrl").

-type state() :: #state{}.

-export_type([state/0, channel_key/0]).

-type runtime_error() :: account_not_found
    | auth_call_not_found
    | channel_does_not_exist
    | contract_does_not_exist
    | name_does_not_exist
    | oracle_does_not_exist
    | no_matching_oracle_query
    | account_is_not_an_active_oracle
    | name_not_preclaimed
    | name_not_in_auction.
-type pubkey() :: aec_keys:pubkey().
-type hash() :: aec_hash:hash().
-type auction_hash() :: aens_hash:auction_hash().
-type aec_trees() :: aec_trees:trees().
-type aetx_env() :: aetx_env:env().
-type call_id() :: aect_call:id().
-type tag() :: auction
    | account
    | auth_call
    | channel
    | contract
    | call
    | name
    | oracle
    | oracle_query
    | commitment
    | name_auction.
-type object() :: aec_accounts:account()
    | aect_call:call()
    | aect_contracts:contract()
    | aens_auctions:auction()
    | aens_commitments:commitment()
    | aens_names:name()
    | aeo_oracles:oracle()
    | aeo_query:query()
    | aesc_channels:channel().
-type channel_key() :: binary() | {var | binary(), atom() | binary()}.
-type contract() :: aect_contracts:contract().

%%%===================================================================
%%% API
%%%===================================================================

-spec new(aec_trees(), aetx_env()) -> state().
new(Trees, TxEnv) ->
    #state{ trees = Trees
          , cache = dict:new()
          , env = dict:new()
          , height = aetx_env:height(TxEnv)
          , tx_env = TxEnv
          , protocol = aetx_env:consensus_version(TxEnv)
          }.

-spec final_trees(state()) -> aec_trees().
final_trees(State) ->
    #state{trees = Trees} = cache_write_through(State),
    Trees.

-spec tx_env(state()) -> aetx_env().
tx_env(#state{tx_env = TxEnv}) ->
    TxEnv.

%%%===================================================================
%%% Error handling

-spec runtime_error(runtime_error()) -> no_return().
runtime_error(Error) ->
    error({?MODULE, Error}).

%%%===================================================================
%%% Access to cache or trees

-spec delete_account(hash(), state()) -> state().
delete_account(Key, S) ->
    delete_x(account, Key, S).

-spec find_account(hash(), state()) -> {term(), state()} | none.
find_account(Key, S) ->
    find_x(account, Key, S).

-spec get_account(hash(), state()) -> {object(), state()}.
get_account(Key, S) ->
    get_x(account, Key, account_not_found, S).

-spec put_account(object(), state()) -> state().
put_account(Object, S) ->
    cache_put(account, Object, S).

%%----------

-spec find_auth_call(pubkey(), call_id(), state()) -> {object(), state()} | none.
find_auth_call(Pubkey, AuthCallId, S) ->
    find_x(auth_call, {Pubkey, AuthCallId}, S).

-spec get_auth_call(pubkey(), call_id(), state()) -> {object(), state()}.
get_auth_call(Pubkey, AuthCallId, S) ->
    get_x(auth_call, {Pubkey, AuthCallId}, auth_call_not_found, S).

-spec put_auth_call(aect_call:call(), state()) -> state().
put_auth_call(AuthCall, S) ->
    cache_put(auth_call, AuthCall, S).

%%----------

-spec put_call(aect_call:call(), state()) -> state().
put_call(Object, S) ->
    cache_put(call, Object, S).

%%----------

-spec find_channel(channel_key(), state()) -> {term(), state()} | none.
find_channel(Key, S) ->
    find_x(channel, Key, S).

-spec get_channel(channel_key(), state()) -> {term(), state()}.
get_channel(Key, S) ->
    get_x(channel, Key, channel_does_not_exist, S).

-spec put_channel(object(), state()) -> state().
put_channel(Object, S) ->
    cache_put(channel, Object, S).

%%----------

-spec delete_contract(channel_key(), state()) -> state().
delete_contract(Key, S) ->
    delete_x(contract, Key, S).

-spec get_contract(channel_key(), state()) -> {object(), state()} | no_return().
get_contract(Key, S) ->
    get_x(contract, Key, contract_does_not_exist, S).

-spec put_contract(object(), state()) -> state().
put_contract(Object, S) ->
    cache_put(contract, Object, S).

%% Used from fate that has its own store cache management
-spec get_contract_no_cache(pubkey(), state()) -> contract().
get_contract_no_cache(Key, S) ->
    {Contract, _} = get_x(contract, Key, contract_does_not_exist, S),
    Contract.

%% NOTE: This does not cache the contract to avoid over-writing
%% the correct store later
-spec find_contract_without_store(pubkey(), state()) -> {value, contract()} | none.
find_contract_without_store(Pubkey, S) ->
    case cache_find(contract, Pubkey, S) of
        {value, _} = Res -> Res;
        none ->
            CTree = aec_trees:contracts(S#state.trees),
            aect_state_tree:lookup_contract(Pubkey, CTree, [no_store])
    end.

%% NOTE: This does not cache the contract to avoid over-writing
%% the correct store later
-spec get_contract_without_store(pubkey(), state()) -> contract() | no_return().
get_contract_without_store(Pubkey, S) ->
    case cache_find(contract, Pubkey, S) of
        {value, C} -> C;
        none ->
            CTree = aec_trees:contracts(S#state.trees),
            case aect_state_tree:lookup_contract(Pubkey, CTree, [no_store]) of
                none -> runtime_error(contract_does_not_exist);
                {value, C} -> C
            end
    end.

%%----------

-spec find_name(channel_key(), state()) -> {object(), state()} | none.
find_name(Key, S) ->
    find_x(name, Key, S).

-spec get_name(channel_key(), state()) -> {object(), state()} | no_return().
get_name(Key, S) ->
    get_x(name, Key, name_does_not_exist, S).

-spec put_name(object(), state()) -> state().
put_name(Object, S) ->
    cache_put(name, Object, S).

%%----------

-spec find_oracle(channel_key(), state()) -> {object(), state()} | none.
find_oracle(Key, S) ->
    find_x(oracle, Key, S).

-spec get_oracle(channel_key(), runtime_error(), state()) -> {object(), state()} | no_return().
get_oracle(Key, Error, S) ->
    get_x(oracle, Key, Error, S).

-spec put_oracle(object(), state()) -> state().
put_oracle(Object, S) ->
    cache_put(oracle, Object, S).

%%----------

-spec find_oracle_query(channel_key(), atom() | binary(), state()) -> {object(), state()} | none.
find_oracle_query(OraclePubkey, QueryId, S) ->
    find_x(oracle_query, {OraclePubkey, QueryId}, S).

-spec get_oracle_query(channel_key(), atom() | binary(), state()) -> {object(), state()} | no_return().
get_oracle_query(OraclePubkey, QueryId, S) ->
    get_x(oracle_query, {OraclePubkey, QueryId}, no_matching_oracle_query, S).

-spec put_oracle_query(object(), state()) -> state().
put_oracle_query(Object, S) ->
    cache_put(oracle_query, Object, S).

%%----------

-spec find_commitment(hash(), state()) -> {object(), state()} | none.
find_commitment(Hash, S) ->
    find_x(commitment, Hash, S).

-spec get_commitment(hash(), runtime_error(), state()) -> {object(), state()} | no_return().
get_commitment(Hash, Error, S) ->
    get_x(commitment, Hash, Error, S).

-spec put_commitment(object(), state()) -> state().
put_commitment(Object, S) ->
    cache_put(commitment, Object, S).

%%----------

-spec find_name_auction(channel_key(), state()) -> {term(), state()} | none.
find_name_auction(Hash, S) ->
    find_x(name_auction, Hash, S).

-spec get_name_auction(auction_hash(), runtime_error(), state()) -> {object(), state()} | no_return().
get_name_auction(Hash, Error, S) ->
    get_x(name_auction, Hash, Error, S).

-spec put_name_auction(object(), state()) -> state().
put_name_auction(Object, S) ->
    cache_put(name_auction, Object, S).

%%----------

-spec find_x(tag(), channel_key(), state()) -> {object(), state()} | none.
find_x(Tag, Key, S) ->
    case cache_find(Tag, Key, S) of
        none ->
            case trees_find(Tag, Key, S) of
                none -> none;
                {value, Val} ->
                    {Val, cache_put(Tag, Val, S)}
            end;
        {value, Val} ->
            {Val, S}
    end.

-spec get_x(tag(), channel_key(), runtime_error(), state()) -> {object(), state()} | no_return().
get_x(Tag, Key, Error, S) when is_atom(Error) ->
    case find_x(Tag, Key, S) of
        none -> runtime_error(Error);
        {_, _} = Ret -> Ret
    end.

-spec delete_x(tag(), hash(), state()) -> state().
delete_x(channel, Hash, #state{trees = Trees} = S) ->
    S1 = cache_drop(channel, Hash, S),
    CTree = aec_trees:channels(Trees),
    CTree1 = aesc_state_tree:delete(Hash, CTree),
    S1#state{trees = aec_trees:set_channels(Trees, CTree1)};
delete_x(name_auction, Hash, #state{trees = Trees} = S) ->
    S1 = cache_drop(name_auction, Hash, S),
    NTree = aec_trees:ns(Trees),
    NTree1 = aens_state_tree:delete_name_auction(Hash, NTree),
    S1#state{trees = aec_trees:set_ns(Trees, NTree1)};
delete_x(contract, PK, #state{trees = Trees} = S) ->
    S1 = cache_drop(contract, PK, S),
    NTree = aec_trees:contracts(Trees),
    NTree1 = aect_state_tree:delete_contract(PK, NTree),
    S1#state{trees = aec_trees:set_contracts(Trees, NTree1)};
delete_x(account, PK, #state{trees = Trees} = S) ->
    S1 = cache_drop(account, PK, S),
    NTree = aec_trees:accounts(Trees),
    NTree1 = aec_accounts_trees:delete(PK, NTree),
    S1#state{trees = aec_trees:set_accounts(Trees, NTree1)};
delete_x(commitment, Hash, #state{trees = Trees} = S) ->
    S1 = cache_drop(commitment, Hash, S),
    NTree = aec_trees:ns(Trees),
    NTree1 = aens_state_tree:delete_commitment(Hash, NTree),
    S1#state{trees = aec_trees:set_ns(Trees, NTree1)}.

%%%===================================================================
%%% Access to trees

-spec trees_find(tag(), channel_key(), state()) -> {value, object()} | none.
trees_find(account, Key, #state{trees = Trees} = S) ->
    ATree = aec_trees:accounts(Trees),
    aec_accounts_trees:lookup(get_var(Key, account, S), ATree);
%% Not used yet, and Dialyzer finds out
%% trees_find(call, Key, #state{trees = Trees} = S) ->
%%     CTree = aec_trees:calls(Trees),
%%     aect_call_state_tree:lookup(get_var(Key, call, S), CTree);
trees_find(auth_call, {Pubkey, AuthCallId}, #state{trees = Trees}) ->
    CTree = aec_trees:calls(Trees),
    aect_call_state_tree:lookup_call(Pubkey, AuthCallId, CTree);
trees_find(channel, Key, #state{trees = Trees} = S) ->
    CTree = aec_trees:channels(Trees),
    aesc_state_tree:lookup(get_var(Key, channel, S), CTree);
trees_find(contract, Key, #state{trees = Trees} = S) ->
    CTree = aec_trees:contracts(Trees),
    aect_state_tree:lookup_contract(get_var(Key, contract, S), CTree);
trees_find(commitment, Key, #state{trees = Trees} = S) ->
    NTree = aec_trees:ns(Trees),
    aens_state_tree:lookup_commitment(get_var(Key, commitment, S), NTree);
trees_find(name_auction, Key, #state{trees = Trees} = S) ->
    NTree = aec_trees:ns(Trees),
    aens_state_tree:lookup_name_auction(get_var(Key, name_auction, S), NTree);
trees_find(name, Key, #state{trees = Trees} = S) ->
    NTree = aec_trees:ns(Trees),
    aens_state_tree:lookup_name(get_var(Key, name, S), NTree);
trees_find(oracle, Key, #state{trees = Trees} = S) ->
    OTree = aec_trees:oracles(Trees),
    aeo_state_tree:lookup_oracle(get_var(Key, oracle, S), OTree);
trees_find(oracle_query, Key, #state{trees = Trees} = S) ->
    {OraclePubkey, QueryId} = get_var(Key, oracle_query, S),
    OTree = aec_trees:oracles(Trees),
    aeo_state_tree:lookup_query(OraclePubkey, QueryId, OTree).

%%%===================================================================
%%% Cache

-define(IS_TAG(X), ((X =:= account)
    orelse (X =:= auth_call)
    orelse (X =:= call)
    orelse (X =:= channel)
    orelse (X =:= contract)
    orelse (X =:= oracle)
    orelse (X =:= oracle_query)
    orelse (X =:= commitment)
    orelse (X =:= name_auction)
    orelse (X =:= name))
).

-spec cache_find(tag(), channel_key(), state()) -> {value, object()} | none.
cache_find(Tag, Key, #state{cache = C} = S) when ?IS_TAG(Tag) ->
    case dict:find({Tag, get_var(Key, Tag, S)}, C) of
        {ok, Val} -> {value, Val};
        error -> none
    end.

-spec cache_drop(tag(), hash(), state()) -> state().
cache_drop(channel, Hash, #state{cache = C} = S) ->
    S#state{cache = dict:erase({channel, Hash}, C)};
cache_drop(name_auction, Hash, #state{cache = C} = S) ->
    S#state{cache = dict:erase({name_auction, Hash}, C)};
cache_drop(contract, PK, #state{cache = C} = S) ->
    S#state{cache = dict:erase({contract, PK}, C)};
cache_drop(account, PK, #state{cache = C} = S) ->
    S#state{cache = dict:erase({account, PK}, C)};
cache_drop(commitment, Hash, #state{cache = C} = S) ->
    S#state{cache = dict:erase({commitment, Hash}, C)}.

-spec cache_put(tag(), object(), state()) -> state().
cache_put(account, Val, #state{cache = C} = S) ->
    Pubkey = aec_accounts:pubkey(Val),
    S#state{cache = dict:store({account, Pubkey}, Val, C)};
cache_put(auth_call, Val, #state{cache = C} = S) ->
    Id = aect_call:id(Val),
    Pubkey = aect_call:caller_pubkey(Val),
    S#state{cache = dict:store({auth_call, {Pubkey, Id}}, Val, C)};
cache_put(call, Val, #state{cache = C} = S) ->
    Id = aect_call:id(Val),
    S#state{cache = dict:store({call, Id}, Val, C)};
cache_put(channel, Val, #state{cache = C} = S) ->
    Pubkey = aesc_channels:pubkey(Val),
    S#state{cache = dict:store({channel, Pubkey}, Val, C)};
cache_put(contract, Val, #state{cache = C} = S) ->
    Pubkey = aect_contracts:pubkey(Val),
    S#state{cache = dict:store({contract, Pubkey}, Val, C)};
cache_put(commitment, Val, #state{cache = C} = S) ->
    Hash = aens_commitments:hash(Val),
    S#state{cache = dict:store({commitment, Hash}, Val, C)};
cache_put(name_auction, Val, #state{cache = C} = S) ->
    Hash = aens_auctions:hash(Val),
    S#state{cache = dict:store({name_auction, Hash}, Val, C)};
cache_put(name, Val, #state{cache = C} = S) ->
    Hash = aens_names:hash(Val),
    S#state{cache = dict:store({name, Hash}, Val, C)};
cache_put(oracle, Val, #state{cache = C} = S) ->
    Pubkey = aeo_oracles:pubkey(Val),
    S#state{cache = dict:store({oracle, Pubkey}, Val, C)};
cache_put(oracle_query, Val, #state{cache = C} = S) ->
    Pubkey = aeo_query:oracle_pubkey(Val),
    QueryId = aeo_query:id(Val),
    S#state{cache = dict:store({oracle_query, {Pubkey, QueryId}}, Val, C)}.

-spec cache_write_through(state()) -> state().
cache_write_through(#state{cache = C, trees = T} = S) ->
    Trees = dict:fold(fun cache_write_through_fun/3, T, C),
    S#state{trees = Trees, cache = dict:new()}.

%% TODO: Should have a dirty flag.
-spec cache_write_through_fun({tag(), _}, object(), aec_trees()) -> aec_trees().
cache_write_through_fun({account, _Pubkey}, Account, Trees) ->
    ATrees = aec_trees:accounts(Trees),
    ATrees1 = aec_accounts_trees:enter(Account, ATrees),
    aec_trees:set_accounts(Trees, ATrees1);
cache_write_through_fun({auth_call, {_Pubkey, _Id}}, Call, Trees) ->
    CTree = aec_trees:calls(Trees),
    CTree1 = aect_call_state_tree:enter_auth_call(Call, CTree),
    aec_trees:set_calls(Trees, CTree1);
cache_write_through_fun({call, _Id}, Call, Trees) ->
    CTree = aec_trees:calls(Trees),
    CTree1 = aect_call_state_tree:insert_call(Call, CTree),
    aec_trees:set_calls(Trees, CTree1);
cache_write_through_fun({channel, _Pubkey}, Channel, Trees) ->
    CTree = aec_trees:channels(Trees),
    CTree1 = aesc_state_tree:enter(Channel, CTree),
    aec_trees:set_channels(Trees, CTree1);
cache_write_through_fun({contract, Pubkey}, Contract, Trees) ->
    %% NOTE: There is a semantical difference between inserting a new contract
    %%       and updating one.
    CTree = aec_trees:contracts(Trees),
    case aect_state_tree:lookup_contract(Pubkey, CTree, [no_store]) of
        {value, _} ->
            CTree1 = aect_state_tree:enter_contract(Contract, CTree),
            aec_trees:set_contracts(Trees, CTree1);
        none ->
            CTree1 = aect_state_tree:insert_contract(Contract, CTree),
            aec_trees:set_contracts(Trees, CTree1)
    end;
cache_write_through_fun({commitment, _Hash}, Commitment, Trees) ->
    NTree = aec_trees:ns(Trees),
    NTree1 = aens_state_tree:enter_commitment(Commitment, NTree),
    aec_trees:set_ns(Trees, NTree1);
cache_write_through_fun({name_auction, _Hash}, Name, Trees) ->
    NTree = aec_trees:ns(Trees),
    NTree1 = aens_state_tree:enter_name_auction(Name, NTree),
    aec_trees:set_ns(Trees, NTree1);
cache_write_through_fun({name, _Hash}, Name, Trees) ->
    NTree = aec_trees:ns(Trees),
    NTree1 = aens_state_tree:enter_name(Name, NTree),
    aec_trees:set_ns(Trees, NTree1);
cache_write_through_fun({oracle, _Pubkey}, Oracle, Trees) ->
    OTrees = aec_trees:oracles(Trees),
    OTrees1 = aeo_state_tree:enter_oracle(Oracle, OTrees),
    aec_trees:set_oracles(Trees, OTrees1);
cache_write_through_fun({oracle_query, {_Pubkey, _Id}}, Query, Trees) ->
    OTrees = aec_trees:oracles(Trees),
    OTrees1 = aeo_state_tree:enter_query(Query, OTrees),
    aec_trees:set_oracles(Trees, OTrees1).

%%%===================================================================
%%% Variable environment

-spec get_var(channel_key(), _, state()) -> any().
get_var({var, X}, Tag, #state{env = E}) when is_atom(X) ->
    {Tag, Val} = dict:fetch(X, E),
    Val;
get_var({X, Y} = Res, oracle_query, #state{}) when is_binary(X),
                                                   is_binary(Y) ->
    Res;
get_var({X, Y} = Res, auth_call, #state{}) when is_binary(X),
                                                is_binary(Y) ->
    Res;
get_var(X, _Tag, #state{}) when is_binary(X) ->
    X.

-spec set_var(channel_key(), tag(), pubkey(), state()) -> state().
set_var({var, X}, account = Tag, Pubkey, #state{} = S)
    when is_atom(X), is_binary(Pubkey) ->
    S#state{env = dict:store(X, {Tag, Pubkey}, S#state.env)};
set_var(Var, Tag, Pubkey, _S) ->
    error({illegal_assignment, Var, Tag, Pubkey}).
