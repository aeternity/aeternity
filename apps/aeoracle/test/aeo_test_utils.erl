%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Test utils for oracles
%%% @end
%%%-------------------------------------------------------------------
-module(aeo_test_utils).

-export([ extend_tx/2
        , extend_tx/3
        , new_state/0
        , oracles/1
        , priv_key/2
        , query_tx/3
        , query_tx/4
        , register_tx/2
        , register_tx/3
        , response_tx/4
        , response_tx/5
        , set_account_balance/3
        , set_trees/2
        , setup_new_account/1
        , setup_new_account/2
        , trees/1
        , ttl_defaults/0
        ]).

-include_lib("apps/aecore/include/blocks.hrl").

%%%===================================================================
%%% Test state
%%%===================================================================

new_state() ->
    #{}.

trees(#{} = S) ->
    maps:get(trees, S, aec_trees:new()).

set_trees(Trees, S) ->
    S#{trees => Trees}.

insert_key_pair(Pub, Priv, S) ->
    Old = key_pairs(S),
    S#{key_pairs => Old#{Pub => Priv}}.

key_pairs(S) -> maps:get(key_pairs, S, #{}).

next_nonce(PubKey, S) ->
    Account = aec_accounts_trees:get(PubKey, aec_trees:accounts(trees(S))),
    aec_accounts:nonce(Account) + 1.

priv_key(PubKey, State) ->
    maps:get(PubKey, key_pairs(State)).

ttl_defaults() ->
    #{ oracle => 250, query => 50, response => 75, extend => 125 }.

%%%===================================================================
%%% Info API
%%%===================================================================

oracles(State) ->
    aec_trees:oracles(trees(State)).

%%%===================================================================
%%% Register tx
%%%===================================================================

register_tx(PubKey, State) ->
    register_tx(PubKey, #{}, State).

register_tx(PubKey, Spec0, State) ->
    Spec = maps:merge(register_tx_default_spec(PubKey, State), Spec0),
    {ok, Tx} = aeo_register_tx:new(Spec),
    Tx.

register_tx_default_spec(PubKey, State) ->
    #{ account    => aec_id:create(account, PubKey)
     , oracle_ttl => {delta, maps:get(oracle, ttl_defaults())}
     , fee        => 5
     , nonce      => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , query_fee  => 5
     , query_spec => <<"string()">>
     , response_spec => <<"boolean() | integer()">>
     , ttl        => 0
     }.

%%%===================================================================
%%% Extend tx
%%%===================================================================

extend_tx(PubKey, State) ->
    extend_tx(PubKey, #{}, State).

extend_tx(PubKey, Spec0, State) ->
    Spec = maps:merge(extend_tx_default_spec(PubKey, State), Spec0),
    {ok, Tx} = aeo_extend_tx:new(Spec),
    Tx.

extend_tx_default_spec(PubKey, State) ->
    #{ oracle     => aec_id:create(oracle, PubKey)
     , oracle_ttl => {delta, maps:get(extend, ttl_defaults())}
     , fee        => 5
     , nonce      => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , ttl        => 0
     }.

%%%===================================================================
%%% Query tx
%%%===================================================================

query_tx(PubKey, OracleKey, State) ->
    query_tx(PubKey, OracleKey, #{}, State).

query_tx(PubKey, OracleKey, Spec0, State) ->
    Spec = maps:merge(query_tx_default_spec(PubKey, OracleKey, State), Spec0),
    {ok, Tx} = aeo_query_tx:new(Spec),
    Tx.

query_tx_default_spec(PubKey, OracleKey, State) ->
    #{ sender       => aec_id:create(account, PubKey)
     , oracle       => OracleKey
     , query        => <<"Hello world">>
     , query_fee    => 5
     , query_ttl    => {delta, maps:get(query, ttl_defaults())}
     , response_ttl => {delta, maps:get(response, ttl_defaults())}
     , fee          => 5
     , nonce        => try next_nonce(PubKey, State) catch _:_ -> 0 end
     }.

%%%===================================================================
%%% Response tx
%%%===================================================================

response_tx(PubKey, ID, Response, State) ->
    response_tx(PubKey, ID, Response, #{}, State).

response_tx(PubKey, ID, Response, Spec0, State) ->
    Spec = maps:merge(response_tx_default_spec(PubKey, ID, Response, State), Spec0),
    {ok, Tx} = aeo_response_tx:new(Spec),
    Tx.

response_tx_default_spec(PubKey, ID, Response, State) ->
    #{ nonce    => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , oracle   => aec_id:create(oracle, PubKey)
     , query_id => ID
     , response => Response
     , fee      => 3
     , ttl      => 0
     }.


%%%===================================================================
%%% Accounts
%%%===================================================================

setup_new_account(State) ->
    setup_new_account(1000, State).

setup_new_account(Balance, State) ->
    {PubKey, PrivKey} = new_key_pair(),
    State1            = insert_key_pair(PubKey, PrivKey, State),
    State2            = set_account(aec_accounts:new(PubKey, Balance), State1),
    {PubKey, State2}.

set_account_balance(PubKey, NewBalance, State) ->
    A        = get_account(PubKey, State),
    Balance  = aec_accounts:balance(A),
    Nonce    = aec_accounts:nonce(A),
    {ok, A1} = aec_accounts:spend(A, Balance, Nonce),
    {ok, A2} = aec_accounts:earn(A1, NewBalance),
    set_account(A2, State).

get_account(PubKey, State) ->
    aec_accounts_trees:get(PubKey, aec_trees:accounts(trees(State))).

set_account(Account, State) ->
    Trees   = trees(State),
    AccTree = aec_accounts_trees:enter(Account, aec_trees:accounts(Trees)),
    set_trees(aec_trees:set_accounts(Trees, AccTree), State).

new_key_pair() ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    {PubKey, PrivKey}.

