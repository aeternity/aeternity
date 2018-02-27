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
        , setup_new_account/3
        , trees/1
        ]).

-include_lib("apps/aeoracle/include/oracle_txs.hrl").

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
    aetx:new(aeo_register_tx,
             #oracle_register_tx{ account   = PubKey
                                , nonce     = maps:get(nonce, Spec)
                                , ttl       = maps:get(ttl, Spec)
                                , fee       = maps:get(fee, Spec)
                                , query_fee = maps:get(query_fee, Spec)
                                }).

register_tx_default_spec(PubKey, State) ->
    #{ ttl       => {delta, 10}
     , fee       => 5
     , nonce     => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , query_fee => 5
     }.

%%%===================================================================
%%% Extend tx
%%%===================================================================

extend_tx(PubKey, State) ->
    extend_tx(PubKey, #{}, State).

extend_tx(PubKey, Spec0, State) ->
    Spec = maps:merge(extend_tx_default_spec(PubKey, State), Spec0),
    aetx:new(aeo_extend_tx,
             #oracle_extend_tx{ oracle = PubKey
                              , nonce  = maps:get(nonce, Spec)
                              , ttl    = maps:get(ttl, Spec)
                              , fee    = maps:get(fee, Spec)
                              }).

extend_tx_default_spec(PubKey, State) ->
    #{ ttl       => {delta, 10}
     , fee       => 5
     , nonce     => try next_nonce(PubKey, State) catch _:_ -> 0 end
     }.

%%%===================================================================
%%% Query tx
%%%===================================================================

query_tx(PubKey, OracleKey, State) ->
    query_tx(PubKey, OracleKey, #{}, State).

query_tx(PubKey, OracleKey, Spec0, State) ->
    Spec = maps:merge(query_tx_default_spec(PubKey, State), Spec0),
    aetx:new(aeo_query_tx,
             #oracle_query_tx{ sender = PubKey
                             , nonce  = maps:get(nonce, Spec)
                             , oracle = OracleKey
                             , query  = maps:get(query, Spec)
                             , query_fee = maps:get(query_fee, Spec)
                             , query_ttl = maps:get(query_ttl, Spec)
                             , response_ttl = maps:get(response_ttl, Spec)
                             , fee = maps:get(fee, Spec)
                             }).

query_tx_default_spec(PubKey, State) ->
    #{ query        => <<"Hello world">>
     , query_fee    => 5
     , query_ttl    => {delta, 50}
     , response_ttl => {delta, 25}
     , fee          => 5
     , nonce        => try next_nonce(PubKey, State) catch _:_ -> 0 end
     }.

%%%===================================================================
%%% Response tx
%%%===================================================================

response_tx(PubKey, ID, Response, State) ->
    response_tx(PubKey, ID, Response, #{}, State).

response_tx(PubKey, ID, Response, Spec0, State) ->
    Spec = maps:merge(response_tx_default_spec(PubKey, State), Spec0),
    aetx:new(aeo_response_tx,
             #oracle_response_tx{ oracle   = PubKey
                                , nonce    = maps:get(nonce, Spec)
                                , query_id = ID
                                , response = Response
                                , fee      = maps:get(fee, Spec)
                                }).

response_tx_default_spec(PubKey, State) ->
    #{ nonce    => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , fee      => 3
     }.


%%%===================================================================
%%% Accounts
%%%===================================================================

setup_new_account(State) ->
    setup_new_account(1000, 1, State).

setup_new_account(Balance, Height, State) ->
    {PubKey, PrivKey} = new_key_pair(),
    State1            = insert_key_pair(PubKey, PrivKey, State),
    State2            = set_account(aec_accounts:new(PubKey, Balance, Height), State1),
    {PubKey, State2}.

set_account_balance(PubKey, NewBalance, State) ->
    A        = get_account(PubKey, State),
    Balance  = aec_accounts:balance(A),
    Height   = aec_accounts:height(A),
    Nonce    = aec_accounts:nonce(A),
    {ok, A1} = aec_accounts:spend(A, Balance, Nonce, Height),
    {ok, A2} = aec_accounts:earn(A1, NewBalance, Height),
    set_account(A2, State).

get_account(PubKey, State) ->
    aec_accounts_trees:get(PubKey, aec_trees:accounts(trees(State))).

set_account(Account, State) ->
    Trees   = trees(State),
    AccTree = aec_accounts_trees:enter(Account, aec_trees:accounts(Trees)),
    set_trees(aec_trees:set_accounts(Trees, AccTree), State).

%%%===================================================================
%%% Keys TODO: Should move
%%%===================================================================

-define(PUB_SIZE, 65).
-define(PRIV_SIZE, 32).

new_key_pair() ->
    {Pubkey, PrivKey} = crypto:generate_key(ecdh, crypto:ec_curve(secp256k1)),
    {Pubkey, pad_privkey(PrivKey)}.

%% crypto:generate_keys/2 gives you a binary with as many bytes as are needed to fit the
%% private key. It does not pad with zeros.

pad_privkey(Bin) ->
    Pad = ?PRIV_SIZE - size(Bin),
    <<0:(Pad*8), Bin/binary>>.
