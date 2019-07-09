%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_dry_run).

-export([dry_run/3]).

-include("blocks.hrl").

dry_run(TopHash, Accounts, Txs) ->
    try
        {Env, Trees} = aetx_env:tx_env_and_trees_from_hash('aetx_transaction', TopHash),
        Trees1 = add_accounts(Trees, Accounts),
        dry_run_(Txs, Trees1, Env)
    catch _E:_R ->
        {error, <<"Failed to get block state and environment">>}
    end.

dry_run_(Txs, Trees, Env) ->
    STxs = dummy_sign_txs(Txs),
    {ok, dry_run(STxs, Trees, Env, [])}.

dry_run([], _Trees, _Env, Acc) ->
    lists:reverse(Acc);
dry_run([Tx | Txs], Trees, Env, Acc) ->
    case aec_trees:apply_txs_on_state_trees([Tx], Trees, Env,
                                            [strict, dont_verify_signature]) of
        {ok, [Tx], [], Trees1, _Env1} ->
            dry_run(Txs, Trees1, Env, [dry_run_res(Tx, Trees1, ok) | Acc]);
        Err = {error, _Reason} ->
            dry_run(Txs, Trees, Env, [dry_run_res(Tx, Trees, Err) | Acc])
    end.

dry_run_res(STx, Trees, ok) ->
    Tx = aetx_sign:tx(STx),
    {Type, _} = aetx:specialize_type(Tx),
    case Type of
        _ when Type =:= contract_call_tx;
               Type =:= contract_create_tx ->
            {CB, CTx} = aetx:specialize_callback(Tx),
            Contract  = CB:contract_pubkey(CTx),
            CallId    = CB:call_id(CTx),
            CallTree = aec_trees:calls(Trees),
            {value, CallObj} = aect_call_state_tree:lookup_call(Contract, CallId, CallTree),
            {Type, {ok, CallObj}};
        spend_tx ->
            {Type, ok}
    end;
dry_run_res(STx, _Trees, Err) ->
    {Type, _} = aetx:specialize_type(aetx_sign:tx(STx)),
    {Type, Err}.

add_accounts(Trees, []) ->
    Trees;
add_accounts(Trees, [#{pub_key := PK, amount := A} | Accounts]) ->
    AccountsTree     = aec_trees:accounts(Trees),
    {value, Account} = aec_accounts_trees:lookup(PK, AccountsTree),
    {ok, Account1}   = aec_accounts:earn(Account, A),
    AccountsTree1    = aec_accounts_trees:enter(Account1, AccountsTree),
    add_accounts(aec_trees:set_accounts(Trees, AccountsTree1), Accounts).

dummy_sign_txs(Txs) ->
    [ aetx_sign:new(Tx, [dummy_sign()]) || Tx <- Txs ].

dummy_sign() ->
    <<0:(?BLOCK_SIGNATURE_BYTES*8)>>.
