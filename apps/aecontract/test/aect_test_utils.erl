%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Test utils for contracts
%%% @end
%%%-------------------------------------------------------------------
-module(aect_test_utils).

-export([ new_state/0
        , contracts/1
        , set_contracts/2
        , priv_key/2
        , call_tx/3
        , call_tx/4
        , create_tx/2
        , create_tx/3
        , set_account_balance/3
        , set_trees/2
        , setup_new_account/1
        , setup_new_account/3
        , get_account/2
        , trees/1
        , compile_contract/1
        ]).

-include_lib("apps/aecontract/include/contract_txs.hrl").

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

contracts(State) ->
    aec_trees:contracts(trees(State)).

set_contracts(Contracts, State) ->
    Trees = trees(State),
    set_trees(aec_trees:set_contracts(Trees, Contracts), State).

%%%===================================================================
%%% Register tx
%%%===================================================================

create_tx(PubKey, State) ->
    create_tx(PubKey, #{}, State).

create_tx(PubKey, Spec0, State) ->
    Spec = maps:merge(create_tx_default_spec(PubKey, State), Spec0),
    aetx:new(aect_create_tx,
             #contract_create_tx{ owner      = PubKey
                                , nonce      = maps:get(nonce, Spec)
                                , fee        = maps:get(fee, Spec)
                                , code       = maps:get(code, Spec)
                                , vm_version = maps:get(vm_version, Spec)
                                , deposit    = maps:get(deposit, Spec)
                                , amount     = maps:get(amount, Spec)
                                , gas        = maps:get(gas, Spec)
                                , gas_price  = maps:get(gas_price, Spec)
                                , call_data  = maps:get(call_data, Spec)
                                }).

create_tx_default_spec(PubKey, State) ->
    #{ fee        => 5
     , nonce      => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , code       => <<"NOT PROPER BYTE CODE">>
     , vm_version => 0
     , deposit    => 10
     , amount     => 200
     , gas        => 10
     , gas_price  => 1
     , call_data  => <<"NOT ENCODED ACCORDING TO ABI">>
     }.

%%%===================================================================
%%% Call tx
%%%===================================================================

call_tx(PubKey, ContractKey, State) ->
    call_tx(PubKey, ContractKey, #{}, State).

call_tx(PubKey, ContractKey, Spec0, State) ->
    Spec = maps:merge(call_tx_default_spec(PubKey, State), Spec0),
    aetx:new(aect_call_tx,
             #contract_call_tx{ caller     = PubKey
                              , nonce      = maps:get(nonce, Spec)
                              , contract   = ContractKey
                              , vm_version = maps:get(vm_version, Spec)
                              , fee        = maps:get(fee, Spec)
                              , amount     = maps:get(amount, Spec)
                              , gas        = maps:get(gas, Spec)
                              , gas_price  = maps:get(gas_price, Spec)
                              , call_data  = maps:get(call_data, Spec)
                              }).

call_tx_default_spec(PubKey, State) ->
    #{ fee         => 5
     , nonce       => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , vm_version  => 0
     , amount      => 100
     , gas         => 10000
     , gas_price   => 1
     , call_data   => <<"CALL DATA">>
     }.

%%%===================================================================
%%% Accounts
%%%===================================================================

setup_new_account(State) ->
    setup_new_account(100000, 1, State).

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

compile_contract(File) ->
    CodeDir = code:lib_dir(aering, test),
    FileName = filename:join(CodeDir, File),
    {ok, ContractBin} = file:read_file(FileName),
    Contract = binary_to_list(ContractBin),
    aer_compiler:from_string(Contract, [pp_icode, pp_assembler, pp_bytecode]).

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
