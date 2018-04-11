-module(contract_solidity_bytecode_aevm_SUITE).

%% common_test exports
-export(
   [
    all/0
   ]).

%% test case exports
-export(
   [
     execute_identity_fun_from_solidity_binary/1
   ]).

-include_lib("common_test/include/ct.hrl").

all() -> [ execute_identity_fun_from_solidity_binary ].

execute_identity_fun_from_solidity_binary(_Cfg) ->
    ContractBin} = id_bytecode(),
    CallData = aect_evm:create_call(Code, <<"init">>, <<"42">>),
    {ok, #{ out := RetVal}} =
        aect_evm:execute_call(
          #{ code => Code,
             address => 0,
             caller => 0,
             data => CallData,
             gas => 1000000,
             gasPrice => 1,
             origin => 0,
             value => 0,
             currentCoinbase => 0,
             currentDifficulty => 0,
             currentGasLimit => 10000,
             currentNumber => 0,
             currentTimestamp => 0,
             chainState => aevm_dummy_chain:new_state(),
             chainAPI => aevm_dummy_chain},
          true),
    <<42:256>> = RetVal,
    ok.

id_bytecode() ->
    <<"0x6060604052341561000f57600080fd5b60ae8061001d6000396000f300606060"
      "405260043610603f576000357c0100000000000000000000000000000000000000"
      "000000000000000000900463ffffffff1680631a94d83e146044575b600080fd5b"
      "3415604e57600080fd5b606260048080359060200190919050506078565b604051"
      "8082815260200191505060405180910390f35b60008190509190505600a165627a"
      "7a723058205cc378b9229138b9feea0e5d1a4c82df2ff3e18e9db005d866e7158b"
      "e405cbf70029">>.

create_tx(Override) ->
    Map = #{ owner      => <<4711:65/unit:8>>
           , nonce      => 42
           , code       => <<"THIS IS NOT ACTUALLY PROPER BYTE CODE">>
           , vm_version => 1
           , fee        => 10
           , deposit    => 100
           , amount     => 50
           , gas        => 100
           , gas_price  => 5
           , call_data  => <<"NOT ENCODED ACCORDING TO ABI">>
           },
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aect_create_tx:new(Map1),
    {contract_create_tx, CTx} = aetx:specialize_type(Tx),
    CTx.


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
