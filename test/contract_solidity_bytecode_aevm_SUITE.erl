-module(contract_solidity_bytecode_aevm_SUITE).

%% common_test exports
-export(
   [ all/0
   , end_per_testcase/2
   , init_per_testcase/2
   ]).

%% test case exports
-export(
   [
    execute_counter_fun_from_bytecode/1,
    execute_identity_fun_from_solidity_binary/1
   ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(STARTED_APPS_WHITELIST, [{erlexec,"OS Process Manager","1.7.1"}]).
-define(TO_BE_STOPPED_APPS_BLACKLIST, [erlexec, lager]).
-define(REGISTERED_PROCS_WHITELIST,
        [cover_server, timer_server, %% by test framework
         exec_app, exec, %% by erlexec
         inet_gethost_native_sup, inet_gethost_native, %% by inet
         prfTarg,  %% by eper
         dets_sup, dets,  %% by mnesia
         aeu_env_meck %% by test
        ]).

init_per_testcase(TC, Cfg) ->
    lager_common_test_backend:bounce(error),
    Apps = application:which_applications(),
    Names = registered(),
    mock_fast_and_deterministic_cuckoo_pow(),
    {MyPubKey, MyPrivKey} = new_key_pair(),
    Amount = 100000000000000000000,
    Preset = [{MyPubKey, Amount}],

    Code = tc_to_code(TC),

    [ {running_apps, Apps}
    , {regnames, Names}
    , {my_pub_key, MyPubKey}
    , {my_priv_key, MyPrivKey}
    , {preset, Preset}
    , {vm_version, 2} %% AEVM/Solidity
    , {code, Code}
      | Cfg].

tc_to_code(execute_identity_fun_from_solidity_binary) ->
     id_bytecode();
tc_to_code(execute_counter_fun_from_bytecode) ->
     counter_bytecode().

end_per_testcase(_TC, Config) ->
    Apps0 = ?config(running_apps, Config),
    Names0 = ?config(regnames, Config),
    Apps = application:which_applications() -- ?STARTED_APPS_WHITELIST,
    Names = registered() -- ?REGISTERED_PROCS_WHITELIST,
    case {(Apps -- Apps0), Names -- Names0, lager_common_test_backend:get_logs()} of
        {[], [], []} ->
            ok;
        {_, _, Logs} when Logs =/= []->
            {fail, {errors_in_lager_log, lists:map(fun iolist_to_s/1, Logs)}};
        {NewApps, _, _} when NewApps =/= [] ->
            %% New applications take precedence over new registered processes
            {fail, {started_applications, NewApps}};
        {_, NewReg, _} ->
            await_registered(NewReg, Names0)
    end.

await_registered(Rest, Names0) ->
    receive after 100 ->
                    await_registered(9, Rest, Names0)
            end.

await_registered(N, _, Names0) when N > 0 ->
    case (registered() -- Names0) -- ?REGISTERED_PROCS_WHITELIST of
        [] ->
            ok;
        [_|_] = NewReg ->
            receive after 100 ->
                            await_registered(N-1, NewReg, Names0)
                    end
    end;
await_registered(_, NewReg, _Names0) ->
    {fail, {registered_processes, NewReg}}.

-spec iolist_to_s(iolist()) -> string().
iolist_to_s(L) ->
    lists:flatten(io_lib:format("~s~n", [L])).


%% ------------------------------------------------------------------------
%% Test cases
%% ------------------------------------------------------------------------

all() -> [
          %% This test works when running as a standalone test,
          %% but not with "make test" where other tests have run before.
          %% Taken out of test suite for now.
          %% TODO: Turn into a "dev1" node test.
          %% execute_counter_fun_from_bytecode,
          %% execute_identity_fun_from_solidity_binary
         ].

execute_identity_fun_from_solidity_binary(Cfg) ->
    {ok, StartedApps, TempDir} = prepare_app_start(aecore, Cfg),
    ok = mock_genesis(Cfg),
    {ok,_} = application:ensure_all_started(aecore),

    Tx = create_tx(#{}, Cfg),
    PrivKey = ?config(my_priv_key, Cfg),
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    ok = aec_tx_pool:push(SignedTx),
    TxHash = aetx_sign:hash(SignedTx),

    wait_for_it(fun () ->
    			none =/=
    			    aec_chain:find_tx_with_location(TxHash)
     		end, true),

    %% lager:error("TxBin ~w~n",[TxHash]),

    wait_for_it(fun () ->
			case aec_chain:find_tx_with_location(TxHash) of
			    'none' -> false;
			    {'mempool', _} -> false;
			    {_Bin, _TX} -> true
			end
     		end, true),

    {_Block, SignedTx} = aec_chain:find_tx_with_location(TxHash),

    ok = unmock_genesis(Cfg),
    ok = application:stop(aecore),
    ok = app_stop(StartedApps -- ?TO_BE_STOPPED_APPS_BLACKLIST, TempDir),
    ok.


id_bytecode() ->
    aeu_hex:hexstring_decode(
      <<"0x6060604052341561000f57600080fd5b60ae8061001d6000396000f300606060"
        "405260043610603f576000357c0100000000000000000000000000000000000000"
        "000000000000000000900463ffffffff1680631a94d83e146044575b600080fd5b"
        "3415604e57600080fd5b606260048080359060200190919050506078565b604051"
        "8082815260200191505060405180910390f35b60008190509190505600a165627a"
        "7a723058205cc378b9229138b9feea0e5d1a4c82df2ff3e18e9db005d866e7158b"
        "e405cbf70029">>).

execute_counter_fun_from_bytecode(Cfg) ->
    application:stop(mnesia),
    application:stop(aec_conductor),
    application:stop(aecore),
    {ok, StartedApps, TempDir} = prepare_app_start(aecore, Cfg),
    ok = mock_genesis(Cfg),
    {ok,_} = application:ensure_all_started(aecore),

    PrivKey = ?config(my_priv_key, Cfg),
    Tx = create_tx(#{}, Cfg),
    OwnerPubKey = ?config(my_pub_key, Cfg),
    Nonce = 1, %% aect_create_tx:nonce(Tx),
    ContractPubKey = aect_contracts:compute_contract_pubkey(OwnerPubKey, Nonce),


    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    ok = aec_tx_pool:push(SignedTx),
    TxHash = aetx_sign:hash(SignedTx),

    wait_for_it(fun () ->
                        none =/=
                            aec_chain:find_tx_with_location(TxHash)
                end, true),

    %% lager:error("TxBin ~w~n",[TxHash]),

    wait_for_it(fun () ->
                        case aec_chain:find_tx_with_location(TxHash) of
                            'none' -> false;
                            {'mempool', _} -> false;
                            {_Bin, _TX} -> true
                        end
                end, true),

    {ok, Contract} = aec_chain:get_contract(ContractPubKey),
    ct:pal("~p~n",[Contract]),
    ct:pal("~p~n",[aect_contracts:state(Contract)]),
    #{} = aect_contracts:state(Contract),
    %% Call Counter:inc(1)
    CallData =  aeu_hex:hexstring_decode(<< "0x6d4ce63c" >>),
    Spec = #{ caller     => aec_id:create(account, OwnerPubKey)
            , nonce      => 2
            , contract   => aec_id:create(contract, ContractPubKey)
            , vm_version => 2
            , fee        => 1
            , amount     => 0
            , gas        => 500
            , gas_price  => 1
            , call_data  => CallData
            },
    {ok, CallTx} = aect_call_tx:new(Spec),
    SignedCallTx = aec_test_utils:sign_tx(CallTx, PrivKey),
    ok = aec_tx_pool:push(SignedCallTx),
    CallTxHash = aetx_sign:hash(SignedCallTx),
    wait_for_it(fun () ->
                        case aec_chain:find_tx_with_location(CallTxHash) of
                            'none' -> false;
                            {'mempool', _} -> false;
                            {_, _} -> true
                        end
                end, true),
    {ok, Contract2} = aec_chain:get_contract(ContractPubKey),
    ct:pal("~p~n",[Contract2]),
    ct:pal("~p~n",[aect_contracts:state(Contract2)]),

    ok = unmock_genesis(Cfg),
    ok = application:stop(aecore),
    ok = app_stop(StartedApps -- ?TO_BE_STOPPED_APPS_BLACKLIST, TempDir),
    ok.


counter_bytecode() ->
    %% pragma solidity ^0.4.0;
    %% contract Counter {
    %%   uint32 public value;
    %%
    %%    constructor() public {
    %%        value = 0;
    %%    }
    %%
    %%    function inc(uint32 x) public {
    %%       value = value + x;
    %%      }
    %%
    %%     function get() public constant returns (uint32) {
    %%         return value;
    %%     }
    %%  }


    aeu_hex:hexstring_decode(
      <<"0x608060405234801561001057600080fd5b5060008060006101000a81548163"
        "ffffffff021916908363ffffffff16021790555061018d8061004160003960"
        "00f300608060405260043610610057576000357c0100000000000000000000"
        "000000000000000000000000000000000000900463ffffffff1680633fa4f2"
        "451461005c5780636d4ce63c14610093578063dd5d5211146100ca575b6000"
        "80fd5b34801561006857600080fd5b506100716100fd565b604051808263ff"
        "ffffff1663ffffffff16815260200191505060405180910390f35b34801561"
        "009f57600080fd5b506100a8610112565b604051808263ffffffff1663ffff"
        "ffff16815260200191505060405180910390f35b3480156100d657600080fd"
        "5b506100fb600480360381019080803563ffffffff16906020019092919050"
        "505061012b565b005b6000809054906101000a900463ffffffff1681565b60"
        "008060009054906101000a900463ffffffff16905090565b80600080905490"
        "6101000a900463ffffffff16016000806101000a81548163ffffffff021916"
        "908363ffffffff160217905550505600a165627a7a72305820d465419d8b4c"
        "7adf48551bcf6e438080d2c45e27489fc706002b4c638d420ebd0029">>).



%% ------------------------------------------------------------------------
%% Helper Functions
%% ------------------------------------------------------------------------

%%%===================================================================
%%% Keys TODO: Should move
%%%===================================================================
new_key_pair() ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    {PubKey, PrivKey}.

%%%===================================================================
%%% Accounts in Genesis
%%%===================================================================


mock_genesis(Cfg) ->
    Preset = ?config(preset, Cfg),
    meck:new(aec_genesis_block_settings, []),
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, Preset),
    ok.

unmock_genesis(_Cfg) ->
    meck:unload(aec_genesis_block_settings),
    ok.

create_tx(Override, Cfg) ->
    PubKey = ?config(my_pub_key, Cfg),
    Code = ?config(code, Cfg),
    VmVersion = ?config(vm_version, Cfg),
    Map = #{ owner      => aec_id:create(account, PubKey)
           , nonce      => 1
           , code       => Code
           , vm_version => VmVersion
           , fee        => 10
           , deposit    => 100
           , amount     => 0
           , gas        => 1000000000000000000
           , gas_price  => 5
           , call_data  => <<"0">>
           },
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aect_create_tx:new(Map1),
    Tx.



prepare_app_start(App, Config) ->
    try prepare_app_start_(App, Config)
    catch
        error:Reason ->
            error({Reason, erlang:get_stacktrace()})
    end.

prepare_app_start_(App, Config) ->
    application:load(App),
    TempDir = create_temp_key_dir(),
    application:set_env(aecore, keys_dir, TempDir),
    application:set_env(aecore, password, <<"secret">>),

    {ok, Deps0} = application:get_key(App, applications),
    Deps = maybe_add_mnesia(App, Deps0), % mnesia is started manually in aecore_app
    AlreadyRunning = [ Name || {Name, _,_} <- proplists:get_value(running_apps, Config) ],
    [ ok = application:ensure_started(Dep) || Dep <- Deps ],
    {ok, lists:reverse(Deps -- AlreadyRunning), TempDir}.

app_stop(Apps, TempDir) ->
    remove_temp_key_dir(TempDir),
    [ application:stop(App) || App <- Apps ],
    ok.

maybe_add_mnesia(App, Deps) ->
    case lists:member(aecore, [App|Deps]) of
        true  -> Deps ++ [mnesia];
        false -> Deps
    end.

remove_temp_key_dir(TmpKeysDir) ->
    {ok, KeyFiles} = file:list_dir(TmpKeysDir),
    %% Expect four filenames - private and public keys x2.
    [_KF1, _KF2, _KF3, _KF4] = KeyFiles,
    lists:foreach(
      fun(F) ->
              AbsF = filename:absname_join(TmpKeysDir, F),
              {ok, _} = {file:delete(AbsF), {F, AbsF}}
      end,
      KeyFiles),
    ok = file:del_dir(TmpKeysDir).

create_temp_key_dir() ->
    mktempd(os:type()).

mktempd({unix, _}) ->
    lib:nonl(?cmd("mktemp -d")).

wait_for_it(Fun, Value) ->
    wait_for_it(Fun, Value, 0).

wait_for_it(Fun, Value, Sleep) ->
    case Fun() of
        Value ->
            Value;
        _Other ->
            %% ?ifDebugFmt("Waiting for ~p got ~p~n",[Value,_Other]),
            timer:sleep(Sleep),
            wait_for_it(Fun, Value, Sleep + 10)
    end.

mock_fast_and_deterministic_cuckoo_pow() ->
    mock_fast_cuckoo_pow({"mean16s-generic", "", 16, false}).

mock_fast_cuckoo_pow({_MinerBin, _MinerExtraArgs, _NodeBits} = Cfg) ->
    meck:expect(aeu_env, get_env, 3,
                fun
                    (aecore, aec_pow_cuckoo, _) ->
                       Cfg;
                    (App, Key, Def) ->
                       meck:passthrough([App, Key, Def])
               end).
