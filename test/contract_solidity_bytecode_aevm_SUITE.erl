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
     execute_identity_fun_from_solidity_binary/1
   ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(STARTED_APPS_WHITELIST, [{erlexec,"OS Process Manager","1.7.1"}]).
-define(TO_BE_STOPPED_APPS_BLACKLIST, [erlexec]).
-define(REGISTERED_PROCS_WHITELIST,
        [cover_server, timer_server, %% by test framework
         exec_app, exec, %% by erlexec
         inet_gethost_native_sup, inet_gethost_native, %% by inet
         prfTarg,  %% by eper
         dets_sup, dets,  %% by mnesia
	 aeu_env_meck %% by test
        ]).

init_per_testcase(_, Cfg) ->
    lager_common_test_backend:bounce(error),
    Apps = application:which_applications(),
    Names = registered(),
    mock_fast_and_deterministic_cuckoo_pow(),
    {MyPubKey, MyPrivKey} = new_key_pair(),
    Amount = 100000000000000000000,
    Preset = [{MyPubKey, Amount}],

    [ {running_apps, Apps}
    , {regnames, Names}    
    , {my_pub_key, MyPubKey}
    , {my_priv_key, MyPrivKey}
    , {preset, Preset}
    , {vm_version, 2} %% AEVM/Solidity
    , {code, id_bytecode()}
      | Cfg].

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

all() -> [ execute_identity_fun_from_solidity_binary ].

execute_identity_fun_from_solidity_binary(Cfg) ->
    {ok, StartedApps, TempDir} = prepare_app_start(aecore, Cfg),

    ok = mock_genesis(Cfg),
    ok = application:start(aecore),
    Tx = create_tx(#{}, Cfg),
    PrivKey = ?config(my_priv_key, Cfg),
    SignedTx = aetx_sign:sign(Tx, PrivKey),
    ok = aec_tx_pool:push(SignedTx),
    TxHash = aetx:hash(aetx_sign:tx(SignedTx)),

    wait_for_it(fun () ->  
    			none =/= 
    			    aec_chain:find_transaction_in_main_chain_or_mempool(TxHash)
     		end, true),

    %% lager:error("TxBin ~w~n",[TxHash]),    

    wait_for_it(fun () ->  
			case aec_chain:find_transaction_in_main_chain_or_mempool(TxHash) of
			    'none' -> false;
			    {'mempool', _} -> false;
			    {_Bin, _TX} -> true
			end
     		end, true),


    {Block, SignedTx} = aec_chain:find_transaction_in_main_chain_or_mempool(TxHash),
    %% lager:error("Res ~w~n",[Res]),

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



%% ------------------------------------------------------------------------
%% Helper Functions
%% ------------------------------------------------------------------------


%%%===================================================================
%%% Keys TODO: Should move
%%%===================================================================

-define(PUB_SIZE, 65).
-define(PRIV_SIZE, 32).

new_key_pair() ->
    {Pubkey, PrivKey} = crypto:generate_key(ecdh, crypto:ec_curve(secp256k1)),
    {Pubkey, pad_privkey(PrivKey)}.

%% crypto:generate_keys/2 gives you a binary with as many bytes as are needed
%%  to fit the private key. It does not pad with zeros.

pad_privkey(Bin) ->
    Pad = ?PRIV_SIZE - size(Bin),
    <<0:(Pad*8), Bin/binary>>.

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
    Map = #{ owner      => PubKey
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
    mock_fast_cuckoo_pow({"mean16s-generic", "", 16}).

mock_fast_cuckoo_pow({_MinerBin, _MinerExtraArgs, _NodeBits} = Cfg) ->
    meck:expect(aeu_env, get_env, 3,
                fun
                    (aecore, aec_pow_cuckoo, _) ->
                       Cfg;
                    (App, Key, Def) ->
                       meck:passthrough([App, Key, Def])
               end).
