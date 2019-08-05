%%
%% In the first test a lot of code is loaded, therefore the times of the first test
%% should be completely ignored
%%
%% We measure what a simple spend transaction takes in time. We can roughly put 300
%% spend transactions in a micro block and 100 spend should take less than a second.
%%
%%
-module(measure_gas_SUITE).

%% common_test exports
-export(
   [ all/0, groups/0, init_per_suite/1, end_per_suite/1 ]).

%% test case exports
-export(
   [ create_identity_contract_test/1,
     create_multiple_identity_contracts_test/1,
     sort_contract_test/1,
     arithm_contract_test/1
   ]).

-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("common_test/include/ct.hrl").

-define(SAMPLE, 100).

all() ->
    [ create_identity_contract_test,
      create_multiple_identity_contracts_test,
      arithm_contract_test,
      sort_contract_test
    ].

groups() ->
    [ {all_tests, [sequence],
        [ {group, identity}
        , {group, pow}
        , {group, sort}
        ]}
    , {identity, [sequence],
        [ create_identity_contract_test, create_multiple_identity_contracts_test ]}
    , {pow, [sequence],
       [ create_identity_contract_test, arithm_contract_test ]}
    , {sort, [sequence],
       [ create_identity_contract_test, sort_contract_test]}
    ].

init_per_suite(Cfg) ->
    case aect_test_utils:latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN -> {skip, no_fate_in_roma};
        ?MINERVA_PROTOCOL_VSN -> {skip, no_fate_in_minerva};
        ?FORTUNA_PROTOCOL_VSN -> {skip, no_fate_in_fortuna};
        ?LIMA_PROTOCOL_VSN -> Cfg
    end.

end_per_suite(_Cfg) ->
    ok.


create_identity_contract_test(_Cfg) ->
    Name = "identity.aes",
    {Aevm, Fate} = compile(Name),

    {Trees, Env}   = init(),
    {Trees1, Env1} = create_contract(1, Trees, Env, [{fate, Fate, 2, undefined}, {aevm, Aevm, 1, undefined}], "init", []),
    [ ct:log("Account ~p now ~p (total cost: ~p)", [Account, balance(Trees1, Account),
                                                    balance_dec(Trees, Trees1, Account)])
      || Account <- [1,2] ],

    ct:log("Fate cheaper than AEVM: ~p per create", [balance_diff(Trees1, 2, 1)]),

    {Trees2, _Env2, _Calls} = call_contract(1, Trees1, Env1, [{fate, Fate, 2, id(2,1)}, {aevm, Aevm, 1, id(1,1)}], "main", ["42"]),
    [ ct:log("Account ~p now ~p (total cost: ~p)", [Account, balance(Trees2, Account),
                                                    balance_dec(Trees1, Trees2, Account)])
      || Account <- [1,2] ],

    ct:log("Fate cheaper than AEVM: ~p per call", [balance_dec(Trees1, Trees2, 1) - balance_dec(Trees1, Trees2, 2)]),

    {_Trees3, _Env3} = mine(1, Trees2),
    ok.


create_multiple_identity_contracts_test(_Cfg) ->
    Name = "identity.aes",
    {Aevm, Fate} = compile(Name),
    Sample = 100,

    {Trees, Env}   = init(),
    {Trees1, Env1} = create_contract(Sample, Trees, Env, [{fate, Fate, 2, undefined}, {aevm, Aevm, 1, undefined}], "init", []),

    {Trees2, _Env2, _Calls} = call_contract(Sample, Trees1, Env1, [{fate, Fate, 4, id(2,2)}, {aevm, Aevm, 3, id(1,2)}], "main", ["42"]),

    {_Trees3, _Env3} = mine(1, Trees2),
    ok.

arithm_contract_test(_Cfg) ->
    Name = "arithm.aes",
    {Aevm, Fate} = compile(Name),
    Sample = 10,

    {Trees, Env}   = init(),
    {Trees1, Env1} = create_contract(Sample, Trees, Env, [{fate, Fate, 2, undefined}, {aevm, Aevm, 1, undefined}], "init", ["100"]),
    {Trees2, Env2} = create_contract(Sample, Trees1, Env1, [{fate, Fate, 2, undefined}, {aevm, Aevm, 1, undefined}], "init", ["10"]),

    ct:log("100^10"),
    {Trees3, _Env3, _Calls} = call_contract(Sample, Trees2, Env2, [{fate, Fate, 4, id(2,1)}, {aevm, Aevm, 3, id(1,1)}], "pow", ["10"]),
    {Trees4, Env4} = mine(1, Trees3),
    ct:log("10^50"),
    {Trees5, Env5, _} = call_contract(Sample, Trees4, Env4, [{fate, Fate, 4, id(2, Sample+1)}, {aevm, Aevm, 3, id(1, Sample+1)}], "pow", ["50"]),
    ct:log("10^60"),
    {Trees6, Env6, _} = call_contract(Sample, Trees5, Env5, [{fate, Fate, 4, id(2, Sample+1)}, {aevm, Aevm, 3, id(1, Sample+1)}], "pow", ["60"]),
    ct:log("10^70"),
    {Trees7, Env7, _} = call_contract(Sample, Trees6, Env6, [{fate, Fate, 4, id(2, Sample+1)}, {aevm, Aevm, 3, id(1, Sample+1)}], "pow", ["70"]),
    ct:log("10^80"),
    {Trees8, Env8, _} = call_contract(Sample, Trees7, Env7, [{fate, Fate, 4, id(2, Sample+1)}, {aevm, Aevm, 3, id(1, Sample+1)}], "pow", ["80"]),
    ct:log("10^90"),
    {Trees9, Env9, _} = call_contract(Sample, Trees8, Env8, [{fate, Fate, 4, id(2, Sample+1)}, {aevm, Aevm, 3, id(1, Sample+1)}], "pow", ["90"]),
    ct:log("10^100"),
    {_Trees10, _Env10, _} = call_contract(Sample, Trees9, Env9, [{fate, Fate, 4, id(2, Sample+1)}, {aevm, Aevm, 3, id(1, Sample+1)}], "pow", ["100"]),

    ok.


sort_contract_test(_Cfg) ->
    Name = "sorting.aes",
    {Aevm, Fate} = compile(Name),
    Sample = 1,

    ct:log("init with list in state"),
    {Trees, Env}   = init(),
    {Trees1, Env1} = create_contract(Sample, Trees, Env, [{fate, Fate, 2, undefined}, {aevm, Aevm, 1, undefined}], "init", ["[2,4,1,5,3]"]),
    {Trees2, Env2} = create_contract(Sample, Trees1, Env1, [{fate, Fate, 2, undefined}, {aevm, Aevm, 1, undefined}], "init", ["[2,4,1,5,3,1,2,6,7,1]"]),
    {Trees3, Env3} = create_contract(Sample, Trees2, Env2, [{fate, Fate, 2, undefined}, {aevm, Aevm, 1, undefined}], "init",
                                     ["[20,19,18,17,16,15,14,13,12,11,10,9,8,7,2,3,4,5,1,6]"]),
    {Trees4, Env4} = create_contract(Sample, Trees3, Env3, [{fate, Fate, 2, undefined}, {aevm, Aevm, 1, undefined}], "init",
                                     ["[20,19,18,17,16,15,14,13,12,11,10,40,39,38,37,36,35,34,33,32,9,31,8,7,2,3,4,5,1,6,30,29,28,27,26,25,24,23,22,21]"]),

    ct:log("sorting an argument list"),
    {Trees5, Env5, _} = call_contract(Sample, Trees4, Env4, [{fate, Fate, 4, id(2,1)}, {aevm, Aevm, 3, id(1,1)}], "sort", ["[2,4,1,5,3]"]),
    {Trees6, Env6, _} = call_contract(Sample, Trees5, Env5, [{fate, Fate, 4, id(2,1)}, {aevm, Aevm, 3, id(1,1)}], "sort", ["[2,4,1,5,3,1,2,6,7,1]"]),
    {Trees7, Env7, _} = call_contract(Sample, Trees6, Env6, [{fate, Fate, 4, id(2,1)}, {aevm, Aevm, 3, id(1,1)}], "sort",
                                      ["[20,19,18,17,16,15,14,13,12,11,10,9,8,7,2,3,4,5,1,6]"]),
    {Trees8, Env8, _} = call_contract(Sample, Trees7, Env7, [{fate, Fate, 4, id(2,1)}, {aevm, Aevm, 3, id(1,1)}], "sort",
                                      ["[20,19,18,17,16,15,14,13,12,11,10,40,39,38,37,36,35,34,33,32,9,31,8,7,2,3,4,5,1,6,30,29,28,27,26,25,24,23,22,21]"]),

    ct:log("sorting the state"),
    {Trees9, Env9, _} = call_contract(Sample, Trees8, Env8, [{fate, Fate, 4, id(2,1)}, {aevm, Aevm, 3, id(1,1)}], "state_sort", []),
    {Trees10, Env10, _} = call_contract(Sample, Trees9, Env9, [{fate, Fate, 4, id(2,2)}, {aevm, Aevm, 3, id(1,2)}], "state_sort", []),
    {Trees11, Env11, _} = call_contract(Sample, Trees10, Env10, [{fate, Fate, 4, id(2,3)}, {aevm, Aevm, 3, id(1,3)}], "state_sort", []),
    {_Trees12, _Env12, _} = call_contract(Sample, Trees11, Env11, [{fate, Fate, 4, id(2,4)}, {aevm, Aevm, 3, id(1,4)}], "state_sort", []),

    ok.



create_contract(_Sample, Trees, Env, [], _InitFun, _InitArgs) ->
    {Trees, Env};
create_contract(Sample, Trees, Env, [{Backend, Code, Account, _} | VMs], InitFun, InitArgs) ->
    {Trees1, Env1} = create_contract(Sample, Trees, Env, VMs, InitFun, InitArgs),

    {ok, Trees2, Env2} = tx_timer(Sample, Backend, Trees1, Env1,
                                  fun(T) -> contract_create(T, account(Account),
                                                            Code, InitFun, InitArgs, Backend) end),


    NewCalls = calls_produced(Trees1, Trees2, Backend),
    ct:log("~p calls ~p", [Backend, NewCalls]),

    {Trees2, Env2}.

call_contract(_Sample, Trees, Env, [], _Fun, _Args) ->
    {Trees, Env, []};
call_contract(Sample, Trees, Env, [{Backend, Code, Account, PK} | VMs], Fun, Args) ->
    {Trees1, Env1, Calls} = call_contract(Sample, Trees, Env, VMs, Fun, Args),

    {ok, Trees2, Env2} = tx_timer(Sample, Backend, Trees1, Env1,
                                  fun(T) -> contract_call(T, account(Account), PK,
                                                          Code, Fun, Args, Backend)
                                  end),

    NewCalls = calls_produced(Trees1, Trees2, Backend),
    ct:log("~p calls ~p", [Backend, NewCalls]),

    {Trees2, Env2, [{Backend, NewCalls} | Calls]}.


compile(File) ->
    CodeDir = filename:join(code:lib_dir(aecore), "../../extras/test/contracts"),
    FileName = filename:join(CodeDir, File),
    {ok, Cwd} = file:get_cwd(),
    {ok, Aevm} = aeso_compiler:file(FileName, [{backend, aevm}, {include, {file_system, [Cwd, CodeDir]}}]),
    {ok, Fate} =  aeso_compiler:file(FileName, [{backend, fate}, {include, {file_system, [Cwd, CodeDir]}}]),
    ct:pal("Size aevm: ~p\n     fate: ~p\n", [byte_size(aect_sophia:serialize(Aevm)),
                                              byte_size(aect_sophia:serialize(Fate))]),
    {Aevm, Fate}.

init() ->
    init([{account, account(N), 20000000000000000} || N<-lists:seq(1,9)]).

init(Accounts) ->
    mine(0, trees_with_accounts(Accounts)).

%% Mine at height Height
mine(Height, Trees) ->
    Vsn = aec_hard_forks:protocol_effective_at_height(Height),
    NextH = Height + 1,
    NextVsn = aec_hard_forks:protocol_effective_at_height(NextH),
    Trees1 = aec_trees:perform_pre_transformations(Trees, Vsn, NextVsn, NextH),
    {Trees1, aetx_env:tx_env(NextH)}.

contract_create(Trees, Sender, CompiledContract, Init, Args, Backend) ->
    #{contract_source := Contract} = CompiledContract,
    {ok, CallData} = encode_call_data(Contract, Init, Args, Backend),
    Tx =
        #{owner_id => aeser_id:create(account, Sender),
          vm_version  => case Backend of
                           aevm -> aect_test_utils:latest_sophia_vm_version();
                           fate -> ?VM_FATE_SOPHIA_1
                         end,
          abi_version => case Backend of aevm -> 1; fate -> 3 end,
          fee => 100000 * 1500000 * 20,
          gas_price => 1000000,
          gas => 20000000,
          nonce => nonce(Trees, Sender),
          deposit => 0,
          amount => 0,
          code => aect_sophia:serialize(CompiledContract),
          call_data => CallData   %% Check this for Fate!
         },
    {ok, AeTx} = aect_create_tx:new(Tx),
    AeTx.

contract_call(Trees, Sender, ContractId, CompiledContract, Fun, Args, Backend) ->
    #{contract_source := Contract} = CompiledContract,
    {ok, CallData} = encode_call_data(Contract, Fun, Args, Backend),
    Tx =
        #{caller_id => aeser_id:create(account, Sender),
          contract_id => aeser_id:create(contract, ContractId),
          abi_version => case Backend of aevm -> ?ABI_AEVM_SOPHIA_1; fate -> ?ABI_FATE_SOPHIA_1 end,
          fee =>  500000 * 1000000 * 20,
          gas_price => 1000000,
          gas => 20000000,
          nonce => nonce(Trees, Sender),
          amount => 0,
          call_data => CallData
         },
    {ok, AeTx} = aect_call_tx:new(Tx),
    AeTx.


encode_call_data(Code, Fun, Args, Backend) ->
    aeso_compiler:create_calldata(Code, Fun, Args, [{backend, Backend}]).

tx_timer(N, Backend, Trees, Env, F) ->
    {NewTrees, NewEnv, AllTimes} =
        lists:foldl(fun(_I, {Ts, E, Times}) ->
                            AeTx = F(Ts),
                            {Time, {ok, NewTs, NewE}} = timer:tc(aetx, process, [AeTx, Ts, E]),
                            {NewTs, NewE, [Time|Times]}
                    end , {Trees, Env, []}, lists:seq(1,N)),
    ct:log("~p: Average ~p micro-secs per transaction", [Backend, lists:sum(AllTimes) div N]),
    {ok, NewTrees, NewEnv}.


account(N) ->
    <<N,0:248>>.

nonce(Trees, Sender) ->
    {value, Account} = aec_accounts_trees:lookup(Sender, aec_trees:accounts(Trees)),
    aec_accounts:nonce(Account) + 1.

%% Contract Id from account and nonce
id(Account, Nonce) ->
    aect_contracts:compute_contract_pubkey(account(Account), Nonce).


balance(Trees, N) ->
    {value, Account} = aec_accounts_trees:lookup(account(N), aec_trees:accounts(Trees)),
    aec_accounts:balance(Account).

balance_diff(Trees1, N1, N2) ->
    balance(Trees1, N1) - balance(Trees1, N2).

%% We call it decrease, because we assume spending between Trees1 and Trees2
balance_dec(Trees1, Trees2, N) ->
    balance(Trees1, N) - balance(Trees2, N).

%% calls new in Trees2 not yet in Trees1
calls_produced(Trees1, Trees2, Backend) ->
    lists:usort([#{gas => aect_call:gas_used(Call),
                   return => case Backend of
                                 aevm -> aect_call:return_value(Call);
                                 fate ->
                                     case aect_call:return_value(Call) of
                                         <<>> -> <<>>;
                                         Bin -> aeb_fate_encoding:deserialize(Bin)
                                     end
                             end } || {_, Call} <- aect_call_state_tree:to_list(aec_trees:calls(Trees2)) --
                                                                  aect_call_state_tree:to_list(aec_trees:calls(Trees1))]).

trees_with_accounts(Accounts) ->
    trees_with_accounts(Accounts, aec_trees:new_without_backend()).

trees_with_accounts([], Trees) ->
    Trees;
trees_with_accounts([{account, Acc, Amount}|Rest], Trees) ->
    Account = aec_accounts:new(Acc, Amount),
    AccountTrees = aec_accounts_trees:enter(Account, aec_trees:accounts(Trees)),
    trees_with_accounts(Rest, aec_trees:set_accounts(Trees, AccountTrees)).
