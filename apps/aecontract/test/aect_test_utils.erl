%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Test utils for contracts
%%% @end
%%%-------------------------------------------------------------------
-module(aect_test_utils).
-on_load(setup_contract_cache/0).

-export([ new_state/0
        , calls/1
        , set_calls/2
        , contracts/1
        , get_call/3
        , set_contracts/2
        , get_contract/2
        , priv_key/2
        , call_tx/3
        , call_tx/4
        , create_tx/2
        , create_tx/3
        , set_account_balance/3
        , set_trees/2
        , setup_new_account/1
        , setup_new_account/2
        , setup_miner_account/2
        , get_account/2
        , lookup_account/2
        , next_nonce/2
        , trees/1
        , read_contract/1
        , read_contract/2
        , compile_contract/1
        , compile_contract/2
        , compile_filename/1
        , compile_filename/2
        , encode_call_data/3
        , encode_call_data/4
        , decode_data/2
        , decode_call_result/4
        , assert_state_equal/2
        , get_oracle_queries/2
        , dummy_bytecode/0
        , latest_sophia_abi_version/0
        , latest_sophia_vm_version/0
        , latest_protocol_version/0
        , latest_sophia_version/0
        , latest_sophia_contract_version/0
        , require_at_least_protocol/1
        , delegation_signature/3
        ]).

-export([ abi_version/0
        , abi_version/2
        , backend/0
        , contract_filename/1
        , copts/1
        , init_per_group/2
        , init_per_group/3
        , setup_testcase/1
        , sophia_version/0
        , sophia_version/2
        , vm_version/0
        , vm_version/2
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aecontract/test/include/aect_sophia_vsn.hrl").
-include_lib("aecontract/test/include/aect_contract_cache.hrl").
-include("../src/aect_sophia.hrl").

%%%===================================================================
%%% Test state
%%%===================================================================

new_state() ->
    #{}.

trees(#{} = S) ->
    maps:get(trees, S, aec_trees:new_without_backend()).

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

%% Errs if actual test state is different from expected one.
assert_state_equal(Exp, Act) ->
    case {maps:take(trees, Exp), maps:take(trees, Act)} of
        {error, error} ->
            {Exp, _} = {Act, {expected_state, Exp}};
        {{ExpTs, Exp2}, {ActTs, Act2}} ->
            ExpTsHash = aec_trees:hash(ExpTs),
            ActTsHash = aec_trees:hash(ActTs),
            {ExpTsHash, _} = {ActTsHash,
                              {{expected_trees_root_hash, ExpTsHash},
                               {trees, {{actual, ActTs},
                                        {expected, ExpTs}}}}},
            {Exp2, _} = {Act2, {expected_state_except_trees, Exp2}},
            ok
    end.

%%%===================================================================
%%% Info API
%%%===================================================================

latest_sophia_vm_version() ->
    case latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> ?VM_AEVM_SOPHIA_1;
        ?MINERVA_PROTOCOL_VSN -> ?VM_AEVM_SOPHIA_2;
        ?FORTUNA_PROTOCOL_VSN -> ?VM_AEVM_SOPHIA_3;
        ?LIMA_PROTOCOL_VSN    -> ?VM_AEVM_SOPHIA_4;
        ?IRIS_PROTOCOL_VSN    -> ?VM_FATE_SOPHIA_2;
        ?CERES_PROTOCOL_VSN   -> ?VM_FATE_SOPHIA_3;
        ?ARCUS_PROTOCOL_VSN   -> ?VM_FATE_SOPHIA_3
    end.

latest_sophia_abi_version() ->
    case latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> ?ABI_AEVM_SOPHIA_1;
        ?MINERVA_PROTOCOL_VSN -> ?ABI_AEVM_SOPHIA_1;
        ?FORTUNA_PROTOCOL_VSN -> ?ABI_AEVM_SOPHIA_1;
        ?LIMA_PROTOCOL_VSN    -> ?ABI_AEVM_SOPHIA_1;
        ?IRIS_PROTOCOL_VSN    -> ?ABI_FATE_SOPHIA_1;
        ?CERES_PROTOCOL_VSN   -> ?ABI_FATE_SOPHIA_1;
        ?ARCUS_PROTOCOL_VSN   -> ?ABI_FATE_SOPHIA_1
    end.

latest_sophia_version() ->
    case latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> ?SOPHIA_ROMA;
        ?MINERVA_PROTOCOL_VSN -> ?SOPHIA_MINERVA;
        ?FORTUNA_PROTOCOL_VSN -> ?SOPHIA_FORTUNA;
        ?LIMA_PROTOCOL_VSN    -> ?SOPHIA_LIMA_AEVM;
        ?IRIS_PROTOCOL_VSN    -> ?SOPHIA_IRIS_FATE;
        ?CERES_PROTOCOL_VSN   -> ?SOPHIA_CERES_FATE;
        ?ARCUS_PROTOCOL_VSN   -> ?SOPHIA_ARCUS_FATE
    end.

latest_sophia_contract_version() ->
    case latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> ?SOPHIA_CONTRACT_VSN_1;
        ?MINERVA_PROTOCOL_VSN -> ?SOPHIA_CONTRACT_VSN_2;
        ?FORTUNA_PROTOCOL_VSN -> ?SOPHIA_CONTRACT_VSN_2;
        ?LIMA_PROTOCOL_VSN    -> ?SOPHIA_CONTRACT_VSN_3;
        ?IRIS_PROTOCOL_VSN    -> ?SOPHIA_CONTRACT_VSN_3;
        ?CERES_PROTOCOL_VSN   -> ?SOPHIA_CONTRACT_VSN_3;
        ?ARCUS_PROTOCOL_VSN   -> ?SOPHIA_CONTRACT_VSN_3
    end.

latest_protocol_version() ->
    lists:max(maps:keys(aec_hard_forks:protocols())).

require_at_least_protocol(Protocol) ->
    case latest_protocol_version() of
	V when V >= Protocol ->
	    ok;
	Other ->
	    Msg = case Other of
		      ?ROMA_PROTOCOL_VSN    -> not_in_roma;
		      ?MINERVA_PROTOCOL_VSN -> not_in_minerva;
		      ?FORTUNA_PROTOCOL_VSN -> not_in_fortuna;
		      ?LIMA_PROTOCOL_VSN    -> not_in_lima;
		      ?IRIS_PROTOCOL_VSN    -> not_in_iris;
		      ?CERES_PROTOCOL_VSN   -> not_in_ceres;
		      ?ARCUS_PROTOCOL_VSN   -> not_in_arcus
		  end,
	    {skip, Msg}
    end.

calls(State) ->
    aec_trees:calls(trees(State)).

get_call(ContractId, CallId, State) ->
    Calls = aec_trees:calls(trees(State)),
    {value, Call} = aect_call_state_tree:lookup_call(ContractId, CallId,
                                                     Calls),
    Call.

set_calls(Calls, State) ->
    Trees = trees(State),
    set_trees(aec_trees:set_calls(Trees, Calls), State).

contracts(State) ->
    aec_trees:contracts(trees(State)).

set_contracts(Contracts, State) ->
    Trees = trees(State),
    set_trees(aec_trees:set_contracts(Trees, Contracts), State).

get_contract(ContractId, State) ->
    Trees = aec_trees:contracts(trees(State)),
    aect_state_tree:get_contract(ContractId, Trees).

%%%===================================================================
%%% Register tx
%%%===================================================================

create_tx(PubKey, State) ->
    create_tx(PubKey, #{}, State).

create_tx(PubKey, Spec0, State) ->
    Spec = maps:merge(create_tx_default_spec(PubKey, State), Spec0),
    {ok, Tx} = aect_create_tx:new(Spec),
    Tx.

create_tx_default_spec(PubKey, State) ->
    #{ fee         => 1000000 * aec_test_utils:min_gas_price()
     , owner_id    => aeser_id:create(account, PubKey)
     , nonce       => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , code        => dummy_bytecode()
     , vm_version  => vm_version()
     , abi_version => abi_version()
     , deposit     => 10
     , amount      => 200
     , gas         => 10
     , gas_price   => 1 * aec_test_utils:min_gas_price()
     , call_data   => <<"NOT ENCODED ACCORDING TO ABI">>
     , ttl         => 0
     }.

dummy_bytecode() ->
    {ok, Version} = aeso_compiler:version(),
    aect_sophia:serialize(#{byte_code => <<"NOT PROPER BYTE CODE">>,
                            type_info => [],  %% No type info
                            contract_source => "NOT PROPER SOURCE STRING",
                            compiler_version => Version,
                            payable => false},
                          latest_sophia_contract_version()
                         ).

%%%===================================================================
%%% Call tx
%%%===================================================================

call_tx(PubKey, ContractKey, State) ->
    call_tx(PubKey, ContractKey, #{}, State).

call_tx(PubKey, ContractKey, Spec0, State) ->
    Spec = maps:merge(call_tx_default_spec(PubKey, ContractKey, State), Spec0),
    {ok, Tx} = aect_call_tx:new(Spec),
    Tx.

call_tx_default_spec(PubKey, ContractKey, State) ->
    #{ fee         => 600000 * aec_test_utils:min_gas_price()
     , contract_id => aeser_id:create(contract, ContractKey)
     , caller_id   => aeser_id:create(account, PubKey)
     , nonce       => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , abi_version => abi_version()
     , amount      => 100
     , gas         => 10000
     , gas_price   => 1 * aec_test_utils:min_gas_price()
     , call_data   => <<"CALL DATA">>
     , ttl         => 0
     }.

%%%===================================================================
%%% Accounts
%%%===================================================================

setup_new_account(State) ->
    setup_new_account(10000000 * aec_test_utils:min_gas_price(), State).

setup_new_account(Balance, State) ->
    {PubKey, PrivKey} = new_key_pair(),
    State1            = insert_key_pair(PubKey, PrivKey, State),
    State2            = set_account(aec_accounts:new(PubKey, Balance), State1),
    {PubKey, State2}.

setup_miner_account(PubKey, State) ->
    A = aec_accounts:new(PubKey, aec_governance:block_mine_reward(0)),
    set_account(A, State).

set_account_balance(PubKey, NewBalance, State) ->
    A        = get_account(PubKey, State),
    Balance  = aec_accounts:balance(A),
    Nonce    = aec_accounts:nonce(A),
    {ok, A1} = aec_accounts:spend(A, Balance, Nonce),
    {ok, A2} = aec_accounts:earn(A1, NewBalance),
    set_account(A2, State).

get_account(PubKey, State) ->
    aec_accounts_trees:get(PubKey, aec_trees:accounts(trees(State))).

lookup_account(PubKey, State) ->
    aec_accounts_trees:lookup(PubKey, aec_trees:accounts(trees(State))).

set_account(Account, State) ->
    Trees   = trees(State),
    AccTree = aec_accounts_trees:enter(Account, aec_trees:accounts(Trees)),
    set_trees(aec_trees:set_accounts(Trees, AccTree), State).

read_contract(Name) ->
    read_contract(sophia_version(), Name).

read_contract(Compiler, Name) ->
    file:read_file(contract_filename(Compiler, Name)).

contract_dirs(?SOPHIA_ROMA)       -> ["sophia_1"      | contract_dirs(?SOPHIA_MINERVA)];
contract_dirs(?SOPHIA_MINERVA)    -> ["sophia_2"      | contract_dirs(?SOPHIA_FORTUNA)];
contract_dirs(?SOPHIA_FORTUNA)    -> ["sophia_3"      | contract_dirs(?SOPHIA_LIMA_AEVM)];
contract_dirs(?SOPHIA_LIMA_AEVM)  -> ["sophia_4_aevm" | contract_dirs(?SOPHIA_LIMA_FATE)];
contract_dirs(?SOPHIA_LIMA_FATE)  -> ["sophia_4"      | contract_dirs(?SOPHIA_IRIS_FATE)];
contract_dirs(?SOPHIA_IRIS_FATE)  -> ["sophia_5"];
contract_dirs(?SOPHIA_CERES_FATE) -> [];
contract_dirs(?SOPHIA_ARCUS_FATE) -> [].

contract_filenames(Compiler, Name) when is_atom(Name) ->
    contract_filenames(Compiler, atom_to_list(Name));
contract_filenames(Compiler, Name) ->
    CodeDir = filename:join(code:lib_dir(aecontract), "../../extras/test/"),
    Name1 = filename:rootname(Name, ".aes") ++ ".aes",
    Files = [filename:join([CodeDir] ++ Contracts ++ [SubDir, Name1])
             || Contracts <- [["contracts"], []],
                SubDir    <- contract_dirs(Compiler) ++ ["."]],
    %% io:format("Files for ~p (compiler ~p): ~p\n", [Name, Compiler, Files]),
    lists:filter(fun filelib:is_regular/1, Files).

contract_filename(Name) ->
    contract_filename(sophia_version(), Name).

contract_filename(Compiler, Name) ->
    case contract_filenames(Compiler, Name) of
        [File | _] -> File;
        []         -> error({cant_find_contract, Name, [{compiler, Compiler}]})
    end.

compile_filename(FileName) ->
    compile(sophia_version(), FileName).

compile_filename(Compiler, FileName) ->
    compile(Compiler, FileName).

compile_contract(File) ->
    compile_contract(sophia_version(), File).

compile_contract(Compiler, File) ->
    compile_filename(Compiler, contract_filename(Compiler, File)).

compile(Vsn, File) ->
    %% Lookup the res in the cache - if not present just calculate the result
    CompilationId = #compilation_id{vsn = Vsn, filename = File},
    NoCache = os:getenv("SOPHIA_NO_CACHE"),
    %% Depending on the compiler support, generate ACI either in parallel, during
    %% the compilation process, or don't do it at all
    case aci_json_enabled(Vsn) of
        yes_automatic -> ok; % will generate during compilation
        yes_manual ->
            Self = self(),
            spawn(fun() ->
                {ok, AsmBin} = file:read_file(File),
                case generate_json_aci(Vsn, AsmBin, copts({file, File})) of
                    {error, _} = Err ->
                        Self ! {aci_done, Err};
                    _ ->
                        Self ! {aci_done, ok}
                end end);
        no -> ok
    end,
    Result1 = case ets:lookup(?COMPILE_TAB, CompilationId) of
        _ when NoCache =/= false ->
            compile_(Vsn, File);
        [#compilation_cache_entry{result = Result}] ->
            %% This should save 200ms - 2000ms per invocation
            ct:log("Compilation cache HIT  :)"),
            Result;
        [] ->
            ct:log("Compilation cache MISS :("),
            Result = compile_(Vsn, File),
            ets:insert_new(?COMPILE_TAB, #compilation_cache_entry{compilation_id = CompilationId, result = Result}),
            Result
    end,
    case aci_json_enabled(Vsn) of
        yes_automatic -> Result1;
        yes_manual ->
            receive
                {aci_done, ok} ->
                    Result1;
                {aci_done, Err} ->
                    Err
            after
                3000 ->
                    ct:log("ACI process failed to deliver response", []),
                    exit(logic_error)
            end;
        no -> Result1
    end.

copts({file, File}) ->
    SrcDir = aeso_utils:canonical_dir(filename:dirname(File)),
    [{src_dir, SrcDir}, {include, {file_system, [SrcDir]}}].

compile_(SophiaVsn, File) when SophiaVsn >= ?SOPHIA_CERES_FATE ->
    {ok, AsmBin} = file:read_file(File),
    Source = binary_to_list(AsmBin),
    ACIFlag = case aci_json_enabled(SophiaVsn) of
                  yes_automatic -> [{aci, json}];
                  _ -> []
              end,
    case aeso_compiler:from_string(Source, [{backend, fate}, {src_file, File}] ++
                                           copts({file, File}) ++ ACIFlag) of
        {ok, Map} ->
            case Map of
                #{aci := JAci} ->
                    AciId = make_aci_id(Source),
                    ACI = aeaci_aci:from_string(jsx:encode(JAci), #{backend => fate}),
                    cache_aci(AciId, ACI);
                _ -> ok
            end,
            {ok, aect_sophia:serialize(Map, latest_sophia_contract_version())};
        {error, E} = Err -> ct:log("~p\n", [E]), Err
    end;
compile_(LegacyVersion, File) ->
    case legacy_compile(LegacyVersion, File) of
        {ok, Code}      -> {ok, Code};
        {error, Reason} -> {error, {compiler_error, File, Reason}}
    end.

new_key_pair() ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    {PubKey, PrivKey}.

legacy_compile(Vsn, SrcFile) ->
    Compiler = compiler_cmd(Vsn),
    OutFile  = tempfile_name("tmp_sophia_", ".aeb"),
    Cmd = Compiler ++ " " ++ SrcFile ++ " -o " ++ OutFile,
    _Output = os:cmd(Cmd),
    try
        {ok, Bin} = file:read_file(OutFile),
        aeser_api_encoder:safe_decode(contract_bytearray, Bin)
    catch _:_ ->
        {error, <<"Compiler error:\n", (list_to_binary(_Output))/binary>>}
    after
        cleanup_tempfiles()
    end.

compiler_cmd(Vsn) ->
    BaseDir = filename:join([code:priv_dir(aesophia_cli), "bin"]),
    case Vsn of
        ?SOPHIA_ROMA       -> filename:join([BaseDir, "v1.4.0", "aesophia_cli"]);
        ?SOPHIA_MINERVA    -> filename:join([BaseDir, "v2.1.0", "aesophia_cli"]);
        ?SOPHIA_FORTUNA    -> filename:join([BaseDir, "v3.2.0", "aesophia_cli"]);
        ?SOPHIA_LIMA_AEVM  -> filename:join([BaseDir, "v4.3.1", "aesophia_cli"]) ++ " --backend=aevm";
        ?SOPHIA_LIMA_FATE  -> filename:join([BaseDir, "v4.3.1", "aesophia_cli"]);
        ?SOPHIA_IRIS_FATE  -> filename:join([BaseDir, "v6.1.0", "aesophia_cli"]);
        ?SOPHIA_CERES_FATE -> filename:join([BaseDir, "v6.1.0", "aesophia_cli"]); %% used for ACI generation in tests :rolling_eyes:
        ?SOPHIA_ARCUS_FATE -> filename:join([BaseDir, "v6.1.0", "aesophia_cli"])
    end.

aci_json_enabled(Vsn) ->
    case aci_disabled() of
        true -> no;
        _  when Vsn =< ?SOPHIA_ROMA      -> no;
        _  when Vsn =< ?SOPHIA_LIMA_FATE -> yes_manual;
        _                                -> yes_automatic
    end.

tempfile_name(Prefix, Extension) ->
    File = temp_filename(Prefix, Extension),
    case get('$tmp_files') of
        undefined -> put('$tmp_files', [File]);
        Files     -> put('$tmp_files', [File | Files])
    end,
    File.

%% A few functions mostly lifted / adapted from the bucs lib
temp_filename(Prefix, Extension) ->
    Path = temp_dir(),
    filename:join([Path, Prefix ++ randstr(20) ++ Extension]).

-define(CHARS, <<"azertyuiopqsdfghjklmwxcvbn"
                 "AZERTYUIOPQSDFGHJKLMWXCVBN1234567890">>).
randstr(Size) ->
    PoolSize = byte_size(?CHARS),
    [binary:at(?CHARS, rand:uniform(PoolSize) - 1)
     || _ <- lists:seq(1, Size)].

%% Yes, this might be a dreaded nested case, but it is extremely obvious
%% what it does at a glance.
temp_dir() ->
    case os:getenv("TMPDIR") of
    false ->
      case os:getenv("TEMP") of
        false ->
          case os:getenv("TMP") of
            false ->
              case writeable_dir("/tmp") of
                false ->
                  Cwd = case file:get_cwd() of
                          {ok, Dir} -> Dir;
                          _ -> "."
                        end,
                  case writeable_dir(Cwd) of
                    false -> false;
                    LTmp -> LTmp
                  end;
                STmp -> STmp
              end;
            Tmp -> Tmp
          end;
        Temp -> Temp
      end;
    Tmpdir -> Tmpdir
  end.

writeable_dir(Path) ->
  case file:read_file_info(Path) of
    {ok, #file_info{type = directory, access = Access}}
        when Access =:= read_write; Access =:= write ->
      Path;
    _ -> false
  end.

cleanup_tempfiles() ->
    case get('$tmp_files') of
        Files when is_list(Files) -> [ delete_file(F) || F <- Files ];
        _                         -> ok
    end.

delete_file(F) ->
    try
        file:delete(F)
    catch _:_ ->
        ok
    end.

to_str(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_str(Str)                     -> Str.

encode_call_data(Code, Fun, Args) ->
    encode_call_data(sophia_version(), Code, Fun, Args).

encode_call_data(Vsn, {contract, ContractName}, Fun, Args) ->
    File = contract_filename(Vsn, ContractName),
    {ok, SrcBin} = file:read_file(File),
    maybe_fast_encode_call_data(Vsn, to_str(SrcBin), Fun, Args, copts({file, File}));
encode_call_data(Vsn, Code, Fun, Args) ->
    maybe_fast_encode_call_data(Vsn, Code, Fun, Args, []).

maybe_fast_encode_call_data(Vsn, Code, Fun, Args, COpts) ->
    case aci_json_enabled(Vsn) of
        no ->
            slow_encode_call_data(Vsn, Code, Fun, Args, COpts);
        _ -> % yes_manual | yes_automatic
            Aci = generate_json_aci(Vsn, Code, COpts),
            Args1 = string:join([ to_str(Arg) || Arg <- Args ], ", "),
            Args2 = to_str(Fun) ++ "(" ++ Args1 ++ ")",
            case aeaci_aci:encode_call_data(Aci, Args2) of
                {ok, _} = Res ->
                    Res;
                {error, Reason} = Err ->
                    ct:log("Encoding call data using JSON ACI failed: ~p\n", [Reason]),
                    Err
            end
    end.

slow_encode_call_data(Vsn, Code, Fun, Args, COpts) ->
    %% Lookup the res in the cache - if not present just calculate the result
    Backend = backend(Vsn),
    CallId = #encode_call_id{vsn = Vsn, code_hash = crypto:hash(md5, Code), fun_name = Fun, args = Args, backend = Backend},
    case ets:lookup(?ENCODE_CALL_TAB, CallId) of
        [#encode_call_cache_entry{result = Result}] ->
            %% This should save 100ms - 300ms per invocation
            ct:log("Encode call cache HIT  :)"),
            Result;
        [] ->
            ct:log("Encode call cache MISS :("),
            Result = slow_encode_call_data_(Vsn, Code, Fun, Args, [{backend, Backend} | COpts]),
            ets:insert_new(?ENCODE_CALL_TAB, #encode_call_cache_entry{call_id = CallId, result = Result}),
            Result
    end.

slow_encode_call_data_(Vsn, Code, Fun, Args, COpts) when Vsn >= ?SOPHIA_CERES_FATE ->
    try aeso_compiler:create_calldata(to_str(Code), to_str(Fun),
                                      lists:map(fun to_str/1, Args),
                                      COpts)
    catch _T:_E ->
        {error, <<"bad argument">>}
    end;
slow_encode_call_data_(Vsn, Code, Fun, Args0, _COpts) ->
    SrcFile = tempfile_name("sophia_code", ".aes"),
    Args    = legacy_args(Vsn, Args0),
    ok = file:write_file(SrcFile, Code),
    Compiler = compiler_cmd(max(Vsn, ?SOPHIA_MINERVA)),
    Cmd = Compiler ++ create_calldata_args(Vsn, SrcFile, Fun, Args),
    Output = os:cmd(Cmd),
    try
        [_, CalldataStr] = string:lexemes(Output, "\n"),
        aeser_api_encoder:safe_decode(contract_bytearray, list_to_binary(CalldataStr))
    catch _:Err:StackTrace ->
        {error, {<<"Compiler error">>, Err, StackTrace}}
    after
        cleanup_tempfiles()
    end.

create_calldata_args(Vsn, SrcFile, Fun, Args) ->
    Esc = fun(Str) -> lists:flatten(string:replace(string:replace(Str, "\\", "\\\\", all), "\"", "\\\"", all)) end,
    Args1 = string:join([ Esc(to_str(Arg)) || Arg <- Args ], ", "),
    CallArgs =
        if Vsn =< ?SOPHIA_FORTUNA ->
                " --calldata_fun " ++ to_str(Fun) ++ " --calldata_args \"" ++ Args1 ++ "\"";
           true ->
                " --call \"" ++ to_str(Fun) ++ "(" ++ Args1 ++ ")\""
        end,
    " --create_calldata " ++ contract_filename(Vsn, SrcFile) ++ CallArgs.

decode_call_result(Code, Fun, Res, Value) ->
    %% Lookup the res in the cache - if not present just calculate the result
    DecodeCallId = #decode_call_id{code_hash = crypto:hash(md5, Code), fun_name = Fun, res = Res, val = Value},
    case ets:lookup(?DECODE_CALL_TAB, DecodeCallId) of
        [#decode_call_cache_entry{result = Result}] ->
            %% This should save 10-30ms per invocation - this still saves time as some tests call this function >200 times mostly with the same args
            ct:log("Decode call cache HIT  :)"),
            Result;
        [] ->
            ct:log("Decode call cache MISS :("),
            Result = decode_call_result(backend(), Code, Fun, Res, Value),
            ets:insert_new(?DECODE_CALL_TAB, #decode_call_cache_entry{decode_call_id = DecodeCallId, result = Result}),
            Result
    end.


decode_call_result(aevm, Code, Fun, Res, Value) ->
    decode_call_result_aevm(Code, Fun, Res, Value);
decode_call_result(fate, Code, Fun, Res, Value) ->
    decode_call_result_fate(Code, Fun, Res, Value).

decode_call_result_aevm(Code, Fun, Res, EValue = <<"cb_", _/binary>>) ->
    SrcFile = tempfile_name("sophia_code", ".aes"),
    ok = file:write_file(SrcFile, Code),
    Compiler = compiler_cmd(?SOPHIA_LIMA_AEVM),
    Args = lists:flatten(io_lib:format(" ~s --call_result_type ~p --call_result ~s --call_result_fun ~s",
                                       [SrcFile, Res, EValue, Fun])),

    Cmd = Compiler ++ Args,
    Output = os:cmd(Cmd),
    try
        [_ | CallRes] = string:lexemes(Output, "\n"),
        simple_encode_expr(string:join(CallRes, ""))
    catch _:Err:StackTrace ->
        {error, {<<"Compiler error">>, Err, StackTrace}}
    after
        cleanup_tempfiles()
    end;
decode_call_result_aevm(Code, Fun, Res, Value) ->
    decode_call_result_aevm(Code, Fun, Res, aeser_api_encoder:encode(contract_bytearray, Value)).

decode_call_result_fate(Code, Fun, Res, EValue = <<"cb_", _/binary>>) ->
    case aeser_api_encoder:safe_decode(contract_bytearray, EValue) of
        {ok, Value} ->
            decode_call_result_fate(Code, Fun, Res, Value);
        Err = {error, _} ->
            Err
    end;
decode_call_result_fate(Code, Fun, Res, Value) ->
    {ok, ValExpr} = aeso_compiler:to_sophia_value(to_str(Code), to_str(Fun), Res, Value),
    aeso_aci:json_encode_expr(ValExpr).


simple_encode_expr(E = [C | _]) when C == $[; C == $( ->
    [ simple_encode_expr(E0) || E0 <- split_at_top_expr([$,], trim_outer(E)) ];
simple_encode_expr(E = [${ | _]) ->
    maps:from_list([ simple_encode_map_expr(string:trim(E0, both)) || E0 <- split_at_top_expr([$,], trim_outer(E)) ]);
simple_encode_expr("false") -> false;
simple_encode_expr("true") -> true;
simple_encode_expr("None") -> <<"None">>;
simple_encode_expr("Some" ++ SomeVal) -> #{<<"Some">> => [simple_encode_expr(trim_outer(SomeVal))]};
simple_encode_expr("abort" ++ SomeVal) -> #{<<"abort">> => [simple_encode_expr(trim_outer(SomeVal))]};
simple_encode_expr([$" | _] = Str) ->
    list_to_binary(string:trim(Str, both, "\""));
simple_encode_expr([D | _] = Int) when D >= $0, D =< $9 ->
    list_to_integer(Int);
simple_encode_expr([$# | _] = Enc) ->
    list_to_binary(Enc);
simple_encode_expr([_, _, $_ | _] = Enc) ->
    list_to_binary(Enc);
simple_encode_expr(E) ->
    E.

simple_encode_map_expr(E) ->
    [Key, Val] = split_at_top_expr(E),
    {list_to_binary(Key), simple_encode_expr(Val)}.

trim_outer(S) ->
    lists:droplast(tl(string:trim(S, both))).


split_at_top_expr(Str) -> split_at(Str, [$,, $=], [$(, $[, ${], [$), $], $}], 0, []).
split_at_top_expr(At, Str) -> split_at(Str, At, [$(, $[, ${], [$), $], $}], 0, []).

split_at([], _, _, _, _, [])                -> [];
split_at([], _, _, _, _, Acc)               -> [string:trim(lists:reverse(Acc), both)];
split_at([C | Rest], At, In, Out, N, Acc)   ->
  case {lists:member(C, At) andalso N == 0, lists:member(C, In), lists:member(C, Out)} of
        {true, _, _} -> [string:trim(lists:reverse(Acc), both) | split_at(Rest, At, In, Out, N, [])];
        {_, true, _} -> split_at(Rest, At, In, Out, N + 1, [C | Acc]);
        {_, _, true} -> split_at(Rest, At, In, Out, N - 1, [C | Acc]);
        _            -> split_at(Rest, At, In, Out, N, [C | Acc])
    end.

generate_json_aci(Vsn, Code, COpts) ->
    Backend = backend(),
    AciId = make_aci_id(Code),
    NoCache = os:getenv("SOPHIA_ACI_NO_CACHE"),
    case ets:lookup(?ACI_TAB, AciId) of
        _ when NoCache /= false ->
            generate_json_aci_(Vsn, Backend, Code, COpts);
        [#aci_cache_entry{result = Result}] ->
            ct:log("ACI cache HIT :)"),
            Result;
        [] ->
            ct:log("ACI cache MISS :)"),
            Result = generate_json_aci_(Vsn, Backend, Code, COpts),
            cache_aci(AciId, Result),
            Result
    end.

generate_json_aci_(Vsn, Backend, Code, COpts) when Vsn >= ?SOPHIA_CERES_FATE ->
    try
        {ok, JAci} = aeso_aci:contract_interface(json, to_str(Code), [{backend, Backend}, {no_code, true}] ++ COpts),
        aeaci_aci:from_string(jsx:encode(JAci), #{backend => Backend})
    catch Err:Reason:Stack ->
        ct:log("Aci generation failed ~p ~p ~p\n", [Err, Reason, Stack]),
        {error, <<"bad argument">>}
    end;
generate_json_aci_(Vsn, Backend, Code, _COpts) ->
    SrcFile = tempfile_name("sophia_code", ".aes"),
    ok = file:write_file(SrcFile, Code),
    Compiler = compiler_cmd(max(Vsn, ?SOPHIA_MINERVA)),
    Cmd = Compiler ++ " --create_json_aci " ++ SrcFile,
    Output = os:cmd(Cmd),
    try
        Output1 =
            if  Vsn < ?SOPHIA_LIMA_AEVM ->
                    <<"ACI generated successfully!\n\n", JText/binary>> = list_to_binary(Output),
                    JText;
                true ->
                    list_to_binary(Output)
            end,
        aeaci_aci:from_string(Output1, #{backend => Backend})
    catch Err:Reason:Stack ->
        ct:log("Aci generation failed ~p ~p ~p\n", [Err, Reason, Stack]),
        {error, <<"bad argument">>}
    after
        cleanup_tempfiles()
    end.

make_aci_id(Code) ->
    #aci_cache_id{code_hash = crypto:hash(md5, Code), backend = backend()}.

cache_aci(AciId, Aci) ->
    ets:insert_new(?ACI_TAB, #aci_cache_entry{aci_id = AciId, result = Aci}).

decode_data(Type, <<"cb_", _/binary>> = EncData) ->
    case aeser_api_encoder:safe_decode(contract_bytearray, EncData) of
        {ok, Data} ->
            decode_data_(Type, Data);
        Err = {error, _} ->
            Err
    end;
decode_data(Type, Data) ->
    decode_data_(Type, Data).

decode_data_(Type, Data) ->
    Return = decode_data_(backend(), Type, Data),
    Return.

decode_data_(fate, _Type, Data) ->
    try {ok, aefa_test_utils:decode(aeb_fate_encoding:deserialize(Data))}
    catch _:_ -> {error, <<"bad fate data">>}
    end;
decode_data_(aevm, Type, Data0) ->
    Data = case Data0 of
               <<"cb_", _/binary>> -> Data0;
               _ -> aeser_api_encoder:encode(contract_bytearray, Data0)
           end,

    Compiler = compiler_cmd(?SOPHIA_LIMA_AEVM),
    Args = lists:flatten(io_lib:format(" -b aevm --decode_data ~s --decode_type ~s", [Data, Type])),
    Cmd = Compiler ++ Args,
    Output = os:cmd(Cmd),
    try
        [_ | ValueStr] = string:lexemes(Output, "\n"),
        {ok, simple_encode_expr(string:join(ValueStr, ""))}
    catch _:Err:StackTrace ->
        {error, {<<"Compiler error">>, Err, StackTrace}}
    after
        cleanup_tempfiles()
    end.

%% Convert to old style hex literals.
legacy_args(Vsn, Args) when Vsn =< ?SOPHIA_MINERVA ->
    lists:map(fun legacy_arg/1, Args);
legacy_args(_, Args) -> Args.

legacy_arg(Str) when is_list(Str) -> legacy_arg(list_to_binary(Str));
legacy_arg(Bin) when is_binary(Bin) ->
    try aeser_api_encoder:decode(Bin) of
        {_, Val} -> <<"0x", Hex/binary>> = aeu_hex:hexstring_encode(Val),
                    <<"#", Hex/binary>>
    catch _:_ ->
        Bin
    end;
legacy_arg(X) -> X.

%%%===================================================================
%%% Oracles
%%%===================================================================

get_oracle_queries(OracleId, State) ->
    get_oracle_queries(OracleId, 1000, State).

get_oracle_queries(OracleId, Max, State) ->
    aeo_state_tree:get_oracle_queries(OracleId, '$first', all, Max, aec_trees:oracles(trees(State))).

%%%===================================================================
%%% Common test common stuff
%%%===================================================================
init_per_group(Vm, Cfg) ->
    init_per_group(Vm, Cfg, fun(X) -> X end).


sophia_version(aevm, ?ROMA_PROTOCOL_VSN) -> ?SOPHIA_ROMA;
sophia_version(aevm, ?MINERVA_PROTOCOL_VSN) -> ?SOPHIA_MINERVA;
sophia_version(aevm, ?FORTUNA_PROTOCOL_VSN) -> ?SOPHIA_FORTUNA;
sophia_version(aevm, ?LIMA_PROTOCOL_VSN) -> ?SOPHIA_LIMA_AEVM;
sophia_version(aevm, _) -> {error, aevm_deprecated};
sophia_version(fate, ?LIMA_PROTOCOL_VSN) -> ?SOPHIA_LIMA_FATE;
sophia_version(fate, ?IRIS_PROTOCOL_VSN) -> ?SOPHIA_IRIS_FATE;
sophia_version(fate, ?CERES_PROTOCOL_VSN) -> ?SOPHIA_CERES_FATE;
sophia_version(fate, ?ARCUS_PROTOCOL_VSN) -> ?SOPHIA_ARCUS_FATE;
sophia_version(fate, Protocol) when Protocol < ?LIMA_PROTOCOL_VSN -> {error, fate_not_available}.

vm_version(aevm, ?ROMA_PROTOCOL_VSN) -> ?VM_AEVM_SOPHIA_1;
vm_version(aevm, ?MINERVA_PROTOCOL_VSN) -> ?VM_AEVM_SOPHIA_2;
vm_version(aevm, ?FORTUNA_PROTOCOL_VSN) -> ?VM_AEVM_SOPHIA_3;
vm_version(aevm, ?LIMA_PROTOCOL_VSN) -> ?VM_AEVM_SOPHIA_4;
vm_version(aevm, _) -> {error, aevm_deprecated};
vm_version(fate, ?LIMA_PROTOCOL_VSN) -> ?VM_FATE_SOPHIA_1;
vm_version(fate, ?IRIS_PROTOCOL_VSN) -> ?VM_FATE_SOPHIA_2;
vm_version(fate, ?CERES_PROTOCOL_VSN) -> ?VM_FATE_SOPHIA_3;
vm_version(fate, ?ARCUS_PROTOCOL_VSN) -> ?VM_FATE_SOPHIA_3;
vm_version(fate, Protocol) when Protocol < ?LIMA_PROTOCOL_VSN -> {error, fate_not_available}.

abi_version(aevm, ?ROMA_PROTOCOL_VSN) -> ?ABI_AEVM_SOPHIA_1;
abi_version(aevm, ?MINERVA_PROTOCOL_VSN) -> ?ABI_AEVM_SOPHIA_1;
abi_version(aevm, ?FORTUNA_PROTOCOL_VSN) -> ?ABI_AEVM_SOPHIA_1;
abi_version(aevm, ?LIMA_PROTOCOL_VSN) -> ?ABI_AEVM_SOPHIA_1;
abi_version(aevm, _) -> {error, aeavm_deprecated};
abi_version(fate, ?LIMA_PROTOCOL_VSN) -> ?ABI_FATE_SOPHIA_1;
abi_version(fate, ?IRIS_PROTOCOL_VSN) -> ?ABI_FATE_SOPHIA_1;
abi_version(fate, ?CERES_PROTOCOL_VSN) -> ?ABI_FATE_SOPHIA_1;
abi_version(fate, ?ARCUS_PROTOCOL_VSN) -> ?ABI_FATE_SOPHIA_1;
abi_version(fate, Protocol) when Protocol < ?LIMA_PROTOCOL_VSN -> {error, fate_not_available}.

init_per_group(VM, Cfg, Cont) ->
    Protocol = aect_test_utils:latest_protocol_version(),
    case sophia_version(VM, Protocol) of
        {error, Err} -> {skip, Err};
        _ ->
            ProtocolAtom = aec_hard_forks:protocol_vsn_name(Protocol),
            ct:pal("Running tests under ~p protocol using ~p", [ProtocolAtom, VM]),
            Cont([{sophia_version, sophia_version(VM, Protocol)},
                  {vm_version, vm_version(VM, Protocol)},
                  {abi_version, abi_version(VM, Protocol)},
                  {protocol, ProtocolAtom} | Cfg])
    end.

setup_testcase(Config) ->
    VmVersion = ?config(vm_version, Config),
    ABIVersion = ?config(abi_version, Config),
    SophiaVersion = ?config(sophia_version, Config),
    ProtocolVersion = aec_hard_forks:protocol_vsn(?config(protocol, Config)),
    AciDisabled = case os:getenv("SOPHIA_NO_ACI") of
                      false ->
                          ?config(aci_disabled, Config);
                      _ ->
                          true
                  end,
    put('$vm_version', VmVersion),
    put('$abi_version', ABIVersion),
    put('$sophia_version', SophiaVersion),
    put('$protocol_version', ProtocolVersion),
    put('$aci_disabled', AciDisabled).

vm_version() ->
    case get('$vm_version') of
        undefined -> latest_sophia_vm_version();
        X         -> X
    end.

abi_version() ->
    case get('$abi_version') of
        undefined -> latest_sophia_abi_version();
        X         -> X
    end.

sophia_version() ->
    case get('$sophia_version') of
        undefined -> latest_sophia_version();
        X         -> X
    end.

backend() ->
    case abi_version() of
        ?ABI_AEVM_SOPHIA_1 -> aevm;
        ?ABI_FATE_SOPHIA_1 -> fate
    end.

backend(?SOPHIA_LIMA_FATE ) -> fate;
backend(?SOPHIA_IRIS_FATE ) -> fate;
backend(?SOPHIA_CERES_FATE) -> fate;
backend(?SOPHIA_ARCUS_FATE) -> fate;
backend(_                 ) -> aevm.

aci_disabled() ->
    case get('$aci_disabled') of
        undefined            -> false;
        X when is_boolean(X) -> X
    end.

%% setup a global memoization cache for contracts
setup_contract_cache() ->
    [ok = try_setup_cache(ETSTable, Keypos) || {ETSTable, Keypos} <- cached_tables()],
    ok.

try_setup_cache(Tab, Keypos) ->
    Self = self(),
    case ets:info(Tab, name) of
        undefined ->
            spawn(fun() ->
                ets:new(Tab, [set, public, named_table, {keypos, Keypos}]),
                Self ! cache_ready,
                timer:sleep(infinity)
            end),
            receive
                cache_ready ->
                    ct:log("Cache ~p initialized", [Tab]),
                    ok
            after
                3000 ->
                    ct:log("Failed to init cache ~p", [Tab]),
                    exit(timeout)
            end;
        _ ->
            ok
    end.

%% delegation signature
delegation_signature(Type, Material, PrivKey) ->
    SigData =
        case vm_version() of
            VM when VM =< ?VM_FATE_SOPHIA_2 ->
                aec_governance:add_network_id(delegate_sign_old(Type, Material));
            _ ->
                delegate_sign_new(Type, Material)
        end,
    enacl:sign_detached(SigData, PrivKey).

%% Pre-Ceres
delegate_sign_old(aens_wild, {A, C}) -> <<A/binary, "AENS"/utf8, C/binary>>;
delegate_sign_old(_, {A, B})         -> <<A/binary, B/binary>>;
delegate_sign_old(_, {A, B, C})      -> <<A/binary, B/binary, C/binary>>.

%% From Ceres
delegate_sign_new(aens_wild, {A, C}) ->
    aeser_del(aens_sig, {aeser_id:create(account, A), aeser_id:create(contract, C)});
delegate_sign_new(aens_preclaim, {A, C}) ->
    aeser_del(aens_preclaim_sig, {aeser_id:create(account, A), aeser_id:create(contract, C)});
delegate_sign_new(aens_name, {A, N, C}) ->
    aeser_del(aens_name_sig, {aeser_id:create(account, A), aeser_id:create(name, N), aeser_id:create(contract, C)});
delegate_sign_new(oracle, {A, C}) ->
    aeser_del(oracle_sig, {aeser_id:create(account, A), aeser_id:create(contract, C)});
delegate_sign_new(oracle_response, {Q, C}) ->
    aeser_del(oracle_response_sig, {aeser_id:create(oracle, Q), aeser_id:create(contract, C)}).

aeser_del(Fun, {A, B})    -> aeser_delegation:Fun(aec_governance:get_network_id(), A, B);
aeser_del(Fun, {A, B, C}) -> aeser_delegation:Fun(aec_governance:get_network_id(), A, B, C).
