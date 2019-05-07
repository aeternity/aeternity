%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Test utils for contracts
%%% @end
%%%-------------------------------------------------------------------
-module(aect_test_utils).

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
        , compile_contract/1
        , compile_contract/2
        , compile_filename/1
        , compile_filename/2
        , encode_call_data/3
        , encode_call_data/4
        , decode_data/2
        , assert_state_equal/2
        , get_oracle_queries/2
        , dummy_bytecode/0
        , latest_sophia_abi_version/0
        , latest_sophia_vm_version/0
        , latest_protocol_version/0
        ]).

-include("../include/aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("include/aect_sophia_vsn.hrl").
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
        ?FORTUNA_PROTOCOL_VSN -> ?VM_AEVM_SOPHIA_3
    end.

latest_sophia_abi_version() ->
    case latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> ?ABI_AEVM_SOPHIA_1;
        ?MINERVA_PROTOCOL_VSN -> ?ABI_AEVM_SOPHIA_1;
        ?FORTUNA_PROTOCOL_VSN -> ?ABI_AEVM_SOPHIA_1
    end.

latest_sophia_version() ->
    case latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> ?SOPHIA_ROMA;
        ?MINERVA_PROTOCOL_VSN -> ?SOPHIA_MINERVA;
        ?FORTUNA_PROTOCOL_VSN -> ?SOPHIA_FORTUNA_AEVM
    end.

latest_protocol_version() ->
    lists:last(aec_hard_forks:sorted_protocol_versions()).

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
     , vm_version  => latest_sophia_vm_version()
     , abi_version => latest_sophia_abi_version()
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
                            compiler_version => Version}
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
     , abi_version => latest_sophia_abi_version()
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
    file:read_file(contract_filename(aevm, Name)).

contract_filename(Type, Name) ->
    CodeDir = filename:join(code:lib_dir(aecontract), "../../extras/test/"),
    case Type of
        aevm -> filename:join(CodeDir, filename:rootname(Name, ".aes") ++ ".aes");
        fate -> filename:join(CodeDir, filename:rootname(Name, ".fate") ++ ".fate")
    end.

compile_filename(FileName) ->
    compile(latest_sophia_version(), FileName).

compile_filename(Compiler, FileName) ->
    compile(Compiler, FileName).

compile_contract(File) ->
    compile_contract(latest_sophia_version(), File).

compile_contract(?SOPHIA_FORTUNA_FATE, File) ->
    compile_filename(?SOPHIA_FORTUNA_FATE, contract_filename(fate, File));
compile_contract(Compiler, File) ->
    compile_filename(Compiler, contract_filename(aevm, File)).

compile(?SOPHIA_FORTUNA_FATE, File) ->
    {ok, AsmBin} = file:read_file(File),
    Source = binary_to_list(AsmBin),
    {_Env, ByteCode} = aeb_fate_asm:asm_to_bytecode(Source, []),
    {ok, aect_sophia:serialize(#{ byte_code => ByteCode
                                , contract_source => Source %% TODO: This is wrong.
                                , compiler_version => 1     %% TODO: This is wrong.
                                , type_info => []           %% TODO: This is wrong.
                                })};
compile(?SOPHIA_FORTUNA_AEVM, File) ->
    {ok, ContractBin} = file:read_file(File),
    case aeso_compiler:from_string(binary_to_list(ContractBin), []) of
        {ok, Map}        -> {ok, aect_sophia:serialize(Map)};
        {error, _} = Err -> Err
    end;
compile(LegacyVersion, File) ->
    case legacy_compile(LegacyVersion, File) of
        {ok, Code}      -> {ok, Code};
        {error, Reason} -> {error, {compiler_error, File, Reason}}
    end.

new_key_pair() ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    {PubKey, PrivKey}.

legacy_compile(Vsn, SrcFile) ->
    Compiler = compiler_cmd(Vsn),
    OutFile  = tempfile_name("tmp_sophia_", [{ext, ".aeb"}]),
    Cmd = Compiler ++ " " ++ SrcFile ++ " -o " ++ OutFile,
    _Output = os:cmd(Cmd),
    try
        {ok, Bin} = file:read_file(OutFile),
        aeser_api_encoder:safe_decode(contract_bytearray, Bin)
    catch _:_ ->
        {error, <<"Compiler error">>}
    after
        cleanup_tempfiles()
    end.

compiler_cmd(Vsn) ->
    BaseDir = filename:join([code:priv_dir(aesophia_cli), "bin"]),
    case Vsn of
        ?SOPHIA_ROMA    -> filename:join([BaseDir, "v1.4.0", "aesophia_cli"]);
        ?SOPHIA_MINERVA -> filename:join([BaseDir, "v2.1.0", "aesophia_cli"])
    end.

tempfile_name(Prefix, Opts) ->
    File = tempfile:name(Prefix, Opts),
    case get('$tmp_files') of
        undefined -> put('$tmp_files', [File]);
        Files     -> put('$tmp_files', [File | Files])
    end,
    File.

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
    encode_call_data(latest_sophia_version(), Code, Fun, Args).

encode_call_data(?SOPHIA_FORTUNA_AEVM, Code, Fun, Args) ->
    try aeso_compiler:create_calldata(to_str(Code), to_str(Fun),
                                      lists:map(fun to_str/1, Args)) of
        {error, _} = Err -> Err;
        {ok, Data,_DataType,_OutType} when is_binary(Data) ->
            {ok, Data}
    catch _T:_E ->
        {error, <<"bad argument">>}
    end;
encode_call_data(_LegacyVsn, Code, Fun, Args0) ->
    SrcFile = tempfile_name("sophia_code", [{ext, ".aes"}]),
    Args    = legacy_args(Args0),
    ok = file:write_file(SrcFile, Code),
    Compiler = compiler_cmd(?SOPHIA_MINERVA),
    Esc = fun(Str) -> lists:flatten(string:replace(string:replace(Str, "\\", "\\\\", all), "\"", "\\\"", all)) end,
    Cmd = Compiler ++ " --create_calldata " ++ contract_filename(aevm, SrcFile) ++
          " --calldata_fun " ++ to_str(Fun) ++ " --calldata_args \"" ++
          string:join(lists:map(Esc, lists:map(fun to_str/1, Args)), ", ") ++ "\"",
    Output = os:cmd(Cmd),
    try
        [_, CalldataStr] = string:lexemes(Output, "\n"),
        aeser_api_encoder:safe_decode(contract_bytearray, list_to_binary(CalldataStr))
    catch _:_ ->
        {error, <<"Compiler error">>}
    after
        cleanup_tempfiles()
    end.

decode_data(Type, <<"cb_", _/binary>> = EncData) ->
    case aeser_api_encoder:safe_decode(contract_bytearray, EncData) of
        {ok, Data} ->
            decode_data_(Type, Data);
        Err = {error, _} ->
            Err
    end;
decode_data(Type, Data) ->
    decode_data_(Type, Data).

decode_data_(fate, Data) ->
    try {ok, aefate_test_utils:decode(aeb_fate_encoding:deserialize(Data))}
    catch _:_ -> {error, <<"bad fate data">>}
    end;
decode_data_(Type, Data) ->
    case get_type(Type) of
        {ok, SophiaType} ->
            try aeb_heap:from_binary(SophiaType, Data) of
                {ok, Term} ->
                    try aect_sophia:prepare_for_json(SophiaType, Term) of
                        R -> {ok, R}
                    catch throw:R -> R
                    end;
                {error, _} -> {error, <<"bad type/data">>}
            catch _T:_E ->    {error, <<"bad argument">>}
            end;
        {error, _} = E -> E
    end.

get_type(Type) ->
    case aeso_compiler:sophia_type_to_typerep(to_str(Type)) of
        {ok, _Type} = R -> R;
        {error, ErrorAtom} ->
            {error, unicode:characters_to_binary(atom_to_list(ErrorAtom))}
    end.

%% Convert to old style hex literals.
legacy_args(Args) ->
    lists:map(fun legacy_arg/1, Args).

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
