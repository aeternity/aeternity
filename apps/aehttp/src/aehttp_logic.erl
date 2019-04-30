-module(aehttp_logic).

-export([ get_top/0
        , get_top_height/0
        , get_top_hash/0
        , get_header_by_hash/1
        , get_key_header_by_height/1
        , get_key_block_by_hash/1
        , get_key_block_by_height/1
        , get_micro_block_by_hash/1
        , get_block_by_hash/1
        , get_block_latest/0
        , get_block_pending/0
        , get_block_genesis/0
        , get_genesis_hash/0
        , get_top_blocks_time_summary/1
        ]).

-export([ get_account/1
        , get_account_balance/1
        ]).

-export([ version/0
        , revision/0
        ]).

-export([ contract_compile/1
        , contract_compile/2
        , contract_call/4
        , contract_decode_data/2
        , contract_encode_call_data/4
        , sophia_encode_call_data/3
        ]).

-export([peer_pubkey/0]).

-export([ connected_peers/1
        , blocked_peers/0
        ]).

-spec get_top() -> {ok, aec_blocks:block()}.
get_top() ->
    Block = aec_chain:top_block(),
    {ok, Block}.

-spec get_top_height() -> {ok, integer()}.
get_top_height() ->
    TopHeader = aec_chain:top_header(),
    Height = aec_headers:height(TopHeader),
    {ok, Height}.

-spec get_top_hash() -> {ok, binary()}.
get_top_hash() ->
    TopHeader = aec_chain:top_header(),
    {ok, _Hash} = aec_headers:hash_header(TopHeader).

-spec get_header_by_hash(binary()) -> {ok, aec_headers:header()} | {error, header_not_found}.
get_header_by_hash(Hash) ->
    case aec_chain:get_header(Hash) of
        {ok, _Header} = OK -> OK;
        error ->
            {error, header_not_found}
    end.

-spec get_key_header_by_height(integer()) -> {ok, aec_headers:header()} | {error, chain_too_short}.
get_key_header_by_height(Height) ->
    case aec_chain:get_key_header_by_height(Height) of
        {ok, _Header} = OK -> OK;
        {error, chain_too_short} = Err ->
            Err
    end.

-spec get_key_block_by_height(integer()) -> {ok, aec_blocks:block()} |
                                        {error, block_not_found | chain_too_short}.
get_key_block_by_height(Height) ->
    case aec_chain:get_key_block_by_height(Height) of
        {ok, Block} ->
            {ok, Block};
        {error, Msg} = Err when Msg =:= block_not_found orelse
                                Msg =:= chain_too_short ->
            Err
    end.

get_block_genesis() ->
    GenBlock = aec_chain:genesis_block(),
    {ok, GenBlock}.

get_block_latest() ->
    TopBlock = aec_chain:top_block(),
    {ok, TopBlock}.

get_block_pending() ->
    aec_conductor:get_key_block_candidate().

-spec get_block_by_hash(binary()) -> {ok, aec_blocks:block()} |
                                     {error, block_not_found}.
get_block_by_hash(Hash) ->
    case aec_chain:get_block(Hash) of
        {ok, Block} ->
            {ok, Block};
        error ->
            {error, block_not_found}
    end.

-spec get_key_block_by_hash(binary()) -> {ok, aec_blocks:block()} |
                                         {error, block_not_found}.
get_key_block_by_hash(Hash) ->
    case aec_chain:get_block(Hash) of
        {ok, Block} ->
            case aec_blocks:is_key_block(Block) of
                true -> {ok, Block};
                false -> {error, block_not_found}
            end;
        error ->
            {error, block_not_found}
    end.

-spec get_micro_block_by_hash(binary()) -> {ok, aec_blocks:block()} |
                                           {error, block_not_found}.
get_micro_block_by_hash(Hash) ->
    case aec_chain:get_block(Hash) of
        {ok, Block} ->
            case aec_blocks:is_key_block(Block) of
                false -> {ok, Block};
                true -> {error, block_not_found}
            end;
        error ->
            {error, block_not_found}
    end.

-spec get_account(binary()) -> {ok, map()} | {error, account_not_found}.
get_account(Pubkey) ->
    case aec_chain:get_account(Pubkey) of
        {value, Account} -> {ok, Account};
        none -> {error, account_not_found}
    end.

-spec get_account_balance(binary()) -> {ok, integer()}
                                     | {error, account_not_found}.
get_account_balance(Pubkey) when is_binary(Pubkey) ->
    case aec_chain:get_account(Pubkey) of
        {value, A} ->
            {ok, aec_accounts:balance(A)};
        none ->
            {error, account_not_found}
    end.

version() -> {ok, aeu_info:get_version()}.

revision() -> {ok, aeu_info:get_revision()}.

get_genesis_hash() -> {ok, aec_chain:genesis_hash()}.

get_top_blocks_time_summary(Count) ->
    TimeSummary0 = aec_chain:get_top_N_blocks_time_summary(Count),
    TimeSummary =
        lists:foldl(
          fun({Height, Ts, Delta, Difficulty}, Acc) ->
                  [#{height => Height,
                    time => Ts,
                    difficulty => Difficulty,
                    time_delta_to_parent => Delta} | Acc];
            ({Height, Ts, Difficulty}, Acc) ->
                  [#{height => Height,
                    time => Ts,
                    difficulty => Difficulty} | Acc]
          end, [], TimeSummary0),
    {ok, lists:reverse(TimeSummary)}.

contract_compile(Code, _Options) ->
    InFile = tempfile_name("tmp_sophia_", [{ext, ".aes"}]),
    case file:write_file(InFile, Code) of
        ok ->
            contract_compile(InFile);
        {error, _} ->
            {error, <<"Could not write temporary file">>}
    end.

contract_compile(SrcFile) ->
    Compiler = compiler_cmd(),
    OutFile  = tempfile_name("tmp_sophia_", [{ext, ".aeb"}]),
    Cmd = Compiler ++ " " ++ SrcFile ++ " -o " ++ OutFile,
    _Output = os:cmd(Cmd),
    try
        {ok, Bin} = file:read_file(OutFile),
        case binary_to_term(Bin) of
            {ok, ByteCode} ->
                {ok, aect_sophia:serialize(ByteCode)};
            Err = {error, _} ->
                Err
        end
    catch _:_ ->
        {error, <<"Compiler error">>}
    after
        cleanup_tempfiles()
    end.

contract_call(ABI, EncodedCode, Function, Argument) ->
    Call =
        fun(CodeOrAddress, Arg) ->
          case aect_dispatch:call(ABI, CodeOrAddress, Function, Arg) of
              {ok, Result, _} -> {ok, aeser_api_encoder:encode(contract_bytearray, Result)};
              {error, _ErrorMsg} = Err -> Err
          end
        end,
    case ABI of
        <<"sophia-address">> ->
            case aeser_api_encoder:safe_decode(contract_pubkey, EncodedCode) of
                {ok, ContractAddress} ->
                    Call(ContractAddress, Argument);
                _ ->
                    {error, <<"Invalid hash for contract address">>}
            end;
        %% <<"evm">> ->
        %%     case aeser_api_encoder:safe_decode(contract_bytearray, EncodedCode) of
        %%         {ok, Code} ->
        %%             case aeser_api_encoder:safe_decode(contract_bytearray, Argument) of
        %%                 {ok, CallData} -> Call(Code, CallData);
        %%                 {error, _} -> {error, <<"Illegal call data">>}
        %%             end;
        %%         {error, _} ->
        %%             {error, <<"Illegal code">>}
        %%     end;
        _Other ->
            {error, "Unknown/Unsupported ABI"}
    end.

contract_decode_data(Type, Data) ->
    case aeser_api_encoder:safe_decode(contract_bytearray, Data) of
        {error, _} ->
            {error, <<"Data must be a contract bytearray (\"cb_...\")">>};
        {ok, CallData} ->
            try aect_sophia:decode_data(Type, CallData) of
                {ok, _Result} = OK -> OK;
                {error, _ErrorMsg} = Err -> Err
            catch
                _T:_E ->
                    String = io_lib:format("~p:~p ~p", [_T,_E,erlang:get_stacktrace()]),
                    Error = << <<B>> || B <- "Bad argument: " ++ lists:flatten(String) >>,
                    {error, Error}
            end
    end.

contract_encode_call_data(<<"evm">>, _Code, _Function, _Argument) ->
    {error, <<"deprecated">>};
contract_encode_call_data(<<"sophia">>, Code, Function, Argument) ->
    case sophia_encode_call_data(Code, Function, Argument) of
        {ok, ByteCode} -> {ok, aeser_api_encoder:encode(contract_bytearray, ByteCode)};
        {error, _ErrorMsg} = Err -> Err
    end.

sophia_encode_call_data(Code, Function, Argument) ->
    try aect_sophia:deserialize(Code) of
        Contract = #{} ->
            InFile = tempfile_name("tmp_sophia_", [{ext, ".aeb"}]),
            case file:write_file(InFile, term_to_binary(Contract)) of
                ok ->
                    encode_call_data(InFile, Function, Argument);
                {error, _} ->
                    {error, <<"Failed to write temporary file">>}
            end
    catch _:_ ->
        {error, <<"Bad contract code">>}
    end.

encode_call_data(CFile, BinFun, BinArgOrCall)
        when is_binary(BinFun), is_binary(BinArgOrCall) ->
    encode_call_data(CFile, binary_to_list(BinFun), binary_to_list(BinArgOrCall));
encode_call_data(CFile, Function, ArgumentOrCall) ->
    Cmd0 = case Function of
        "" ->
            CallFile = tempfile_name("tmp_sophia_", [{ext, ".aes"}]),
            file:write_file(CallFile, ArgumentOrCall),
            CallFile;
        _ ->
            " --calldata_fun='" ++ Function ++ "' " ++
            " --calldata_args='" ++ ArgumentOrCall ++ "' "
    end,
    OutFile = tempfile_name("tmp_sophia_", [{ext, ".aeb"}]),
    Cmd = compiler_cmd() ++ " -c " ++ CFile ++ " " ++ Cmd0 ++ " -o " ++ OutFile,
    _Output = os:cmd(Cmd),
    try
        {ok, Bin} = file:read_file(OutFile),
        binary_to_term(Bin)
    catch _:_ ->
        {error, <<"Compiler error">>}
    after
        cleanup_tempfiles()
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

compiler_cmd() ->
    filename:join([code:lib_dir(aehttp), "priv", "bin", "aesophia"]).

peer_pubkey() ->
    case aec_keys:peer_pubkey() of
        {ok, _Pubkey} = OK -> OK;
        {error, key_not_found} = Err -> Err
    end.

connected_peers(Tag) -> aec_peers:connected_peers(Tag).

blocked_peers() -> aec_peers:blocked_peers().
