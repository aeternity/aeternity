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

-export([ contract_compile/2
        , contract_call/4
        , contract_decode_data/2
        , contract_encode_call_data/4
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

contract_compile(Code, Options) ->
    case aect_sophia:compile(Code, Options) of
          {ok, ByteCode} -> {ok, ByteCode};
          {error, _ErrorMsg} = Err -> Err
    end.

contract_call(ABI, EncodedCode, Function, Argument) ->
    Call =
        fun(CodeOrAddress, Arg) ->
          case aect_dispatch:call(ABI, CodeOrAddress, Function, Arg) of
              {ok, Result} -> {ok, aec_base58c:encode(contract_bytearray, Result)};
              {error, _ErrorMsg} = Err -> Err
          end
        end,
    case ABI of
        <<"sophia-address">> ->
            case aec_base58c:safe_decode(contract_pubkey, EncodedCode) of
                {ok, ContractAddress} ->
                    Call(ContractAddress, Argument);
                _ ->
                    {error, <<"Invalid hash for contract address">>}
            end;
        <<"sophia">> ->
            case aec_base58c:safe_decode(contract_bytearray, EncodedCode) of
                {ok, Code} -> Call(Code, Argument);
                {error, _} -> {error, <<"Illegal code">>}
            end;
        <<"evm">> ->
            case aec_base58c:safe_decode(contract_bytearray, EncodedCode) of
                {ok, Code} ->
                    case aec_base58c:safe_decode(contract_bytearray, Argument) of
                        {ok, CallData} -> Call(Code, CallData);
                        {error, _} -> {error, <<"Illegal call data">>}
                    end;
                {error, _} ->
                    {error, <<"Illegal code">>}
            end;
        _Other ->
            {error, "Unknown ABI"}
    end.

contract_decode_data(Type, Data) ->
    case aec_base58c:safe_decode(contract_bytearray, Data) of
        {error, _} ->
            {error, <<"Data must be hex encoded">>};
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



contract_encode_call_data(ABI, Code, Function, Argument) ->
    case aect_dispatch:encode_call_data(ABI, Code, Function, Argument) of
        {ok, ByteCode} -> {ok, aec_base58c:encode(contract_bytearray, ByteCode)};
        {error, _ErrorMsg} = Err -> Err
    end.

peer_pubkey() ->
    case aec_keys:peer_pubkey() of
        {ok, _Pubkey} = OK -> OK;
        {error, key_not_found} = Err -> Err
    end.

connected_peers(Tag) -> aec_peers:connected_peers(Tag).

blocked_peers() -> aec_peers:blocked_peers().
