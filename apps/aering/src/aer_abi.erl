%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Encode and decode data and function calls according to
%%%     Ring-AEVM-ABI.
%%% @end
%%% Created : 25 Jan 2018
%%%
%%%-------------------------------------------------------------------
-module(aer_abi).
-define(HASH_SIZE, 32).

-export([create_calldata/3]).

create_calldata(Contract, Function, Arguments) ->
    Arity = length(Arguments),
    %% TODO: Verify that Function/Arity exists in Contract
    _ArgumentTypes = get_types(Arguments),
    %% TODO: Verify that the types matches the function signature.
    FunctionHandle = encode_function(Contract, Function, Arity),
    EncodedArguments = encode_arguments(Arguments, Arity),
    <<FunctionHandle/binary,
      EncodedArguments/binary>>.

encode_function(_Contract,_Function,_Arity) ->
    %% TODO: Get function encoding fom contract
    %%       or use hash of function name.
    <<0:?HASH_SIZE/unit:8>>.

get_types(Arguments) ->
    [get_type(A) || A <- Arguments].

%% TODO: Handle all ring data types.
get_type(I) when is_integer(I) -> int.

encode_arguments(Arguments, Arity) ->
    encode_arguments(Arguments, Arity, Arity, <<>>, <<>>).

encode_arguments([Argument|Arguments], Arity, Mempos, Stack, Mem) ->
    case encode_data(Argument) of
        <<X:256>> -> encode_arguments(Arguments, Arity -1, Mempos,
                                      <<Stack/binary, X:256>>,
                                      Mem);
        <<X:256, Data/binary>> ->
            DataSize = size(Data),
            encode_arguments(Arguments, Arity-1,
                             Mempos+DataSize,
                             <<Stack/binary, X:256>>,
                             <<Mem/binary, Data/binary>>)
    end;
encode_arguments([], 0, _Mempos, Stack, Mem) ->
    <<Stack/binary, Mem/binary>>.

encode_data(I) when is_integer(I) -> <<I:256>>.
