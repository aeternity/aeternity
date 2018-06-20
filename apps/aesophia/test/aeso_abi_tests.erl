-module(aeso_abi_tests).

-include_lib("eunit/include/eunit.hrl").

encode_call_with_integer_test() ->
    [64, 128, 192, 4, "main", 42] =
        aeso_test_utils:dump_words(
            aeso_abi:create_calldata("", "main", "(42)")).

-define(SANDBOX(Code), sandbox(fun() -> Code end)).

sandbox(Code) ->
    Parent = self(),
    Tag    = make_ref(),
    {Pid, Ref} = spawn_monitor(fun() -> Parent ! {Tag, Code()} end),
    receive
        {Tag, Res} -> erlang:demonitor(Ref, [flush]), {ok, Res};
        {'DOWN', Ref, process, Pid, Reason} -> {error, Reason}
    after 100 ->
        exit(Pid, kill),
        {error, loop}
    end.

malicious_from_binary_test() ->
    CircularList = from_words([32, 1, 32]), %% Xs = 1 :: Xs
    {ok, {error, circular_references}}   = ?SANDBOX(aeso_data:from_binary({list, word}, CircularList)),
    {ok, {error, {binary_too_short, _}}} = ?SANDBOX(aeso_data:from_binary(word, <<1, 2, 3, 4>>)),
    ok.

from_words(Ws) ->
    << <<(from_word(W))/binary>> || W <- Ws >>.

from_word(W) when is_integer(W) ->
    <<W:256>>;
from_word(S) when is_list(S) ->
    Len = length(S),
    Bin = <<(list_to_binary(S))/binary, 0:(32 - Len)/unit:8>>,
    <<Len:256, Bin/binary>>.

encode_decode_test() ->
    encode_decode(word, 42),
    42 = encode_decode(word, 42),
    -1 = encode_decode(signed_word, -1),
    <<"Hello world">> = encode_decode(string, <<"Hello world">>),
    {} = encode_decode({tuple, []}, {}),
    {42} = encode_decode({tuple, [word]}, {42}),
    {42, 0} = encode_decode({tuple, [word, word]}, {42, 0}),
    [] = encode_decode({list, word}, []),
    [32] = encode_decode({list, word}, [32]),
    none = encode_decode({option, word}, none),
    {some, 1} = encode_decode({option, word}, {some, 1}),
    string = encode_decode(typerep, string),
    word = encode_decode(typerep, word),
    {list, word} = encode_decode(typerep, {list, word}),
    {tuple, [word]} = encode_decode(typerep, {tuple, [word]}),
    1 = encode_decode(word, 1),
    0 = encode_decode(word, 0),
    ok.

encode_decode_sophia_test() ->
    42 = encode_decode_sophia_string("42"),
    1 = encode_decode_sophia_string("true"),
    0 = encode_decode_sophia_string("false"),
    <<"Hello">> = encode_decode_sophia_string("\"Hello\""),
    {<<"Hello">>, [1,2,3], {some, 1}} =
        encode_decode_sophia_string(
          "(\"Hello\", [1,2,3], Some(true))"),
    ok.

encode_decode_sophia_string(String) ->
    io:format("String ~p~n", [String]),
    {ok, AstType} = aeso_constants:get_type(String),
    io:format("AstType ~p~n", [AstType]),
    {ok, Type} = aeso_data:sophia_type_to_typerep(AstType),
    io:format("Type ~p~n", [Type]),
    Data = aeso_abi:create_calldata(<<>>, "foo", String),
    io:format("Data ~p~n", [Data]),
    case Type of
        {tuple, _} ->
            {<<"foo">>, R} = decode({tuple, [string, Type]}, Data),
            R;
        _ ->
            {<<"foo">>, {R}} = decode({tuple, [string, {tuple, [Type]}]}, Data),
            R
    end.


encode_decode(T, D) ->
    D = decode(T, encode(D)).

encode(D) ->
    aeso_data:to_binary(D).

decode(T,B) ->
    {ok, D} = aeso_data:from_binary(32, T, B),
    D.
