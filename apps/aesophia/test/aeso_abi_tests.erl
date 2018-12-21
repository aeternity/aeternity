-module(aeso_abi_tests).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

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
    {ok, {error, circular_references}}   = ?SANDBOX(aeso_heap:from_binary({list, word}, CircularList)),
    {ok, {error, {binary_too_short, _}}} = ?SANDBOX(aeso_heap:from_binary(word, <<1, 2, 3, 4>>)),
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
    {42} = encode_decode_sophia_string("int", "42"),
    {1} = encode_decode_sophia_string("bool", "true"),
    {0} = encode_decode_sophia_string("bool", "false"),
    {<<"Hello">>} = encode_decode_sophia_string("string", "\"Hello\""),
    {<<"Hello">>, [1,2,3], {variant, 1, [1]}} =
        encode_decode_sophia_string(
          "(string, list(int), option(bool))",
          "\"Hello\", [1,2,3], Some(true)"),
    ok.

encode_decode_sophia_string(SophiaType, String) ->
    io:format("String ~p~n", [String]),
    Code = [ "contract Call =\n"
           , "  function foo : ", SophiaType, " => _\n"
           , "  function __call() = foo(", String, ")\n" ],
    {ok, _, {Types, _}, Args} = aeso_compiler:check_call(lists:flatten(Code), []),
    Arg  = list_to_tuple(Args),
    Type = {tuple, Types},
    io:format("Type ~p~n", [Type]),
    Data = encode(Arg),
    decode(Type, Data).

encode_decode(T, D) ->
    ?assertEqual(D, decode(T, encode(D))),
    D.

encode(D) ->
    aeso_heap:to_binary(D).

decode(T,B) ->
    {ok, D} = aeso_heap:from_binary(T, B),
    D.
