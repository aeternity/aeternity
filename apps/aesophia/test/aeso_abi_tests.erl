-module(aeso_abi_tests).

-include_lib("eunit/include/eunit.hrl").

encode_call_with_integer_test() ->
    [64, 128, 42, 4, "main"] =
        aeso_test_utils:dump_words(
            aeso_abi:create_calldata("", "main", "42")).

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
