%%% @doc Pretty-print an Erlang term, formatting binaries and flags nicely
%%% @end
-module(hctest).

%% API
-export([pp/1, format/2]).

pp(Term) ->
    iolist_to_binary(pp_1(Term)).

pp_1(T) when is_tuple(T) ->
    Contents = lists:join(", ", [pp_1(X) || X <- tuple_to_list(T)]),
    [${, Contents, $}];
pp_1(L) when is_list(L) ->
    case io_lib:char_list(L) of
        true ->
            [$", L, $"];
        false ->
            Contents = lists:join(", ", lists:map(fun pp_1/1, L)),
            [$[, Contents, $]]
    end;
% flags!
pp_1(<<Flags:32>>) ->
    io_lib:format("~.16X", [Flags, "0x"]);
pp_1(B) when is_binary(B) ->
    ["<<", bin_to_hex(B), ">>"];
pp_1(I) when is_integer(I) ->
    integer_to_list(I);
pp_1(F) when is_float(F) ->
    float_to_list(F);
pp_1(A) when is_atom(A) ->
    [$', atom_to_list(A), $'];
pp_1(M) when is_map(M) ->
    KeysValues = lists:join(", ", [[pp_1(K), " => ", pp_1(V)] || {K, V} <- maps:to_list(M)]),
    [
        "#{",
        KeysValues,
        $}
    ];
pp_1(Other) ->
    erlang:error({cant_pretty_print, Other}).

bin_to_hex(Bin) ->
    Bits = bit_size(Bin),
    <<Int:Bits>> = Bin,
    io_lib:format("~.16B", [Int]).

format(Format, Args) ->
    iolist_to_binary(io_lib:format(Format, Args)).
