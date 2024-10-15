%%% @doc Pretty-print an Erlang term, formatting binaries and flags nicely
%%% @end
-module(hctest).

%% API
-export([pp/1]).

pp(T) when is_tuple(T) ->
    list_to_tuple([pp(X) || X <- tuple_to_list(T)]);
pp(L) when is_list(L) ->
    case io_lib:char_list(L) of
        true -> [$", L, $"];
        false -> [$[, string:join([pp(X) || X <- L], ", "), $]]
    end;
% flags!
pp(F) when is_binary(F), size(F) =:= 4 ->
    ["flags:", pp(F)];
pp(B) when is_binary(B), size(B) =/= 4 ->
    ["<<", bin_to_hex(B), ">>"];
pp(I) when is_integer(I) ->
    integer_to_list(I);
pp(F) when is_float(F) ->
    float_to_list(F);
pp(A) when is_atom(A) ->
    [$', atom_to_list(A), $'];
pp(Other) ->
    Other.

bin_to_hex(Bin) ->
    Bits = bit_size(Bin),
    <<Int:Bits>> = Bin,
    io:format("~.16B\n", [Int]).
