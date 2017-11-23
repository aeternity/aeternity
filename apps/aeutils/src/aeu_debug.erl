-module(aeu_debug).

-export([pp/1]).

pp(X) ->
    try pp_(X)
    catch error:Err -> {'PP_ERROR', Err, X} end.

pp_(B) when is_tuple(B), element(1, B) == block ->
    try
        H  = aec_blocks:height(B),
        RH = aec_blocks:root_hash(B),
        PH = aec_blocks:prev_hash(B),
        lists:flatten(
          io_lib:fwrite("#block{height=~w, root_hash=~s, prev_hash=~s}",
                        [H, pp_bin2str(RH), pp_bin2str(PH)]))
    catch error:E ->
            io:fwrite("oops: ~p", [E]),
            B
    end;
pp_(B) when is_binary(B) ->
    pp_bin2str(B);
pp_(T) when is_tuple(T) ->
    list_to_tuple([pp_(E) || E <- tuple_to_list(T)]);
pp_(L) when is_list(L) ->
    [pp_(E) || E <- L];
pp_(M) when is_map(M) -> pp_map(M);
pp_(X) ->
    X.

pp_map(#{} = M) ->
    maps:fold(fun(K,V,Acc) -> Acc#{K => pp(V)} end, #{}, M).


pp_bin2str(X) when byte_size(X) > 12 ->
    Sz = byte_size(X),
    [A,B,C,D] = binary:bin_to_list(X, 0, 4),
    [E,F,G,H] = binary:bin_to_list(X, Sz-4, 4),
    lists:flatten(
      ["<<",
       i2l(A),$,,i2l(B),$,,i2l(C),$,,i2l(D),",...,",
       i2l(E),$,,i2l(F),$,,i2l(G),$,,i2l(H),">>"]);
pp_bin2str(X) ->
    X.


i2l(I) ->
    integer_to_list(I).
