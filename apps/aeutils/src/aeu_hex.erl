%from http://necrobious.blogspot.com/2008/03/binary-to-hex-string-back-to-binary-in.html

-module(aeu_hex).
-export([bin_to_hex/1,hex_to_bin/1]).

bin_to_hex(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

hex_to_bin(S) ->
  hex_to_bin(S, []).
hex_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hex_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hex_to_bin(T, [V | Acc]).
