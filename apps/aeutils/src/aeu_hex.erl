%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Helper functions for passing binary data as strings of hex nibbles.
%%% The hex nibbles can be prefixed with "0x" and binary called hexstring
%%% or lists of bytes not prefixed and just called hex.
%%% Some examples
%%% ```
%%%  Binary      | Hex      | HexString
%%%  <<>>        | ""       | <<"0x">>
%%%  << 0 >>     | "00"     | <<"0x00">>
%%%  << 0, 255>> | "00ff"   | <<"0x00ff">>
%%% '''
%%%
%%% The original implementation of bin_to_hex and hex_to_bin comes from
%%% http://necrobious.blogspot.com/2008/03/binary-to-hex-string-back-to-binary-in.html
%%% @end
%%%-------------------------------------------------------------------
-module(aeu_hex).
-export([ bin_to_hex/1
	, hexstring_decode/1
	, hexstring_encode/1
	, hex_to_bin/1]).

-spec bin_to_hex(binary()) -> [byte()].
bin_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

-spec hex_to_bin([byte()]) -> binary().
hex_to_bin(S) ->
    hex_to_bin(S, []).
hex_to_bin([], Acc) ->
    list_to_binary(lists:reverse(Acc));
hex_to_bin([X,Y|T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
    hex_to_bin(T, [V | Acc]).

-spec hexstring_encode(binary()) -> binary().
hexstring_encode(Binary) ->
    BinaryAsHexString = 
        << << (hex_nibble(X)):8, (hex_nibble(Y)):8 >>
           || <<X:4, Y:4>> <= Binary >>,
    <<"0x", BinaryAsHexString/binary >>.
    
-spec hexstring_decode(binary()) -> binary() .
hexstring_decode(Binary) ->
    case Binary of
        <<"0x", BinaryAsHexString/binary >> ->
            CharacterCnt = byte_size(BinaryAsHexString),
            case CharacterCnt rem 2 of
                0 when CharacterCnt > 0 -> pass;
                _ -> throw(invalid_hex_string)
            end,
            << << (hex_to_int(X)):4, (hex_to_int(Y)):4 >>
                  || <<X:8, Y:8>> <= BinaryAsHexString >>;
	<<"0x">> -> <<>>;
	<<>> -> <<>>;
        _ -> throw(invalid_hex_string)
    end.

hex_nibble(X) ->
    if X < 10 -> X+$0;
       true   -> X+87
    end.

hex_to_int(X) when $A =< X, X =< $F -> 10 + X - $A;
hex_to_int(X) when $a =< X, X =< $f -> 10 + X - $a;
hex_to_int(X) when $0 =< X, X =< $9 -> X - $0.
