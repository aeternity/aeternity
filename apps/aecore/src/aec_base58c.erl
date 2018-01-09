-module(aec_base58c).

-export([encode/2,
         decode/1]).

-type known_type() :: block_hash
                    | block_tx_hash
                    | transaction
                    | oracle_pubkey
                    | account_pubkey.

-type payload() :: binary().
-type encoded() :: binary().

-spec encode(known_type() | binary(), payload()) -> encoded().
encode(Type, Payload) ->
    Pfx = type2pfx(Type),
    Enc = base58_check(Payload),
    <<Pfx/binary, "$", Enc/binary>>.

-spec decode(binary()) -> {known_type() | encoded(), payload()}.
decode(Bin) ->
    case split(Bin) of
        [Pfx, Payload] ->
            {pfx2type(Pfx), decode_check(Payload)};
        _ ->
            {<<>>, decode_check(Bin)}
    end.

decode_check(Bin) ->
    Dec = base58_to_binary(Bin),
    Sz = byte_size(Dec),
    BSz = Sz - 4,
    <<Body:BSz/binary, C:4/binary>> = Dec,
    C = check_str(Body),
    Body.

%% modified from github.com/mbrix/lib_hd
base58_check(Bin) ->
    C = check_str(Bin),
    binary_to_base58(iolist_to_binary([Bin, C])).

split(Bin) ->
    binary:split(Bin, [<<"$">>], []).

check_str(Bin) ->
    <<C:32/bitstring,_/binary>> = crypto:hash(sha256,
                                              crypto:hash(sha256, Bin)),
    C.


type2pfx(block_hash)     -> <<"bh">>;
type2pfx(block_tx_hash)  -> <<"bx">>;
type2pfx(transaction)    -> <<"tx">>;
type2pfx(oracle_pubkey)  -> <<"ok">>;
type2pfx(account_pubkey) -> <<"ak">>;
type2pfx(Other)          -> Other.

pfx2type(<<"bh">>) -> block_hash;
pfx2type(<<"bx">>) -> block_tx_hash;
pfx2type(<<"tx">>) -> transaction;
pfx2type(<<"ok">>) -> oracle_pubkey;
pfx2type(<<"ak">>) -> account_pubkey;
pfx2type(Other)    -> Other.


%% TODO: Fix the base58 module so that it consistently uses binaries instead
%%
binary_to_base58(Bin) ->
    iolist_to_binary(base58:binary_to_base58(Bin)).

base58_to_binary(Bin) when is_binary(Bin) ->
    base58:base58_to_binary(binary_to_list(Bin)).
