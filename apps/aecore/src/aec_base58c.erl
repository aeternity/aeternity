-module(aec_base58c).

-export([encode/2,
         decode/1,
         safe_decode/2]).

-type known_type() :: block_hash
                    | block_tx_hash
                    | block_state_hash
                    | transaction
                    | tx_hash
                    | oracle_pubkey
                    | oracle_query_id
                    | account_pubkey
                    | signature
                    | name
                    | commitment.

-type payload() :: binary().
-type encoded() :: binary().

-spec encode(known_type(), payload()) -> encoded().
encode(Type, Payload) ->
    Pfx = type2pfx(Type),
    Enc = base58_check(Payload),
    <<Pfx/binary, "$", Enc/binary>>.

-spec decode(binary()) -> {known_type(), payload()}.
decode(Bin) ->
    case split(Bin) of
        [Pfx, Payload] ->
            {pfx2type(Pfx), decode_check(Payload)};
        _ ->
            %% {<<>>, decode_check(Bin)}
            erlang:error(missing_prefix)
    end.

safe_decode(Type, Enc) ->
    try decode(Enc) of
        {Type, Dec} ->
            {ok, Dec};
        {_, _} ->
            {error, invalid_prefix}
    catch
        error:_ ->
            {error, invalid_encoding}
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
    <<C:32/bitstring,_/binary>> =
        aec_hash:sha256_hash(aec_hash:sha256_hash(Bin)),
    C.


type2pfx(block_hash)       -> <<"bh">>;
type2pfx(block_tx_hash)    -> <<"bx">>;
type2pfx(block_state_hash) -> <<"bs">>;
type2pfx(transaction)      -> <<"tx">>;
type2pfx(tx_hash)          -> <<"th">>;
type2pfx(oracle_pubkey)    -> <<"ok">>;
type2pfx(oracle_query_id)  -> <<"oq">>;
type2pfx(account_pubkey)   -> <<"ak">>;
type2pfx(signature)        -> <<"sg">>;
type2pfx(commitment)       -> <<"cm">>;
type2pfx(name)             -> <<"nm">>;
type2pfx(node_id)          -> <<"ni">>.

pfx2type(<<"bh">>) -> block_hash;
pfx2type(<<"bx">>) -> block_tx_hash;
pfx2type(<<"bs">>) -> block_state_hash;
pfx2type(<<"tx">>) -> transaction;
pfx2type(<<"th">>) -> tx_hash;
pfx2type(<<"ok">>) -> oracle_pubkey;
pfx2type(<<"oq">>) -> oracle_query_id;
pfx2type(<<"ak">>) -> account_pubkey;
pfx2type(<<"sg">>) -> signature;
pfx2type(<<"cm">>) -> commitment;
pfx2type(<<"nm">>) -> name;
pfx2type(<<"ni">>) -> node_id.

%% TODO: Fix the base58 module so that it consistently uses binaries instead
%%
binary_to_base58(Bin) ->
    iolist_to_binary(base58:binary_to_base58(Bin)).

base58_to_binary(Bin) when is_binary(Bin) ->
    base58:base58_to_binary(binary_to_list(Bin)).
