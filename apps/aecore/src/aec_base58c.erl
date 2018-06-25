-module(aec_base58c).

-export([encode/2,
         decode/1,
         safe_decode/2,
         byte_size_for_type/1]).

-type known_type() :: block_hash
                    | block_tx_hash
                    | block_state_hash
                    | channel
                    | contract_pubkey
                    | transaction
                    | tx_hash
                    | oracle_pubkey
                    | oracle_query_id
                    | account_pubkey
                    | signature
                    | name
                    | commitment
                    | peer_pubkey
                    | state
                    | poi.

-type payload() :: binary().
-type encoded() :: binary().

-spec encode(known_type(), payload()) -> encoded().
encode(Type, Payload) ->
    Pfx = type2pfx(Type),
    Enc = base58_check(Payload),
    <<Pfx/binary, "$", Enc/binary>>.

-spec decode(binary()) -> {known_type(), payload()}.
decode(Bin0) ->
    case split(Bin0) of
        [Pfx, Payload] ->
            Type = pfx2type(Pfx),
            Bin = decode_check(Payload),
            case type_size_check(Type, Bin) of
                ok -> {Type, Bin};
                {error, Reason} -> erlang:error(Reason)
            end;
        _ ->
            %% {<<>>, decode_check(Bin)}
            erlang:error(missing_prefix)
    end.

type_size_check(Type, Bin) ->
    case byte_size_for_type(Type) of
        not_applicable -> ok;
        CorrectSize ->
            Size = byte_size(Bin),
            case Size =:= CorrectSize of
                true -> ok;
                false -> {error, incorrect_size}
            end
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
type2pfx(channel)          -> <<"ch">>;
type2pfx(contract_pubkey)  -> <<"ct">>;
type2pfx(transaction)      -> <<"tx">>;
type2pfx(tx_hash)          -> <<"th">>;
type2pfx(oracle_pubkey)    -> <<"ok">>;
type2pfx(oracle_query_id)  -> <<"oq">>;
type2pfx(account_pubkey)   -> <<"ak">>;
type2pfx(signature)        -> <<"sg">>;
type2pfx(commitment)       -> <<"cm">>;
type2pfx(peer_pubkey)      -> <<"pp">>;
type2pfx(name)             -> <<"nm">>;
type2pfx(state)            -> <<"st">>;
type2pfx(poi)              -> <<"pi">>.

pfx2type(<<"bh">>) -> block_hash;
pfx2type(<<"bx">>) -> block_tx_hash;
pfx2type(<<"bs">>) -> block_state_hash;
pfx2type(<<"ch">>) -> channel;
pfx2type(<<"ct">>) -> contract_pubkey;
pfx2type(<<"tx">>) -> transaction;
pfx2type(<<"th">>) -> tx_hash;
pfx2type(<<"ok">>) -> oracle_pubkey;
pfx2type(<<"oq">>) -> oracle_query_id;
pfx2type(<<"ak">>) -> account_pubkey;
pfx2type(<<"sg">>) -> signature;
pfx2type(<<"cm">>) -> commitment;
pfx2type(<<"pp">>) -> peer_pubkey;
pfx2type(<<"nm">>) -> name;
pfx2type(<<"st">>) -> state;
pfx2type(<<"pi">>) -> poi.

-spec byte_size_for_type(known_type()) -> non_neg_integer() | not_applicable.

byte_size_for_type(block_hash)       -> 32;
byte_size_for_type(block_tx_hash)    -> 32;
byte_size_for_type(block_state_hash) -> 32;
byte_size_for_type(channel)          -> 32;
byte_size_for_type(contract_pubkey)  -> 32;
byte_size_for_type(transaction)      -> not_applicable;
byte_size_for_type(tx_hash)          -> 32;
byte_size_for_type(oracle_pubkey)    -> 32;
byte_size_for_type(oracle_query_id)  -> 32;
byte_size_for_type(account_pubkey)   -> 32;
byte_size_for_type(signature)        -> 64;
byte_size_for_type(name)             -> not_applicable;
byte_size_for_type(commitment)       -> 32;
byte_size_for_type(peer_pubkey)      -> 32;
byte_size_for_type(state)            -> 32;
byte_size_for_type(poi)              -> not_applicable.


%% TODO: Fix the base58 module so that it consistently uses binaries instead
%%
binary_to_base58(Bin) ->
    iolist_to_binary(base58:binary_to_base58(Bin)).

base58_to_binary(Bin) when is_binary(Bin) ->
    base58:base58_to_binary(binary_to_list(Bin)).
