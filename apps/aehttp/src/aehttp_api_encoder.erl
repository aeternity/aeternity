-module(aehttp_api_encoder).

-export([encode/2,
         decode/1,
         safe_decode/2,
         byte_size_for_type/1]).

-export_type([encoded/0]).

-type known_type() :: key_block_hash
                    | micro_block_hash
                    | block_pof_hash
                    | block_tx_hash
                    | block_state_hash
                    | channel
                    | contract_pubkey
                    | transaction
                    | tx_hash
                    | oracle_pubkey
                    | oracle_query
                    | oracle_query_id
                    | oracle_response
                    | account_pubkey
                    | signature
                    | name
                    | commitment
                    | peer_pubkey
                    | state
                    | poi
                    | state_trees
                    | call_state_tree.

-type extended_type() :: known_type() | block_hash | {id_hash, [known_type()]}.


-type payload() :: binary().
-type encoded() :: binary().

-define(BASE58, 1).
-define(BASE64, 2).

-spec encode(known_type(), payload() | aec_id:id()) -> encoded().
encode(id_hash, Payload) ->
    {IdType, Val} = aec_id:specialize(Payload),
    encode(id2type(IdType), Val);
encode(Type, Payload) ->
    Pfx = type2pfx(Type),
    Enc = case type2enc(Type) of
              ?BASE58 -> base58_check(Payload);
              ?BASE64 -> base64_check(Payload)
          end,
    <<Pfx/binary, "_", Enc/binary>>.

-spec decode(binary()) -> {known_type(), payload()}.
decode(Bin0) ->
    case split(Bin0) of
        [Pfx, Payload] ->
            Type = pfx2type(Pfx),
            Bin = decode_check(Type, Payload),
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

-spec safe_decode(extended_type(), encoded()) -> {'ok', payload() | aec_id:id()}
                                                     | {'error', atom()}.
safe_decode({id_hash, AllowedTypes}, Enc) ->
    try decode(Enc) of
        {ActualType, Dec} ->
            case lists:member(ActualType, AllowedTypes) of
                true ->
                    try {ok, aec_id:create(type2id(ActualType), Dec)}
                    catch error:_ -> {error, invalid_prefix}
                    end;
                false ->
                    {error, invalid_prefix}
            end
    catch
        error:_ ->
            {error, invalid_encoding}
    end;
safe_decode(block_hash, Enc) ->
    try decode(Enc) of
        {key_block_hash, Dec} ->
            {ok, Dec};
        {micro_block_hash, Dec} ->
            {ok, Dec};
        {_, _} ->
            {error, invalid_prefix}
    catch
        error:_ ->
            {error, invalid_encoding}
    end;
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

decode_check(Type, Bin) ->
    Dec =
        case type2enc(Type) of
            ?BASE58 -> base58_to_binary(Bin);
            ?BASE64 -> base64_to_binary(Bin)
        end,
    Sz = byte_size(Dec),
    BSz = Sz - 4,
    <<Body:BSz/binary, C:4/binary>> = Dec,
    C = check_str(Body),
    Body.

base64_check(Bin) ->
    C = check_str(Bin),
    binary_to_base64(iolist_to_binary([Bin, C])).

%% modified from github.com/mbrix/lib_hd
base58_check(Bin) ->
    C = check_str(Bin),
    binary_to_base58(iolist_to_binary([Bin, C])).

split(Bin) ->
    binary:split(Bin, [<<"_">>], []).

check_str(Bin) ->
    <<C:32/bitstring,_/binary>> =
        aec_hash:sha256_hash(aec_hash:sha256_hash(Bin)),
    C.

id2type(account)    -> account_pubkey;
id2type(channel)    -> channel;
id2type(commitment) -> commitment;
id2type(contract)   -> contract_pubkey;
id2type(name)       -> name;
id2type(oracle)     -> oracle_pubkey.

type2id(account_pubkey)  -> account;
type2id(channel)         -> channel;
type2id(commitment)      -> commitment;
type2id(contract_pubkey) -> contract;
type2id(name)            -> name;
type2id(oracle_pubkey)   -> oracle.

type2enc(key_block_hash)   -> ?BASE58;
type2enc(micro_block_hash) -> ?BASE58;
type2enc(block_pof_hash)   -> ?BASE58;
type2enc(block_tx_hash)    -> ?BASE58;
type2enc(block_state_hash) -> ?BASE58;
type2enc(channel)          -> ?BASE58;
type2enc(contract_pubkey)  -> ?BASE58;
type2enc(contract_bytearray)-> ?BASE64;
type2enc(transaction)      -> ?BASE64;
type2enc(tx_hash)          -> ?BASE58;
type2enc(oracle_pubkey)    -> ?BASE58;
type2enc(oracle_query)     -> ?BASE64;
type2enc(oracle_query_id)  -> ?BASE58;
type2enc(oracle_response)  -> ?BASE64;
type2enc(account_pubkey)   -> ?BASE58;
type2enc(signature)        -> ?BASE58;
type2enc(commitment)       -> ?BASE58;
type2enc(peer_pubkey)      -> ?BASE58;
type2enc(name)             -> ?BASE58;
type2enc(state)            -> ?BASE64;
type2enc(poi)              -> ?BASE64;
type2enc(state_trees)      -> ?BASE64;
type2enc(call_state_tree)  -> ?BASE64.


type2pfx(key_block_hash)   -> <<"kh">>;
type2pfx(micro_block_hash) -> <<"mh">>;
type2pfx(block_pof_hash)   -> <<"bf">>;
type2pfx(block_tx_hash)    -> <<"bx">>;
type2pfx(block_state_hash) -> <<"bs">>;
type2pfx(channel)          -> <<"ch">>;
type2pfx(contract_pubkey)  -> <<"ct">>;
type2pfx(contract_bytearray)-> <<"cb">>;
type2pfx(transaction)      -> <<"tx">>;
type2pfx(tx_hash)          -> <<"th">>;
type2pfx(oracle_pubkey)    -> <<"ok">>;
type2pfx(oracle_query)     -> <<"ov">>;
type2pfx(oracle_query_id)  -> <<"oq">>;
type2pfx(oracle_response)  -> <<"or">>;
type2pfx(account_pubkey)   -> <<"ak">>;
type2pfx(signature)        -> <<"sg">>;
type2pfx(commitment)       -> <<"cm">>;
type2pfx(peer_pubkey)      -> <<"pp">>;
type2pfx(name)             -> <<"nm">>;
type2pfx(state)            -> <<"st">>;
type2pfx(poi)              -> <<"pi">>;
type2pfx(state_trees)      -> <<"ss">>;
type2pfx(call_state_tree)  -> <<"cs">>.

pfx2type(<<"kh">>) -> key_block_hash;
pfx2type(<<"mh">>) -> micro_block_hash;
pfx2type(<<"bf">>) -> block_pof_hash;
pfx2type(<<"bx">>) -> block_tx_hash;
pfx2type(<<"bs">>) -> block_state_hash;
pfx2type(<<"ch">>) -> channel;
pfx2type(<<"cb">>) -> contract_bytearray;
pfx2type(<<"ct">>) -> contract_pubkey;
pfx2type(<<"tx">>) -> transaction;
pfx2type(<<"th">>) -> tx_hash;
pfx2type(<<"ok">>) -> oracle_pubkey;
pfx2type(<<"ov">>) -> oracle_query;
pfx2type(<<"oq">>) -> oracle_query_id;
pfx2type(<<"or">>) -> oracle_response;
pfx2type(<<"ak">>) -> account_pubkey;
pfx2type(<<"sg">>) -> signature;
pfx2type(<<"cm">>) -> commitment;
pfx2type(<<"pp">>) -> peer_pubkey;
pfx2type(<<"nm">>) -> name;
pfx2type(<<"st">>) -> state;
pfx2type(<<"pi">>) -> poi;
pfx2type(<<"ss">>) -> state_trees;
pfx2type(<<"cs">>) -> call_state_tree.

-spec byte_size_for_type(known_type()) -> non_neg_integer() | not_applicable.

byte_size_for_type(key_block_hash)   -> 32;
byte_size_for_type(micro_block_hash) -> 32;
byte_size_for_type(block_pof_hash)   -> 32;
byte_size_for_type(block_tx_hash)    -> 32;
byte_size_for_type(block_state_hash) -> 32;
byte_size_for_type(channel)          -> 32;
byte_size_for_type(contract_pubkey)  -> 32;
byte_size_for_type(contract_bytearray)-> not_applicable;
byte_size_for_type(transaction)      -> not_applicable;
byte_size_for_type(tx_hash)          -> 32;
byte_size_for_type(oracle_pubkey)    -> 32;
byte_size_for_type(oracle_query)     -> not_applicable;
byte_size_for_type(oracle_query_id)  -> 32;
byte_size_for_type(oracle_response)  -> not_applicable;
byte_size_for_type(account_pubkey)   -> 32;
byte_size_for_type(signature)        -> 64;
byte_size_for_type(name)             -> not_applicable;
byte_size_for_type(commitment)       -> 32;
byte_size_for_type(peer_pubkey)      -> 32;
byte_size_for_type(state)            -> 32;
byte_size_for_type(poi)              -> not_applicable;
byte_size_for_type(state_trees)      -> not_applicable;
byte_size_for_type(call_state_tree)  -> not_applicable.


%% TODO: Fix the base58 module so that it consistently uses binaries instead
%%
binary_to_base58(Bin) ->
    iolist_to_binary(base58:binary_to_base58(Bin)).

base58_to_binary(Bin) when is_binary(Bin) ->
    base58:base58_to_binary(binary_to_list(Bin)).

binary_to_base64(Bin) ->
    base64:encode(Bin).

base64_to_binary(Bin) when is_binary(Bin) ->
    base64:decode(Bin).
