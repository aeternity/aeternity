-module(aestratum_conv).

-export([bin/1,
         binary_to_number/1,
         list_to_chunks/2,
         list_to_chunks/3,
         map_to_chunks/2,
         tx_address/1,
         account_pubkey_to_address/1,
         account_address_to_pubkey/1,
         account_address_to_integer/1,
         contract_address_to_pubkey/1,
         address_to_hex/1,
         hex_encode/1,
         delta_secs_to_universal_datetime/1,
         delta_secs_to_universal_datetime/2]).

-import(aestratum_fn, [tag_val_err/3]).

%%%%%%%%%%

bin(<<X/binary>>) -> X;
bin(X) when is_atom(X) -> atom_to_binary(X, utf8).

binary_to_number(B) ->
    case catch binary_to_integer(B) of
        {'EXIT', {badarg, _}} -> binary_to_float(B);
        I -> I
    end.

tx_address(PK) ->
    aeser_api_encoder:encode(transaction, PK).

account_pubkey_to_address(<<PK:32/binary>>) ->
    aeser_api_encoder:encode(account_pubkey, PK).

account_address_to_integer(<<"ak_", _/binary>> = Addr) ->
    <<IntAddr:256>> = account_address_to_pubkey(Addr),
    IntAddr.

account_address_to_pubkey(Addr) ->
    tag_val_err(aeser_api_encoder:decode(Addr), account_pubkey, invalid_account_address).

contract_address_to_pubkey(Addr) ->
    tag_val_err(aeser_api_encoder:decode(Addr), contract_pubkey, invalid_contract_address).

address_to_hex(Addr) ->
    aeu_hex:bin_to_hex(account_address_to_pubkey(Addr)).

hex_encode(Data) ->
    list_to_binary(string:to_lower(aeu_hex:bin_to_hex(Data))).

list_to_chunks(Xs, ChunkSize) ->
    list_to_chunks(Xs, ChunkSize, []).

list_to_chunks([], _ChunkSize, Res) ->
    Res;
list_to_chunks(Xs, ChunkSize, Res) ->
    try lists:split(ChunkSize, Xs) of
        {Chunk, Rest} ->
            list_to_chunks(Rest, ChunkSize, [Chunk | Res])
    catch
        error:badarg -> [Xs | Res]
    end.

delta_secs_to_universal_datetime(Delta) ->
    UTCSecs = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    delta_secs_to_universal_datetime(Delta, UTCSecs).
delta_secs_to_universal_datetime(Delta, ActualDT) ->
    calendar:gregorian_seconds_to_datetime(ActualDT + Delta).

map_to_chunks(Map, ChunkSize) ->
    [maps:from_list(Chunk) || Chunk <- list_to_chunks(maps:to_list(Map), ChunkSize)].
