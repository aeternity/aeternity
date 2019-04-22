-module(aestratum_conv).

-export([bin/1,
         binary_to_number/1,
         list_to_chunks/2,
         list_to_chunks/3,
         tx_address/1,
         account_pubkey_to_address/1,
         account_address_to_pubkey/1,
         contract_address_to_pubkey/1,
         hex_encode/1]).

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
    aehttp_api_encoder:encode(transaction, PK).

account_pubkey_to_address(<<PK:32/binary>>) ->
    aehttp_api_encoder:encode(account_pubkey, PK).


account_address_to_pubkey(Addr) ->
    tag_val_err(aehttp_api_encoder:decode(Addr), account_pubkey, invalid_account_address).

contract_address_to_pubkey(Addr) ->
    tag_val_err(aehttp_api_encoder:decode(Addr), contract_pubkey, invalid_contract_address).

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
