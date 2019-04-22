-module(aestratum_util).

%% ENV handling
-export([get_env/1,
         set_env/1,
         reset_env/1,
         unset_env/0, unset_env/1,
         env_keys/0]).

%% FN/OK/VAL/ERR/LST handlers
-export([id/1,
         ok/0, ok/1,
         err/0, err/1,
         const/1, fail/1,
         is_ok/1,
         ok_err/1, ok_err/2,
         ok_val_err/1, ok_val_err/2,
         ok_or/2, ok_or_else/2, lazy_ok/3,
         tag_val_err/3,
         hd_or/2, hd_or_else/2, lazy_hd/3,
         key_val/2, val/2, val/3]).

%% MISC
-export([bin/1,
         sum_values/1,
         put_from_default/3,
         config_map/2,
         binary_to_number/1,
         split_list/2,
         idxs/2,
         hex_encode/1,
         resolver/1]).

%% AETERNITY specific
-export([tx_address/1,
         account_pubkey_to_address/1,
         account_address_to_pubkey/1,
         contract_address_to_pubkey/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

id(X) -> X.

is_ok({ok, _}) -> true;
is_ok(_) -> false.

ok() -> fun (X) -> ok(X) end.
const(V) -> fun (_) -> V end.
err() -> error.

ok(X) -> {ok, X}.
err(X) -> {error, X}.
fail(X) -> error(X).

ok_err(ok) -> ok;
ok_err(Other) -> error(Other).

ok_err(ok, _) -> ok;
ok_err(Other, ErrTag) -> error({ErrTag, Other}).

ok_val_err({ok, Val}) -> Val;
ok_val_err(Other) -> error(Other).

ok_val_err({ok, Val}, _ErrTag) -> Val;
ok_val_err(Other, ErrTag) -> error({ErrTag, Other}).

ok_or({ok, _} = OK, _) -> OK;
ok_or(_, Default)      -> Default.

ok_or_else({ok, _} = OK, _) -> OK;
ok_or_else(_, DefaultFn)    -> DefaultFn().

lazy_ok({ok, X}, T, _)  -> T(X);
lazy_ok(Other, _, F) -> F(Other).

tag_val_err({Tag, Val}, Tag, _ErrTag) -> Val;
tag_val_err(Other, _Tag, ErrTag) -> error({ErrTag, Other}).

hd_or([X | _], _)  -> X;
hd_or([], Default) -> Default.

hd_or_else([X | _], _)  -> X;
hd_or_else([], DefaultFn) -> DefaultFn().

lazy_hd([X | _], T, _)  -> T(X);
lazy_hd([], _, F) -> F().

key_val({K, Default}, #{} = M) when is_atom(K) ->
    {K, val(K, M, Default)};
key_val(K, #{} = M) when is_atom(K) ->
    {K, val(K, M)}.

val(K, M) ->
    maps:get(bin(K), M).
val(K, M, Default) ->
    maps:get(bin(K), M, Default).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_env(Key) ->
    {ok, Val} = application:get_env(aestratum, Key),
    Val.

set_env(ConfigMap) ->
    maps:map(fun (K, V) -> application:set_env(aestratum, K, V), V end, ConfigMap).

reset_env(ConfigMap) ->
    aestratum_util:unset_env(),
    aestratum_util:set_env(ConfigMap),
    ConfigMap.

unset_env() ->
    unset_env(env_keys()).
unset_env(Keys) ->
    [application:unset_env(aestratum, K) || K <- Keys].

env_keys() ->
    {Keys, _} = lists:unzip(application:get_all_env(aestratum)),
    Keys.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bin(<<X/binary>>) -> X;
bin(X) when is_atom(X) -> atom_to_binary(X, utf8).

sum_values(M) when is_map(M) ->
    lists:sum(maps:values(M)).

put_from_default(KSD, M, Acc) ->
    {K, Val} = key_val(KSD, M),
    maps:put(K, Val, Acc).

config_map(KeysSomeVals, M) ->
    lists:foldl(resolver(M), #{}, KeysSomeVals).

binary_to_number(B) ->
    case catch binary_to_integer(B) of
        {'EXIT', {badarg, _}} -> binary_to_float(B);
        I -> I
    end.

split_list(Xs, BatchSize) ->
    split_list(Xs, BatchSize, []).

split_list([], _BatchSize, Res) ->
    Res;
split_list(Xs, BatchSize, Res) ->
    try lists:split(BatchSize, Xs) of
        {Batch, Rest} ->
            split_list(Rest, BatchSize, [Batch | Res])
    catch
        error:badarg -> [Xs | Res]
    end.

idxs(Xs, From) ->
    lists:zip(lists:seq(From, From + length(Xs) - 1), Xs).

hex_encode(Data) ->
    list_to_binary(string:to_lower(aeu_hex:bin_to_hex(Data))).

resolver(#{} = M) ->
    fun (KSD, Acc) -> put_from_default(KSD, M, Acc) end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tx_address(PK) ->
    aehttp_api_encoder:encode(transaction, PK).

account_pubkey_to_address(<<PK:32/binary>>) ->
    aehttp_api_encoder:encode(account_pubkey, PK).


account_address_to_pubkey(Addr) ->
    tag_val_err(aehttp_api_encoder:decode(Addr), account_pubkey, invalid_account_address).

contract_address_to_pubkey(Addr) ->
    tag_val_err(aehttp_api_encoder:decode(Addr), contract_pubkey, invalid_contract_address).
