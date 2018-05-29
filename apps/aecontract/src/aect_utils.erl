%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Utility functions for AE Contracts
%%% @end
%%%-------------------------------------------------------------------

-module(aect_utils).

-export([hex_bytes/1, hex_byte/1, check_balance/3, check/2]).

-spec hex_byte(byte()) -> string().
hex_byte(N) ->
    hex_bytes(<<N:8>>).

-spec hex_bytes(binary()) -> string().
hex_bytes(Bin) ->
    lists:flatten("0x" ++ [io_lib:format("~2.16.0B", [B]) || <<B:8>> <= Bin]).

-spec check_balance(aec_keys:pubkey(), aec_trees:trees(), non_neg_integer()) ->
        ok | {error, term()}.
check_balance(ContractKey, Trees, Amount) ->
    AccountsTree = aec_trees:accounts(Trees),
    case aec_accounts_trees:lookup(ContractKey, AccountsTree) of
        {value, Account} ->
            check(aec_accounts:balance(Account) >= Amount, insufficient_funds);
        none -> {error, contract_not_found}
    end.

check(true, _) -> ok;
check(false, Err) -> {error, Err}.

