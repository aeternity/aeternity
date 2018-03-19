%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Utility functions for AE Contracts
%%% @end
%%%-------------------------------------------------------------------

-module(aect_utils).

-export([hex_bytes/1, hex_byte/1, check_balance/3]).

-include_lib("apps/aecore/include/common.hrl").

-spec hex_byte(byte()) -> string().
hex_byte(N) ->
    hex_bytes(<<N:8>>).

-spec hex_bytes(binary()) -> string().
hex_bytes(Bin) ->
    lists:flatten("0x" ++ [io_lib:format("~2.16.0B", [B]) || <<B:8>> <= Bin]).

-spec check_balance(pubkey(), aec_trees:trees(), non_neg_integer()) -> ok | {error, term()}.
check_balance(ContractKey, Trees, Amount) ->
    ContractsTree = aec_trees:contracts(Trees),
    case aect_state_tree:lookup_contract(ContractKey, ContractsTree) of
        {value, Contract} ->
            case aect_contracts:balance(Contract) >= Amount of
                true  -> ok;
                false -> {error, insufficient_funds}
            end;
        none -> {error, contract_not_found}
    end.
