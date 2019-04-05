%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% Chain API for FATE
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_chain_api).

-export([ contract_fate_code/2
        ]).

-include_lib("aebytecode/include/aeb_fate_data.hrl").
-include("../../aecontract/include/aecontract.hrl").

%%%-------------------------------------------------------------------
%%% NOTE: We accept that this module causes havoc in the dependency
%%%       graph for now. The state/chain handling will move down to
%%%       a lower level in the dependency graph later.
%%%-------------------------------------------------------------------

%%%===================================================================
%%% API
%%% ===================================================================

contract_fate_code(Pubkey, #{ contracts := ContractCache } = Chain) ->
    case maps:get(Pubkey, ContractCache, none) of
        none ->
            CTrees = aec_trees:contracts(maps:get(trees, Chain)),
            case aect_state_tree:lookup_contract(Pubkey, CTrees, [no_store]) of
                {value, Contract} ->
                    case aect_contracts:vm_version(Contract) of
                        VMV when ?IS_FATE_SOPHIA(VMV) ->
                            SerCode = aect_contracts:code(Contract),
                            #{ byte_code := ByteCode} = aect_sophia:deserialize(SerCode),
                            try aeb_fate_asm:bytecode_to_fate_code(ByteCode, []) of
                                FateCode ->
                                    Cache1 = ContractCache#{ Pubkey => ByteCode },
                                    Chain1 = Chain#{ contracts => Cache1},
                                    {ok, FateCode, Chain1}
                            catch _:_ -> error
                            end;
                        _ ->
                            error
                    end;
                none ->
                    error
            end;
        ByteCode when is_binary(ByteCode) ->
            try {ok, aeb_fate_asm:bytecode_to_fate_code(ByteCode, []), Chain}
            catch _:_ -> error
            end
    end.
