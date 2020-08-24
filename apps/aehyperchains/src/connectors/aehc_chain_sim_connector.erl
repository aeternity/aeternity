-module(aehc_chain_sim_connector).

-behaviour(aehc_connector).

-export([broadcast/3, verify/1, subscribe/1]).

-spec broadcast(_From, _TxPayload, _To) ->
                       binary().
broadcast(From, TxPayload, To) ->
    %% TODO Could we optimize aec_chain_sim:push/1 to return hash instead of ok?
    %% TODO To fill the Tx by actual data;
    Tx = #{},
    Res = aec_chain_sim:push(#{}),
    Res.

-spec verify(TxHash::binary()) -> 
                    boolean().
verify(TxHash) ->
    {value, _STx} = aec_chain_sim:find_signed_tx(TxHash).

-spec subscribe(Module::atom()) -> ok.
subscribe(Module) ->
    %% aec_chain_sim:subscribe(Module),
    ok.
