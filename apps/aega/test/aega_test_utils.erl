%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc Test utility functions for Generalized accounts
%%% @end
%%%-------------------------------------------------------------------
-module(aega_test_utils).

-compile([export_all, nowarn_export_all]).

%%%===================================================================
%%% Transactions
%%%===================================================================

spend_tx(Spec0) ->
    Spec = maps:merge(spend_tx_default(), Spec0),
    {ok, Tx} = aec_spend_tx:new(Spec),
    Tx.

spend_tx_default() ->
    #{amount       => 123456,
      fee          => 20000 * aec_test_utils:min_gas_price(),
      payload      => <<>>}.

ga_attach_tx(PubKey, Spec0) ->
    Spec = maps:merge(ga_attach_tx_default(PubKey), Spec0),
    {ok, Tx} = aega_attach_tx:new(Spec),
    Tx.

ga_attach_tx_default(PubKey) ->
    #{ fee         => 1000000 * aec_test_utils:min_gas_price()
     , owner_id    => aeser_id:create(account, PubKey)
     , vm_version  => aect_test_utils:latest_sophia_vm_version()
     , abi_version => aect_test_utils:latest_sophia_abi_version()
     , gas         => 10000
     , gas_price   => 1 * aec_test_utils:min_gas_price()
     , ttl         => 0
     }.

ga_meta_tx(PubKey, Spec0) ->
    Spec = maps:merge(ga_meta_tx_default(PubKey), Spec0),
    {ok, Tx} = aega_meta_tx:new(Spec),
    Tx.

ga_meta_tx_default(PubKey) ->
    #{ fee         => 1000000 * aec_test_utils:min_gas_price()
     , ga_id       => aeser_id:create(account, PubKey)
     , abi_version => aect_test_utils:latest_sophia_abi_version()
     , gas         => 50000
     , gas_price   => 1000 * aec_test_utils:min_gas_price()
     , ttl         => 0
     }.

%%%===================================================================
%%% Helper functions
%%%===================================================================

make_calldata(Name, Fun, Args) when length(Name) < 20 ->
    {ok, Src} = read_contract(Name),
    make_calldata(Src, Fun, Args);
make_calldata(Code, Fun, Args) ->
    {ok, Calldata, _, _} = aeso_compiler:create_calldata(Code, Fun, Args),
    Calldata.

get_contract(SophiaVersion, Name0) ->
    Name = filename:join("contracts", Name0),
    {ok, Serial} = aect_test_utils:compile_contract(SophiaVersion, Name),
    {ok, BinSrc} = aect_test_utils:read_contract(Name),
    {ok, #{ bytecode => Serial, map => aect_sophia:deserialize(Serial),
            src => binary_to_list(BinSrc), bin_src => BinSrc }}.

read_contract(Name0) ->
    Name = filename:join("contracts", Name0),
    {ok, BinSrc} = aect_test_utils:read_contract(Name),
    {ok, binary_to_list(BinSrc)}.

to_hex_lit(Len, Bin) ->
    [_, _ | Xs] = binary_to_list(aeu_hex:hexstring_encode(Bin)),
    "#" ++
        if length(Xs) < Len * 2 ->
            lists:duplicate(Len * 2 - length(Xs), $0) ++ Xs;
           true ->
            Xs
        end.

hash_lit_to_bin("#" ++ Hex) ->
    if length(Hex) rem 2 == 1 ->
        aeu_hex:hexstring_decode(list_to_binary("0x0" ++ Hex));
       true ->
        aeu_hex:hexstring_decode(list_to_binary("0x" ++ Hex))
    end.


