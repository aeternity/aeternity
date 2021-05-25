%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc Test utility functions for Generalized accounts
%%% @end
%%%-------------------------------------------------------------------
-module(aega_test_utils).

-compile([export_all, nowarn_export_all]).

-include("../../aecontract/test/include/aect_sophia_vsn.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

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
     , vm_version  => aega_SUITE:vm_version()
     , abi_version => aega_SUITE:abi_version()
     , gas         => 10000
     , gas_price   => 1 * aec_test_utils:min_gas_price()
     , ttl         => 0
     }.

ga_meta_tx(PubKey, Spec0) ->
    Spec = maps:merge(ga_meta_tx_default(PubKey), Spec0),
    {ok, Tx} = aega_meta_tx:new(Spec),
    Tx.

ga_meta_tx_default(PubKey) ->
    Protocol = aect_test_utils:latest_protocol_version(),
    Def0 =
        #{ fee         => 1000000 * aec_test_utils:min_gas_price()
        , ga_id       => aeser_id:create(account, PubKey)
        , abi_version => aega_SUITE:abi_version()
        , gas         => 20000
        , gas_price   => 1000 * aec_test_utils:min_gas_price()
        },
    case Protocol < ?IRIS_PROTOCOL_VSN of
        true -> Def0#{ttl => 0};
        false -> Def0
    end.

%%%===================================================================
%%% Offchain state functions
%%%===================================================================

new_state(Acc1, Amnt1, Sign1, Acc2, Amnt2, Sign2) ->
    Accounts = [aec_accounts:new(Acc1, Amnt1), aec_accounts:new(Acc2, Amnt2)],
    Trees    = aec_test_utils:create_state_tree_with_accounts(Accounts, no_backend),
    #{ state => Trees, sign => [{Acc1, Sign1}, {Acc2, Sign2}], round => 1 }.

transfer(From, To, Amnt, S = #{ state := Trees, round := Rnd }) ->
    ATrees         = aec_trees:accounts(Trees),
    {value, AFrom} = aec_accounts_trees:lookup(From, ATrees),
    {value, ATo}   = aec_accounts_trees:lookup(To, ATrees),
    {ok, AFrom1}   = aec_accounts:spend(AFrom, Amnt, 1),
    {ok, ATo1}     = aec_accounts:earn(ATo, Amnt),
    ATrees1        = aec_accounts_trees:enter(AFrom1, aec_accounts_trees:enter(ATo1, ATrees)),
    S#{ state := aec_trees:set_accounts(Trees, ATrees1), round := Rnd + 1 }.

add_contract(Owner, _DryRunRes = {ok, Ct, OnTrees}, OffS = #{ state := OffTrees }) ->
    OnCTrees = aec_trees:contracts(OnTrees),

    {value, Contract} = aect_state_tree:lookup_contract(Ct, OnCTrees),
    Contract1         = aect_contracts:set_owner(Owner, Contract),
    ContractStore     = aect_contracts:state(Contract1),
    StoreContent      = aect_contracts_store:contents(ContractStore),
    ContractStore1    = aect_contracts_store:put_map(StoreContent, ContractStore),
    Contract2         = aect_contracts:set_state(ContractStore1, Contract1),

    OffCTrees = aec_trees:contracts(OffTrees),
    OffCTrees1 = aect_state_tree:insert_contract(Contract2, OffCTrees),
    OffTrees1 = aec_trees:set_contracts(OffTrees, OffCTrees1),

    OffATrees = aec_trees:accounts(OffTrees1),
    OffATrees1 = aec_accounts_trees:enter(aec_accounts:new(Ct, 0), OffATrees),
    OffTrees2 = aec_trees:set_accounts(OffTrees1, OffATrees1),

    {Ct, OffS#{ state := OffTrees2, round := maps:get(round, OffS) + 1}}.

add_call(Ct, Round, {ok, Call0, OnTrees}, OffS = #{ state := OffTrees }) ->
    Call1 = aect_call:set_caller_nonce(Round, Call0),
    Call2 = aect_call:set_height(Round, Call1),
    Call  = aect_call:set_id(aect_call:id(aect_call:caller_pubkey(Call2), Round, aect_call:contract_pubkey(Call2)), Call2),
    OnCTrees = aec_trees:calls(OnTrees),
    {value, Call0} = aect_call_state_tree:lookup_call(Ct, aect_call:id(Call0), OnCTrees),

    OffCTrees = aec_trees:calls(OffTrees),
    OffCTrees1 = aect_call_state_tree:insert_call(Call, OffCTrees),
    OffTrees1 = aec_trees:set_calls(OffTrees, OffCTrees1),
    OffS#{ state := OffTrees1 }.

balance(PK, #{state := Trees}) ->
   A = aec_accounts_trees:get(PK, aec_trees:accounts(Trees)),
   aec_accounts:balance(A).

state_hash(#{state := Trees}) ->
    aec_trees:hash(Trees).

offchain_trees(#{state := Trees}) ->
    Trees.

poi(What, #{state := Trees}) ->
    PoI = aec_trees:new_poi(Trees),
    add_poi(What, Trees, PoI).

add_poi([], _, PoI) -> PoI;
add_poi([{account, PK} | What], Trees, PoI) ->
    {ok, PoI1} = aec_trees:add_poi(accounts, PK, Trees, PoI),
    add_poi(What, Trees, PoI1).


payload(CId, S = #{round := Rnd, sign := Sigs}, S0) ->
    %% use only aesc_offchain_tx without updates since GAs are introduced in
    %% Fortuna
    {ok, OffTx} = aesc_offchain_tx:new(
                      #{ channel_id => aeser_id:create(channel, CId)
                       , state_hash => state_hash(S)
                       , round      => Rnd + 1}),
    SOffTx = sign_tx(aetx_sign:new(OffTx, []), Sigs, S0),
    aetx_sign:serialize_to_binary(SOffTx).

sign_tx(STx, [], _) ->
    STx;
sign_tx(STx, [Sig | Sigs], S) ->
    case aetx:specialize_type(aetx_sign:tx(STx)) of
        {ga_meta_tx, MetaTx} ->
            STx1 = sign_tx(aega_meta_tx:tx(MetaTx), [Sig], S),
            NewMetaTx = ga_meta_tx(aega_meta_tx:ga_pubkey(MetaTx),
                                   #{auth_data => aega_meta_tx:auth_data(MetaTx),
                                     tx => STx1}),
            sign_tx(aetx_sign:new(NewMetaTx, []), Sigs, S);
        {channel_offchain_tx, _COTx} ->
            Bin = aetx:serialize_to_binary(aetx_sign:tx(STx)),
            BinForNetwork = aec_governance:add_network_id(Bin),
            case Sig of
                {PK, plain} ->
                    PrivKey = aect_test_utils:priv_key(PK, S),
                    Signature = enacl:sign_detached(BinForNetwork, PrivKey),
                    STx1 = aetx_sign:add_signatures(STx, [Signature]),
                    sign_tx(STx1, Sigs, S);
                {PK, {basic, Nonce}} ->
                    PrivKey = aect_test_utils:priv_key(PK, S),
                    TxHash = aec_hash:hash(tx, BinForNetwork),
                    SignBin = basic_auth_sign(list_to_integer(Nonce), TxHash, PrivKey),
                    AuthData = make_calldata("basic_auth", "authorize", [Nonce, to_hex_lit(64, SignBin)]),
                    MetaTx = ga_meta_tx(PK, #{auth_data => AuthData, tx => STx}),
                    sign_tx(aetx_sign:new(MetaTx, []), Sigs, S)
            end
    end.

basic_auth_sign(Nonce, TxHash, PrivKey) ->
    Val = case aega_SUITE:abi_version() of
              ?ABI_AEVM_SOPHIA_1 -> <<32:256, TxHash/binary, Nonce:256>>;
              ?ABI_FATE_SOPHIA_1 -> aeb_fate_encoding:serialize({tuple, {{bytes, TxHash}, Nonce}})
          end,
    enacl:sign_detached(aec_hash:hash(tx, Val), PrivKey).

%%%===================================================================
%%% Helper functions
%%%===================================================================

make_calldata(Name, Fun, Args) when length(Name) < 20 ->
    {ok, Src} = read_contract(Name),
    make_calldata(Src, Fun, Args);
make_calldata(Code, Fun, Args) ->
    %% Use the memoized version to not waste 500 ms each time we make a meta tx with the same nonce -.-
    {ok, CallData} = aect_test_utils:encode_call_data(aega_SUITE:sophia_version(), Code, Fun, Args),
    CallData.

get_contract(Name) ->
    SophiaVersion = aega_SUITE:sophia_version(),
    get_contract(SophiaVersion, Name).

get_contract(SophiaVersion, Name) ->
    {ok, Serial} = aect_test_utils:compile_contract(SophiaVersion, Name),
    {ok, BinSrc} = aect_test_utils:read_contract(SophiaVersion, Name),
    {ok, #{ bytecode => Serial, map => aect_sophia:deserialize(Serial),
            src => binary_to_list(BinSrc), bin_src => BinSrc }}.

read_contract(Name) ->
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

auth_fun_hash(Name, AEVMTypeInfo) ->
    case ?IS_AEVM_SOPHIA(aega_SUITE:vm_version()) of
        true  -> aeb_aevm_abi:type_hash_from_function_name(Name, AEVMTypeInfo);
        false -> {ok, <<(aeb_fate_code:symbol_identifier(Name)):4/binary, 0:(28*8)>>}
    end.
