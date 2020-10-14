%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_dry_run).

-export([ dry_run/3
	, dry_run/4 ]).

-include("blocks.hrl").
-include("../../aecontract/include/aecontract.hrl").

-define(MR_MAGIC, <<1:32/unit:8>>).
-define(BIG_AMOUNT, 1000000000000000000000). %% 1000 AE

dry_run(TopHash, Accounts, Txs) ->
    dry_run(TopHash, Accounts, Txs, []).

dry_run(TopHash, Accounts, Txs, Opts) ->
    try setup_dry_run(TopHash, Accounts) of
        {Env, Trees} -> dry_run_(Txs, Trees, Env, Opts)
    catch
        error:invalid_hash ->
            {error, <<"Invalid hash provided">>};
        error:state_garbage_collected ->
            {error, <<"Block state of given hash was garbage collected">>}
    end.

setup_dry_run(TopHash, Accounts) ->
    {Env, Trees} = aetx_env:tx_env_and_trees_from_hash('aetx_transaction', TopHash),
    Trees1 = add_accounts(Trees, [#{pub_key => ?MR_MAGIC, amount => ?BIG_AMOUNT} | Accounts]),
    Env1   = aetx_env:set_dry_run(Env, true),
    {Env1, Trees1}.

dry_run_(Txs, Trees, Env, Opts) ->
    try
        STxs = prepare_txs(Txs),
        {ok, dry_run_int(STxs, Trees, Env, Opts, [])}
    catch _E:R:ST ->
        {error, iolist_to_binary(io_lib:format("Internal error ~120p", [R]))}
    end.

dry_run_int([], _Trees, Env, _Opts, Acc) ->
    {lists:reverse(Acc), aetx_env:events(Env)};
dry_run_int([{tx, TxOpts, Tx} | Txs], Trees, Env, Opts, Acc) ->
    Stateless = proplists:get_value(stateless, TxOpts, false),
    Env1 = prepare_env(Env, TxOpts),
    %% GH3283: Here we should collect and present the `internal_call_tx` events.
    %% This means expanding the return type, and breaking the api :scream_cat:.
    case aec_trees:apply_txs_on_state_trees([Tx], Trees, Env1, [strict, dont_verify_signature|Opts]) of
        {ok, [Tx], [], Trees1, Events} when Stateless ->
            Env2 = aetx_env:set_events(Env, Events),
            dry_run_int(Txs, Trees, Env2, Opts, [dry_run_res(Tx, Trees1, ok) | Acc]);
        {ok, [Tx], [], Trees1, Events} ->
            Env2 = aetx_env:set_events(Env, Events),
            dry_run_int(Txs, Trees1, Env2, Opts, [dry_run_res(Tx, Trees1, ok) | Acc]);
        Err = {error, _Reason} ->
            dry_run_int(Txs, Trees, Env, Opts, [dry_run_res(Tx, Trees, Err) | Acc])
    end.

dry_run_res(STx, Trees, ok) ->
    Tx = aetx_sign:tx(STx),
    {Type, _} = aetx:specialize_type(Tx),
    case Type of
        _ when Type =:= contract_call_tx;
               Type =:= contract_create_tx;
               Type =:= ga_attach_tx ->
            {CB, CTx} = aetx:specialize_callback(Tx),
            Contract  = CB:contract_pubkey(CTx),
            CallId    = CB:call_id(CTx),
            CallObj   = lookup_call_object(Contract, CallId, Trees),
            {Type, {ok, CallObj}};
        Other when Other /= paying_for_tx, Other /= ga_meta_tx, Other /= offchain_tx ->
            {Type, ok}
    end;
dry_run_res(STx, _Trees, Err) ->
    {Type, _} = aetx:specialize_type(aetx_sign:tx(STx)),
    {Type, Err}.

add_accounts(Trees, Accounts) ->
    AccountsTree = lists:foldl(fun add_account/2, aec_trees:accounts(Trees), Accounts),
    aec_trees:set_accounts(Trees, AccountsTree).

add_account(#{pub_key := PK, amount := A}, AccountsTree) ->
    {ok, Account} =
        case aec_accounts_trees:lookup(PK, AccountsTree) of
            none              -> {ok, aec_accounts:new(PK, A)};
            {value, Account0} -> aec_accounts:earn(Account0, A)
        end,
    aec_accounts_trees:enter(Account, AccountsTree).

prepare_txs([]) -> [];
prepare_txs([{tx, Tx} | Txs]) ->
    [{tx, [], dummy_sign(Tx)} | prepare_txs(Txs)];
prepare_txs([{call_req, Req} | Txs]) ->
    [prepare_call_req(Req) | prepare_txs(Txs)].

dummy_sign(Tx) ->
    aetx_sign:new(Tx, [<<0:(?BLOCK_SIGNATURE_BYTES*8)>>]).

prepare_call_req(ReqMap) ->
    try %% Required
        {ok, CallData} = aeser_api_encoder:safe_decode(contract_bytearray, maps:get(<<"calldata">>, ReqMap)),
        {ok, CtPub}    = aeser_api_encoder:safe_decode(contract_pubkey, maps:get(<<"contract">>, ReqMap)),

        %% Optional
        Amount = maps:get(<<"amount">>, ReqMap, 0),
        Caller = case maps:get(<<"caller">>, ReqMap, undefined) of
                     undefined -> ?MR_MAGIC;
                     EncCaller ->
                         {ok, CallerX} = aeser_api_encoder:safe_decode(account_pubkey, EncCaller),
                         CallerX
                 end,
        Gas    = maps:get(<<"gas">>, ReqMap, 1000000),
        ABI    = maps:get(<<"abi_version">>, ReqMap, ?ABI_AEVM_SOPHIA_1),
        Nonce  = maps:get(<<"nonce">>, ReqMap, 1),

        {ok, CallTx} = aect_call_tx:new(#{caller_id   => aeser_id:create(account, Caller),
                                          nonce       => Nonce,
                                          contract_id => aeser_id:create(contract, CtPub),
                                          abi_version => ABI,
                                          fee         => 1000000 * 1000000,
                                          amount      => Amount,
                                          gas         => Gas,
                                          gas_price   => 1000000,
                                          call_data   => CallData}),

        %% Other options
        ContextMap = maps:get(<<"context">>, ReqMap, #{}),
        TxCtxt = case maps:get(<<"tx">>, ContextMap, none) of
                     none -> [];
                     TxEnc ->
                        {ok, AetxSer} = aeser_api_encoder:safe_decode(transaction, TxEnc),
                        Aetx = aetx:deserialize_from_binary(AetxSer),
                        [{auth_tx, Aetx}]
                 end,

        TxHashCtxt = case maps:get(<<"tx_hash">>, ContextMap, none) of
                         none -> [];
                         TxHashEnc ->
                             {ok, TxHash} = aeser_api_encoder:safe_decode(tx_hash, TxHashEnc),
                             [{auth_tx_hash, TxHash}]
                     end,
        StateCtxt  = [ stateless || maps:get(<<"stateful">>, ContextMap, false) /= true ],
        Context    = TxCtxt ++ TxHashCtxt ++ StateCtxt,
        {tx, Context, dummy_sign(CallTx)}
    catch _:_R ->
        error({bad_dry_run_call_request, ReqMap})
    end.

prepare_env(Env0, Opts) ->
    Env1 = case proplists:get_value(auth_tx_hash, Opts, undefined) of
               undefined -> Env0;
               TxHash    -> aetx_env:set_ga_tx_hash(Env0, TxHash)
           end,
    case proplists:get_value(auth_tx, Opts, undefined) of
        undefined -> Env1;
        Tx        -> aetx_env:set_ga_tx(Env1, Tx)
    end.

lookup_call_object(Key, CallId, Trees) ->
    CallTree = aec_trees:calls(Trees),
    {value, CallObj} = aect_call_state_tree:lookup_call(Key, CallId, CallTree),
    CallObj.
