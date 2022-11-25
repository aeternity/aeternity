%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_dry_run).

-export([ dry_run/3
	, dry_run/4 ]).

-include("blocks.hrl").
-include_lib("aecontract/include/aecontract.hrl").

-define(MR_MAGIC, <<1:32/unit:8>>).
-define(BIG_AMOUNT, 1000000000000000000000). %% 1000 AE

%% Let Top be one of:
%% * {height, X :: int()} - this means "right after keyblock of generation X"
%% * {in, X :: hash()} - this means "in the MB with hash X" - this gets the
%%                       correct timestamp, important for replaying contract calls
%% * X :: hash() - legacy, means "at the end of MB with hash X"
%% * top - at the top of the chain; if top is MB "at then end of Top",
%%         if top is KB in a fictive MB after Top.

dry_run(Top, Accounts, Txs) ->
    dry_run(Top, Accounts, Txs, []).

dry_run(Top, Accounts, Txs, Opts) ->
    try setup_dry_run(Top, Accounts) of
        {Env, Trees} -> dry_run_(Txs, Trees, Env, Opts)
    catch
        error:invalid_hash ->
            {error, <<"Invalid hash provided">>};
        error:state_garbage_collected ->
            {error, <<"Block state of given hash was garbage collected">>}
    end.

setup_dry_run(Top, Accounts) ->
    {Env, Trees} = tx_env_and_trees(Top),
    Trees1 = add_accounts(Trees, [#{pub_key => ?MR_MAGIC, amount => ?BIG_AMOUNT} | Accounts]),
    Env1   = aetx_env:set_dry_run(Env, true),
    {Env1, Trees1}.

tx_env_and_trees(top) ->
    case aec_chain:top_block_hash() of
        undefined -> {error, <<"No top block hash">>};
        TopHash   -> tx_env_and_trees(TopHash)
    end;
tx_env_and_trees({height, X}) ->
    case aec_chain:get_key_header_by_height(X) of
        {ok, KeyHeader} ->
            {ok, KeyHash} = aec_headers:hash_header(KeyHeader),
            tx_env_and_trees(KeyHeader, KeyHash, aec_headers:time_in_msecs(KeyHeader));
        {error, 'chain_too_short'} ->
            {error, <<"Chain too short">>}
    end;
tx_env_and_trees({in, Hash}) ->
    case aec_chain:get_header(Hash) of
        {ok, Header} ->
            case aec_headers:type(Header) of
                key -> {error, <<"dry_run 'in' only applicable to Micro Block (hash)">>};
                micro ->
                    KeyHash = aec_headers:prev_key_hash(Header),
                    {ok, KeyHeader} = aec_chain:get_header(KeyHash),
                    tx_env_and_trees(KeyHeader,
                                     aec_headers:prev_hash(Header),
                                     aec_headers:time_in_msecs(Header))
            end;
        error ->
            {error, <<"Block not found">>}
    end;
tx_env_and_trees(TopHash) ->
    aetx_env:tx_env_and_trees_from_hash(aetx_transaction, TopHash).

tx_env_and_trees(KeyHeader, PrevHash, Time) ->
    try aec_chain:get_block_state(PrevHash) of
        {ok, Trees} ->
            {ok, KeyHash} = aec_headers:hash_header(KeyHeader),
            Env = aetx_env:tx_env_from_key_header(KeyHeader, KeyHash, Time, PrevHash),
            {aetx_env:set_context(Env, aetx_transaction), Trees}
    catch
        error:{hash_not_present_in_db, _} ->
          {error, <<"state garbage collected">>}
    end.


dry_run_(Txs, Trees, Env, Opts) ->
    try
        STxs = prepare_txs(Txs),
        {ok, dry_run_int(STxs, Trees, Env, Opts, [])}
    catch _E:R:_ST ->
        {error, iolist_to_binary(io_lib:format("Internal error ~120p", [R]))}
    end.

dry_run_int([], _Trees, Env, _Opts, Acc) ->
    {lists:reverse(Acc), aetx_env:events(Env)};
dry_run_int([{tx, TxOpts, Tx} | Txs], Trees, Env, Opts, Acc) ->
    Stateless = proplists:get_value(stateless, TxOpts, false),
    Env1 = prepare_env(Env, TxOpts),
    EventsEnabled = proplists:get_bool(tx_events, Opts),
    case aec_trees:apply_txs_on_state_trees([Tx], Trees, Env1, [strict, dont_verify_signature|Opts]) of
        {ok, [Tx], [], Trees1, Events} when Stateless ->
            Env2 = aetx_env:set_events(Env1, Events),
            dry_run_int(Txs, Trees, Env2, Opts, [dry_run_res(Tx, Trees1, Events, EventsEnabled, ok) | Acc]);
        {ok, [Tx], [], Trees1, Events} ->
            Env2 = aetx_env:set_events(Env1, Events),
            dry_run_int(Txs, Trees1, Env2, Opts, [dry_run_res(Tx, Trees1, Events, EventsEnabled, ok) | Acc]);
        Err = {error, _Reason} ->
            dry_run_int(Txs, Trees, Env1, Opts, [dry_run_res(Tx, Trees, [], EventsEnabled, Err) | Acc])
    end.

dry_run_res(STx, Trees, Events, EventsEnabled, ok) ->
    Tx = aetx_sign:tx(STx),
    {Type, _} = aetx:specialize_type(Tx),
    case Type of
        contract_call_tx ->
            {CB, CTx} = aetx:specialize_callback(Tx),
            CtCallId  = CB:ct_call_id(CTx),
            CallId    = CB:call_id(CTx),
            CallObj   = lookup_call_object(CtCallId, CallId, Trees),
            if EventsEnabled ->
                {Type, {ok, Events, CallObj}};
               true ->
                {Type, {ok, CallObj}}
            end;
        _ when Type =:= contract_create_tx;
               Type =:= ga_attach_tx ->
            {CB, CTx} = aetx:specialize_callback(Tx),
            Contract  = CB:contract_pubkey(CTx),
            CallId    = CB:call_id(CTx),
            CallObj   = lookup_call_object(Contract, CallId, Trees),
            %% PR#3848 (Rosetta API): Changing the external API of this function to return
            %% events per transaction would have broken the middleware. Fortunately
            %% the MDW doesn't enable tx_events so keeping the old API for the
            %% no tx_events case is safe.
            if EventsEnabled ->
                {Type, {ok, Events, CallObj}};
               true ->
                {Type, {ok, CallObj}}
            end;
        Other when Other /= paying_for_tx, Other /= offchain_tx ->
            if EventsEnabled ->
                {Type, {ok, Events}};
               true ->
                {Type, ok}
            end
    end;
dry_run_res(STx, _Trees, _Events, _EventsEnabled, Err) ->
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
    Env2 = case proplists:get_value(auth_tx, Opts, undefined) of
               undefined -> Env1;
               Tx        -> aetx_env:set_ga_tx(Env1, Tx)
           end,
    aetx_env:set_events(Env2, []).

lookup_call_object(Key, CallId, Trees) ->
    CallTree = aec_trees:calls(Trees),
    {value, CallObj} = aect_call_state_tree:lookup_call(Key, CallId, CallTree),
    CallObj.
