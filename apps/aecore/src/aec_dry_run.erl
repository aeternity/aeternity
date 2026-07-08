%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_dry_run).

-export([ dry_run/3
	, dry_run/4 ]).

%% exported for eunit only
-export([ run_bounded/2
        , resolve_timeout/1 ]).

-include("blocks.hrl").
-include_lib("aecontract/include/aecontract.hrl").

-define(MR_MAGIC, <<1:32/unit:8>>).
-define(BIG_AMOUNT, 1000000000000000000000). %% 1000 AE

-define(TIMEOUT_ERROR, <<"dry-run exceeded time limit">>).

%% Let Top be one of:
%% * {height, X :: int()} - this means "right after keyblock of generation X"
%% * {in, X :: hash()} - this means "in the MB with hash X" - this gets the
%%                       correct timestamp, important for replaying contract calls
%% * X :: hash() - legacy, means "at the end of MB with hash X"
%% * top - at the top of the chain; if top is MB "at then end of Top",
%%         if top is KB in a fictive MB after Top.

dry_run(Top, Accounts, Txs) ->
    dry_run(Top, Accounts, Txs, []).

%% Entry point for HTTP/Rosetta/aeapi dry-run; off-chain only, never called
%% from block production/validation.
dry_run(Top, Accounts, Txs, Opts) ->
    {TimeoutMs, Opts1} = resolve_timeout(Opts),
    run_bounded(fun() -> dry_run_unbounded(Top, Accounts, Txs, Opts1) end, TimeoutMs).

dry_run_unbounded(Top, Accounts, Txs, Opts) ->
    try setup_dry_run(Top, Accounts) of
        {Env, Trees} -> dry_run_(Txs, Trees, Env, Opts)
    catch
        error:invalid_hash ->
            {error, <<"Invalid hash provided">>};
        error:state_garbage_collected ->
            {error, <<"Block state of given hash was garbage collected">>}
    end.

%% Only the untrusted public endpoint is time-bounded (the DoS surface); Rosetta
%% replay gets a looser bound; internal/trusted calls run unbounded but stay
%% memory-guarded. Explicit {timeout_ms, N} wins; dry-run-only opts are stripped
%% so they never reach tx application. 0 in config means unbounded.
resolve_timeout(Opts) ->
    Opts1 = proplists:delete(timeout_ms, proplists:delete(dry_run_profile, Opts)),
    case proplists:get_value(timeout_ms, Opts) of
        N when is_integer(N) -> {N, Opts1};
        undefined ->
            Profile = proplists:get_value(dry_run_profile, Opts, internal),
            {profile_timeout(Profile), Opts1}
    end.

profile_timeout(public) ->
    cfg_timeout(<<"timeout_ms">>, timeout_ms, aec_governance:micro_block_cycle());
profile_timeout(replay) ->
    cfg_timeout(<<"replay_timeout_ms">>, replay_timeout_ms, 10000);
profile_timeout(_) ->
    infinity.

cfg_timeout(Key, EnvKey, Default) ->
    case aeu_env:user_config_or_env([<<"http">>, <<"dry_run">>, Key],
                                    aehttp, [dry_run, EnvKey], Default) of
        0 -> infinity;
        N -> N
    end.

%% ~2 GB backstop for a single non-yielding allocation the timer can't catch;
%% normal iterative work is bounded by the wall-clock timeout, not this. 0 off.
dry_run_max_heap_words() ->
    aeu_env:user_config_or_env([<<"http">>, <<"dry_run">>, <<"max_heap_words">>],
                                aehttp, [dry_run, max_heap_words], 256000000).

%% Wall-clock bound via a killable worker (infinity = no timer, memory guard
%% only); a non-yielding NIF can still overshoot since exit/2 can't preempt it.
run_bounded(Fun, TimeoutMs) ->
    Caller = self(),
    Tag = make_ref(),
    {WorkerPid, WorkerMRef} = spawn_monitor(fun() -> run_worker(Fun, Caller, Tag) end),
    TimerRef = start_timer(TimeoutMs, Tag),
    Result =
        receive
            {Tag, {worker_result, Res}} ->
                erlang:demonitor(WorkerMRef, [flush]),
                Res;
            {timeout, Tag} ->
                erlang:demonitor(WorkerMRef, [flush]),
                exit(WorkerPid, kill),
                {error, ?TIMEOUT_ERROR};
            {'DOWN', WorkerMRef, process, WorkerPid, Reason} ->
                {error, crash_reason(Reason)}
        end,
    cancel_timer(TimerRef),
    drain_stray(Tag),
    Result.

start_timer(infinity, _Tag) -> undefined;
start_timer(TimeoutMs, Tag) -> erlang:send_after(TimeoutMs, self(), {timeout, Tag}).

cancel_timer(undefined) -> ok;
cancel_timer(TimerRef)  -> _ = erlang:cancel_timer(TimerRef), ok.

%% Drain any stray message that raced with the branch taken above.
drain_stray(Tag) ->
    receive
        {Tag, _}       -> drain_stray(Tag);
        {timeout, Tag} -> drain_stray(Tag)
    after 0 ->
        ok
    end.

%% Inner is linked so a kill of this worker cascades to it; trap_exit keeps an
%% inner crash an error, not a death; Caller is monitored for client disconnect.
run_worker(Fun, Caller, Tag) ->
    process_flag(trap_exit, true),
    CallerMRef = erlang:monitor(process, Caller),
    Self = self(),
    SpawnOpts = case dry_run_max_heap_words() of
                    N when is_integer(N), N > 0 ->
                        [{max_heap_size, #{size => N, kill => true, error_logger => true}}];
                    _ ->
                        []
                end,
    InnerPid = spawn_opt(fun() -> Self ! {inner_done, Fun()} end, SpawnOpts ++ [link]),
    receive
        {inner_done, Res} ->
            erlang:demonitor(CallerMRef, [flush]),
            Caller ! {Tag, {worker_result, Res}};
        {'EXIT', InnerPid, Reason} ->
            erlang:demonitor(CallerMRef, [flush]),
            Caller ! {Tag, {worker_result, {error, crash_reason(Reason)}}};
        {'DOWN', CallerMRef, process, Caller, _Reason} ->
            exit(InnerPid, kill)
    end.

crash_reason(Reason) ->
    iolist_to_binary(io_lib:format("dry-run failed: ~120P", [Reason, 10])).

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
        Other when Other /= offchain_tx ->
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
