-module(sc_ws_handler).

%% WS API
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include_lib("aecontract/include/hard_forks.hrl").
-export([time_since_last_dispatch/1]).

-record(handler, {fsm_pid            :: pid() | undefined,
                  fsm_mref           :: reference() | undefined,
                  channel_id         :: aesc_channels:id() | undefined,
                  enc_channel_id     :: aeser_api_encoder:encoded() | undefined,
                  job_id             :: term(),
                  protocol           :: sc_ws_api:protocol(),
                  role               :: initiator | responder | undefined,
                  host               :: binary() | undefined,
                  port               :: non_neg_integer() | undefined}).

-opaque handler() :: #handler{}.
-export_type([handler/0]).

-define(ERROR_TO_CLIENT(Err), {?MODULE, send_to_client, {error, Err}}).
-include_lib("aeutils/include/aeu_stacktrace.hrl").

%% ===========================================================================
%% @doc API to check if session is potentially hanging, waiting for socket close
%%
time_since_last_dispatch(HandlerPid) ->
    %% We use the process dictionary for this. The cowboy_websocket module
    %% maintains an inactivity timer, but this resides in the inner state,
    %% which is not accessible to the handler. The info we're after is how
    %% long since the handler was dispatched. This can be used to determine
    %% whether we want to kill and replace an older, hung, websocket session.
    case process_info(HandlerPid, dictionary) of
        {_, Dict} ->
            case lists:keyfind(timestamp_key(), 1, Dict) of
                {_, TS} ->
                    Now = erlang:monotonic_time(),
                    erlang:convert_time_unit(Now - TS, native, millisecond);
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

%% Called from websocket_handle()
%%
put_timestamp() ->
    put(timestamp_key(), erlang:monotonic_time()).

timestamp_key() ->
    {?MODULE, last_dispatch}.

%% ===========================================================================

init(Req, _Opts) ->
    Parsed = cowboy_req:parse_qs(Req),
    lager:debug("init(~p, ~p), Parsed ~p",
        [aesc_utils:censor_ws_req(Req), _Opts, aesc_utils:censor_init_opts(Parsed)]),
    process_flag(trap_exit, true),
    {cowboy_websocket, Req,
     maps:from_list(Parsed)}.

-spec websocket_init(map()) -> {ok, handler()} | {stop, undefined}.
websocket_init(#{ <<"reconnect_tx">> := _ } = Params) ->
    case jobs_ask() of
        {ok, JobId} ->
            websocket_init_reconnect(Params#{ job_id => JobId });
        {error, _} ->
            {error, too_many_ws_sockets}
    end;
websocket_init(Params) ->
    case {prepare_handler(Params), read_channel_options(Params)} of
        {{error, Err}, _} ->
            %% Make dialyzer happy by providing a protocol - the protocol is not
            %% relevant here as we will kill this process after sending a error code to the client
            handler_parsing_error(Err, #handler{protocol = sc_ws_api:protocol(<<"json-rpc">>)}, Params);
        {Handler, {error, Err}} ->
            handler_parsing_error(Err, Handler, Params);
        {Handler, ChannelOpts} ->
            case maps:is_key(existing_channel_id, ChannelOpts) of
                true ->
                    lager:debug("existing_channel_id key exists", []),
                    case derive_reconnect_opts(ChannelOpts) of
                        {ok, ReconnectOpts} ->
                            lager:debug("Will try to reconnect; ReconnectOpts = ~p",
                                        [ReconnectOpts]),
                            websocket_init_reconnect_(ReconnectOpts, Handler);
                        {error, _} = Err1 ->
                            lager:debug("Error deriving reconnect opts: ~p", [Err1]),
                            %% TODO: This is probably an error case, with insufficient info also
                            %% for reestablish. Can it even happen?
                            handler_init_error(not_found, Handler)
                    end;
                false ->
                    websocket_init_parsed(Handler, ChannelOpts)
            end
    end.

websocket_init_parsed(Handler, ChannelOpts) ->
    lager:debug("ChannelOpts = ~p", [aesc_utils:censor_init_opts(ChannelOpts)]),
    case start_link_fsm(Handler, ChannelOpts) of
        {ok, FsmPid} ->
            MRef = erlang:monitor(process, FsmPid),
            {ok, Handler#handler{fsm_pid = FsmPid, fsm_mref = MRef}};
        {error, Err} ->
            handler_init_error(Err, Handler)
    end.

websocket_init_reconnect(#{ job_id := JobId
                          , <<"protocol">> := Protocol
                          , <<"reconnect_tx">> := ReconnectTx } = Params0) ->
    Read = sc_ws_utils:read_f(Params0),
    Params = sc_ws_utils:check_params(read_state_password(Read)),
    lager:debug("ReconnectTx = ~p", [ReconnectTx]),
    case check_reconnect_tx(ReconnectTx) of
        {ok, #{} = Opts} ->
            lager:debug("Reconnect tx Opts = ~p", [aesc_utils:censor_init_opts(Opts)]),
            ReconnectOpts =
                maps:merge(Params#{ job_id => JobId
                                  , protocol => sc_ws_api:protocol(Protocol) }, Opts),
            lager:debug("ReconnectOpts = ~p",
                        [aesc_utils:censor_init_opts(ReconnectOpts)]),
            Handler = reconnect_opts_to_handler(ReconnectOpts),
            lager:debug("Handler = ~p", [Handler]),
            websocket_init_reconnect_(ReconnectOpts, Handler);
        {error, CheckError} ->
            lager:debug("CheckError = ~p", [CheckError]),
            {stop, undefined}
    end.

websocket_init_reconnect_(ReconnectOpts, Handler) ->
    case reconnect_to_fsm_(ReconnectOpts, Handler, fun(E) -> {error, E} end) of
        {ok, _} = Ok ->
            Ok;
        {error, {existing_client, OldClient}} ->
            check_existing_client(OldClient, ReconnectOpts, Handler);
        {error, Err} ->
            handler_init_error(Err, Handler)
    end.

reconnect_to_fsm_(#{ channel_id := ChanId
                   , role       := Role
                   , pub_key    := Pubkey
                   , signed_tx  := SignedTx } = Opts, Handler, OnError) ->
    case aesc_fsm:where(ChanId, Role) of
        undefined ->
            lager:debug("where(~p, ~p) -> undefined", [ChanId, Role]),
            maybe_reestablish(Opts, Handler, OnError);
        #{ fsm_pid := Fsm, pub_key := Pubkey } ->
            %% At this point, we haven't verified the signature.
            %% This is done by the fsm.
            lager:debug("Found FSM = ~p", [Fsm]),
            case aesc_fsm:reconnect_client(Fsm, self(), SignedTx) of
                ok ->
                    MRef = erlang:monitor(process, Fsm),
                    { ok, Handler#handler{ fsm_pid  = Fsm
                                         , fsm_mref = MRef } };
                {error, E} ->
                    OnError(E)
            end
    end.

%% The fsm is not running. Perhaps there is cached state, warranting a reestablish?
%%
maybe_reestablish(#{ channel_id := ChId
                   , pub_key    := Pubkey } = ReconnectOpts, Handler0, OnError) ->
    lager:debug("ReconnectOpts = ~p",
                [aesc_utils:censor_init_opts(ReconnectOpts)]),
    case maps:find(state_password, ReconnectOpts) of
        {ok, Password} ->
            case aesc_state_cache:reestablish(ChId, Pubkey, Password) of
                {ok, State, Opts} ->
                    lager:debug("Fetched state. Opts = ~p", [Opts]),
                    Handler = update_handler_for_reestablish(Handler0, Opts),
                    try aesc_offchain_state:get_latest_signed_tx(State) of
                        {_, SignedTx} ->
                            lager:debug("Latest tx = ~p", [SignedTx]),
                            ExpandedOpts = expand_cached_opts(Opts),
                            websocket_init_parsed(
                              Handler,
                              ExpandedOpts#{ existing_channel_id => ChId
                                           , offchain_tx => SignedTx })
                    catch
                        error:_E ->
                            lager:debug("Failed getting latest signed tx: ~p", [_E]),
                            OnError(not_found)
                    end;
                {error, CacheError} ->
                    lager:debug("No cached state", []),
                    OnError(CacheError)
            end;
        error ->
            lager:debug("No state_password specified", []),
            OnError(not_found)
    end.

check_existing_client(Client, Opts, Handler) ->
    OnError = fun(E) ->
                      handler_init_error(E, Handler)
              end,
    T = time_since_last_dispatch(Client),
    lager:debug("Time since last dispatch (~p): ~p", [Client, T]),
    if is_integer(T) ->
            %% It is actually a WS client
            MRef = erlang:monitor(process, Client),
            exit(Client, kill),
            receive {'DOWN', MRef, _, _, _} ->
                    reconnect_to_fsm_(Opts, Handler, OnError)
            end;
       true ->
            reconnect_to_fsm_(Opts, Handler, OnError)
    end.

handler_parsing_error(Err, Handler, Params) ->
    %%TODO: Inform the client of wrong init params
    HandledErrors = [{invalid_password            , invalid_password},
                     {password_required_since_lima, {state_password, missing}}],
    case proplists:get_value(Err, HandledErrors, not_handled_error) of
        not_handled_error ->
            lager:info("Channel WS failed to start because of ~p; params ~p",
                [Err, aesc_utils:censor_init_opts(Params)]),
            {stop, undefined};
        ErrKey ->
            %% because of tests' subscription mechanism, we
            %% might have to give some time tolerance in order
            %% to avoid race conditions
            After = 0,
            timer:send_after(After, ?ERROR_TO_CLIENT(ErrKey)),
            timer:send_after(After + 1, stop),
            {ok, Handler#handler{fsm_pid  = undefined,
                                 fsm_mref = undefined}}
    end.

handler_init_error(Err, Handler) ->
    HandledErrors =[ {initiator_not_found           , participant_not_found}
                   , {responder_not_found           , participant_not_found}
                   , {client_still_active           , client_still_active}
                   , {insufficient_initiator_amount , value_too_low}
                   , {insufficient_responder_amount , value_too_low}
                   , {insufficient_amounts          , value_too_low}
                   , {channel_reserve_too_low       , value_too_low}
                   , {push_amount_too_low           , value_too_low}
                   , {lock_period_too_low           , value_too_low}
                   , {invalid_password              , invalid_password}
                   , {bad_signature                 , bad_signature}
                   , {password_required_since_lima  , {state_password, missing}}
                   ],
    case proplists:get_value(Err, HandledErrors, not_handled_error) of
        not_handled_error ->
            lager:info("Failed to start because of unhandled error ~p", [Err]),
            {stop, undefined};
        ErrKey ->
            %% because of tests' subscription mechanism, we
            %% might have to give some time tolerance in order
            %% to avoid race conditions
            After = 0,
            timer:send_after(After, ?ERROR_TO_CLIENT(ErrKey)),
            timer:send_after(After + 1, stop),
            {ok, Handler#handler{fsm_pid  = undefined,
                                 fsm_mref = undefined}}
    end.


-spec websocket_handle(term(), handler()) -> {ok, handler()}.
websocket_handle(Data, Handler) ->
    put_timestamp(),
    websocket_handle_(Data, Handler).

websocket_handle_({text, MsgBin}, #handler{fsm_pid = undefined} = H) ->
    %% the FSM has not been started, the connection is to die any moment now
    %% do not respond
    lager:debug("Not processing message ~p", [MsgBin]),
    {ok, H};
websocket_handle_({text, MsgBin}, #handler{protocol = Protocol,
                                          enc_channel_id = ChannelId,
                                          fsm_pid  = FsmPid} = H) ->
    case sc_ws_api:process_from_client(Protocol, MsgBin, FsmPid, ChannelId) of
        no_reply          -> {ok, H};
        {reply, Resp}     -> {reply, {text, jsx:encode(Resp)}, H};
        stop              -> {stop, H}
    end;
websocket_handle_(_Data, H) ->
    {ok, H}.

websocket_info(?ERROR_TO_CLIENT(Err), #handler{protocol = Protocol} = H) ->
    {reply, Resp} = sc_ws_api:error_response(Protocol, Err),
    timer:sleep(1000),
    lager:info("Handler critical error: ~p", [Err]),
    {reply, {text, jsx:encode(Resp)}, H};
websocket_info(stop, #handler{} = H) ->
    {stop, H};
websocket_info({aesc_fsm, FsmPid, Msg}, #handler{ fsm_pid = FsmPid
                                                , protocol = Protocol } = H) ->
    #handler{enc_channel_id = ChannelId} = H1 = set_channel_id(Msg, H),
    case sc_ws_api:process_from_fsm(Protocol, Msg, ChannelId) of
        no_reply          -> {ok, H1};
        {reply, Resp}     -> {reply, {text, jsx:encode(Resp)}, H1};
        stop              -> {stop, H1}
    end;
websocket_info({'DOWN', MRef, _, _, _}, #handler{fsm_mref = MRef} = H) ->
    {stop, H#handler{fsm_pid = undefined,
                     fsm_mref = undefined}};
websocket_info(_Info, State) ->
    {ok, State}.

set_channel_id(Msg, H) ->
    Res = set_channel_id_(Msg, H),
    lager:debug("Msg=~p (Id0=~p) -> ~p", [Msg, H#handler.channel_id,
                                          Res#handler.channel_id]),
    Res.

set_channel_id_(#{channel_id := Id},
               #handler{channel_id = undefined} = H) when Id =/= undefined ->
    H#handler{channel_id = Id,
              enc_channel_id = aeser_api_encoder:encode(channel, Id)};
set_channel_id_(#{channel_id := A}, #handler{channel_id = B})
  when A =/= undefined, A =/= B ->
    erlang:error({channel_id_mismatch, [A, B]});
set_channel_id_(_Msg, H) ->
    H.

terminate(Reason, _PartialReq, #{} = _H) ->
    lager:debug("WebSocket dying because of ~p", [Reason]),
    % not initialized yet
    ok;
terminate(Reason, _PartialReq, State) ->
    lager:debug("WebSocket dying because of ~p", [Reason]),
    case fsm_pid(State) of
        undefined -> pass;
        FsmPid ->
            true = unlink(FsmPid)
    end,
    case job_id(State) of
        undefined -> pass;
        JobId -> jobs:done(JobId)
    end,
    ok.

-spec job_id(handler() | undefined) -> term().
job_id(undefined) -> undefined;
job_id(#handler{job_id = JobId}) ->
    JobId.

-spec fsm_pid(handler() | undefined) -> pid() | undefined.
fsm_pid(undefined) -> undefined;
fsm_pid(#handler{fsm_pid = Pid}) ->
    Pid.

-spec start_link_fsm(handler(), map()) -> {ok, pid()} | {error, atom()}.
start_link_fsm(#handler{role = initiator, host=Host, port=Port}, Opts) ->
    aesc_client:initiate(Host, Port, Opts);
start_link_fsm(#handler{role = responder, port=Port}, Opts) ->
    aesc_client:respond(Port, Opts).

derive_reconnect_opts(#{ <<"reconnect_tx">> := ReconnectTx } = Params) ->
    lager:debug("Params = ~p", [Params]),
    case check_reconnect_tx(ReconnectTx) of
        {ok, #{} = Opts} ->
            lager:debug("Reconnect tx Opts = ~p", [Opts]),
            {ok, maps:merge(Params, Opts)};
        {error, _} = Error ->
            Error
    end;
derive_reconnect_opts(#{ existing_channel_id := ChId
                       , role                := Role
                       , initiator           := I
                       , responder           := R } = Opts) ->
    PubKey = case Role of
                 initiator -> I;
                 responder -> R
             end,
    Round = 1,
    {ok, Tx} = aesc_client_reconnect_tx:new(#{ channel_id => aeser_id:create(channel, ChId)
                                             , role       => Role
                                             , pub_key    => aeser_id:create(account, PubKey)
                                             , round      => Round }),
    SignedTx = aetx_sign:new(Tx, []),
    {ok, maps:merge(Opts, #{ channel_id => ChId
                           , role       => Role
                           , pub_key    => PubKey
                           , round      => Round
                           , signed_tx  => SignedTx })};
derive_reconnect_opts(Params) ->
    lager:debug("Params = ~p", [Params]),
    {error, cannot_derive_reconnect_tx}.

check_reconnect_tx(Tx0) ->
    case aeser_api_encoder:safe_decode(transaction, Tx0) of
        {ok, SignedTxBin} ->
            lager:debug("Decoded (bin): ~p", [SignedTxBin]),
            SignedTx = aetx_sign:deserialize_from_binary(SignedTxBin),
            case aetx:specialize_callback(aetx_sign:tx(SignedTx)) of
                {aesc_client_reconnect_tx = Mod, Tx} ->
                    %% Successful deserialization implies proper tx structure
                    lager:debug("Is reconnect tx", []),
                    {ok, lists:foldl(
                           fun({Key, Method}, Acc) ->
                                   Acc#{Key => Mod:Method(Tx)}
                           end, #{ signed_tx => SignedTx },
                           [ {channel_id, channel_pubkey}
                           , {role      , role}
                           , {pub_key   , origin} ])};
                _Other ->
                    lager:debug("Wrong type tx: ~p", [_Other]),
                    {error, invalid_tx}
            end;
        _DecErr ->
            lager:debug("Decode error: ~p", [_DecErr]),
            {error, invalid_tx}
    end.

set_field(H, host, Val)         -> H#handler{host = Val};
set_field(H, role, Val)         -> H#handler{role = Val};
set_field(H, port, Val)         -> H#handler{port = Val}.

reconnect_opts_to_handler(#{ protocol       := Protocol
                           , role           := Role
                           , channel_id     := ChId
                           , job_id         := JobId }) ->
    %% We don't fill in values like host, port, etc. since we don't
    %% have them, and they aren't needed afaict.
    #handler{ protocol       = Protocol
            , role           = Role
            , channel_id     = ChId
            , enc_channel_id = aeser_api_encoder:encode(channel, ChId)
            , job_id         = JobId }.

prepare_handler(#{<<"protocol">> := Protocol} = Params) ->
    Read =
        fun(Key, RecordField, Opts) ->
            fun(H) ->
                case (sc_ws_utils:read_param(Key, RecordField, Opts))(Params) of
                    not_set -> H;
                    {ok, Val} -> set_field(H, RecordField, Val);
                    {error, _} = Err -> Err
                end
            end
        end,
    Validators =
        [fun(H) ->
            case jobs_ask() of
                {ok, JobId} ->
                    H#handler{job_id = JobId};
                {error, _} ->
                    {error, too_many_ws_sockets}
            end
        end,
        Read(<<"role">>, role, #{type => atom,
                                 enum => [responder, initiator]}),
        fun(#handler{role = Role} = H) ->
            case Role of
                initiator -> % require having a host only for initiator
                    F = Read(<<"host">>, host, #{type => string}),
                    F(H);
                responder -> H
            end
        end,
        Read(<<"port">>, port, #{type => integer})
        ],
    lists:foldl(
        fun(_, {error, _} = Err) -> Err;
            (Fun, Accum) -> Fun(Accum)
        end,
        #handler{protocol    = sc_ws_api:protocol(Protocol)}, Validators);
prepare_handler(_Protocol) ->
    {error, {protocol, missing}}.

update_handler_for_reestablish(Handler, Opts) ->
    lager:debug("Handler = ~p; Opts = ~p", [lager:pr(Handler, ?MODULE), Opts]),
    Conn = maps:get(connection, Opts),
    Handler#handler{ role = maps:get(role, Opts)
                   , host = maps:get(host, Conn, undefined)
                   , port = maps:get(port, Conn) }.

expand_cached_opts(Opts) ->
    {Conn, Opts1} = maps:take(connection, Opts),
    maps:merge(Opts1, Conn).

read_channel_options(Params) ->
    Read = sc_ws_utils:read_f(Params),
    Put = sc_ws_utils:put_f(),
    Error =
        fun({error, _} = Err) ->
            fun(_M) ->
                Err
            end
        end,
    ReadInitiator =
        fun(M) ->
                IType = #{type => {hash, account_pubkey}},
                Type = case maps:get(role, M) of
                           responder -> #{type => {alt, [#{type => atom, enum => [any]},
                                                         IType]}};
                           initiator -> IType
                       end,
                (Read(<<"initiator_id">>, initiator, Type))(M)
        end,
    ReadMap = sc_ws_utils:readmap_f(Params),
    ReadTimeout = ReadMap(timeouts, <<"timeout">>, #{type => integer,
                                                     mandatory => false}),
    ReadReport = ReadMap(report, <<"report">>, #{type => boolean,
                                                     mandatory => false}),
    ReadBHDelta = ReadMap(block_hash_delta, <<"bh_delta">>, #{ type => integer
                                                            , mandatory => false }),
    OnChainOpts =
        case (sc_ws_utils:read_param(
                <<"existing_channel_id">>, existing_channel_id,
                #{type => {hash, channel}, mandatory => false}))(Params) of
            not_set ->  %Channel open scenario
                [ read_role(Read, true)
                , Read(<<"push_amount">>, push_amount, #{type => integer})
                %%, Read(<<"initiator_id">>, initiator, #{type => {hash, account_pubkey}})
                , ReadInitiator
                , Read(<<"responder_id">>, responder, #{type => {hash, account_pubkey}})
                , Read(<<"lock_period">>, lock_period, #{type => integer})
                , Read(<<"channel_reserve">>, channel_reserve, #{type => integer})
                , Read(<<"initiator_amount">>, initiator_amount, #{type => integer})
                , Read(<<"responder_amount">>, responder_amount, #{type => integer})
                ];
            {ok, ExistingID} ->  %Channel reestablish (already opened) scenario
                case aec_chain:get_channel(ExistingID) of
                    {ok, Channel} ->
                        [ Put(existing_channel_id, ExistingID)
                        , Read(<<"offchain_tx">>, offchain_tx, #{ type => serialized_tx
                                                                , mandatory => false })
                        , read_role(Read, false)
                          % push_amount is only used in open and is not preserved.
                          % 0 guarantees passing checks (executed amount check is the
                          % same as onchain check)
                        , Put(push_amount, 0)
                        , Put(initiator, aesc_channels:initiator_pubkey(Channel))
                        , Put(responder, aesc_channels:responder_pubkey(Channel))
                        , Put(lock_period, aesc_channels:lock_period(Channel))
                        , Put(channel_reserve, aesc_channels:channel_reserve(Channel))
                        , Put(initiator_amount, aesc_channels:initiator_amount(Channel))
                        , Put(responder_amount, aesc_channels:responder_amount(Channel))
                        ];
                    {error, _} = Err ->
                        [Error(Err)]
                end;
            {error, _} = Err ->
                [Error(Err)]
        end,
    sc_ws_utils:check_params(
      [ Read(<<"minimum_depth">>, minimum_depth, #{type => integer, mandatory => false})
      , Read(<<"minimum_depth_strategy">>, minimum_depth_strategy,
             #{type => atom, enum => [txfee], mandatory => false})
        %% The state_password is mandatory AFTER the lima fork - this is checked by CheckStatePasswordF
      , Read(<<"state_password">>, state_password, #{type => string, mandatory => false})
      , Read(<<"ttl">>, ttl, #{type => integer, mandatory => false})
      , Read(<<"fee">>, fee, #{type => integer, mandatory => false})
      , Put(noise, [{noise, <<"Noise_NN_25519_ChaChaPoly_BLAKE2b">>}])
      ]
      ++ OnChainOpts
      ++ lists:map(ReadTimeout, aesc_fsm:timeouts() ++ [awaiting_open, initialized])
      ++ lists:map(ReadReport, aesc_fsm:report_tags())
      ++ lists:map(ReadBHDelta, aesc_fsm:bh_deltas())
      ++ general_options(Read)
     ).

general_options(Read) ->
    [ Read(<<"minimum_depth">>, minimum_depth, #{type => integer, mandatory => false})
    , Read(<<"ttl">>, ttl, #{type => integer, mandatory => false})
    , Read(<<"keep_running">>, keep_running, #{type => boolean,
                                               default => <<"true">>,
                                               mandatory => false})
    , Read(<<"reconnect_security">>, reconnect_security, #{mandatory => false,
                                                           type => atom,
                                                           enum => [none, signature],
                                                           default => <<"none">>})
    , Read(<<"slogan">>, slogan, #{type => string, mandatory => false})
    ]
    ++ read_state_password(Read).

read_role(Read, Mandatory) ->
    Read(<<"role">>, role, #{type => atom, enum => [responder, initiator],
                             mandatory => Mandatory}).

read_state_password(Read) ->
    %% The state_password is mandatory AFTER the lima fork - this is checked by CheckStatePasswordF
    CheckStatePasswordF
        = fun(M) ->
                  case aesc_checks:state_password_in_opts(M) of
                      ok ->
                          M;
                      Err ->
                          Err
                  end
          end,
    [read_password(Read), CheckStatePasswordF].

read_password(Read) ->
    Read(<<"state_password">>, state_password, #{type => string, mandatory => false,
                                                 default => <<"correct horse battery staple">>}).

jobs_ask() ->
    jobs:ask(sc_ws_handlers).
