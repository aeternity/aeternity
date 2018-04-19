-module(sc_ws_handler).

%% API
-export([push/3,
         push_async/3]).

%% WS API
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-record(handler, {fsm_pid            :: pid() | undefined,
                  channel_id         :: aesc_channels:id() | undefined,
                  job_id             :: term(),
                  role               :: initiator | responder | undefined,
                  host               :: binary() | undefined,
                  port               :: non_neg_integer() | undefined}).

-opaque handler() :: #handler{}.
-export_type([handler/0]).

init(Req, _Opts) ->
    {cowboy_websocket, Req, maps:from_list(cowboy_req:parse_qs(Req))}.

-spec websocket_init(map()) -> {ok, handler()} | {stop, undefined}.
websocket_init(Params) ->
    case {prepare_handler(Params), read_channel_options(Params)} of
        {{error, Err}, _} ->
            lager:info("Channel WS failed to start because of ~p; params ~p",
                       [Err, Params]),
            {stop, undefined};
        {_, {error, Err}} ->
            lager:info("Channel WS failed to start because of ~p; params ~p",
                       [Err, Params]),
            {stop, undefined};
        {Handler, ChannelOpts} ->
            lager:info("Starting Channel WS with params ~p", [Params]),
            {ok, FsmPid} = start_link_fsm(Handler, ChannelOpts),
            {ok, Handler#handler{fsm_pid = FsmPid}}
    end.

-spec websocket_handle(term(), handler()) -> {ok, handler()}.
websocket_handle({text, MsgBin}, State) ->
    try jsx:decode(MsgBin, [return_maps]) of
        Msg ->
            case process_incoming(Msg, State) of
                no_reply -> {ok, State};
                {reply, Resp} -> {reply, {text, jsx:encode(Resp)}, State};
                {error, _} -> {ok, State}
            end
    catch
      error:_ ->
          {ok, State}
    end;
websocket_handle(_Data, State) ->
	  {ok, State}.

websocket_info({push, ChannelId, Data, Options}, State) ->
    case ChannelId =:= channel_id(State) of
        false ->
            process_response({error, wrong_channel_id}, Options),
            {ok, State};
        true ->
            process_response(ok, Options),
            Msg = jsx:encode(Data),
	          {reply, {text, Msg}, State}
    end;
websocket_info({aesc_fsm, FsmPid, ChannelId, Msg}, #handler{fsm_pid=FsmPid}=H) ->
    H1 = case channel_id(H) of
            undefined -> H#handler{channel_id = ChannelId};
            ChannelId -> H % assert no channel id change
         end,
    case process_fsm(Msg) of
        no_reply -> {ok, H1};
        {reply, Resp} -> {reply, {text, jsx:encode(Resp)}, H1};
        {error, _} -> {ok, H1}
    end;
websocket_info(_Info, State) ->
	  {ok, State}.

terminate(_Reason, _PartialReq, #{} = _State) ->
    ok;
terminate(_Reason, _PartialReq, State) ->
    jobs:done(job_id(State)),
    ok.

-spec job_id(handler()) -> term().
job_id(#handler{job_id = JobId}) ->
    JobId.

-spec channel_id(handler()) -> aesc_channels:id() | undefined.
channel_id(#handler{channel_id = ChannelId}) ->
    ChannelId.

-spec fsm_pid(handler()) -> pid() | undefined.
fsm_pid(#handler{fsm_pid = Pid}) ->
    Pid.

-spec push(pid(), aesc_channels:id(), map()) -> ok | {error, timeout}
                                                   | {error, noproc}
                                                   | {error, wrong_channel_id}.
push(WsPid, ChannelId, Data) ->
    WsPid ! {push, ChannelId, Data, [{sender, self()}]},
    receive
        {ws_proc_response, Resp} -> Resp
    after 5000 ->
        case is_ws_alive(WsPid) of
            false -> {error, noproc};
            true -> {error, timeout}
        end
    end.

-spec push_async(pid(), aesc_channels:id(), map()) -> ok | {error, noproc}.
push_async(WsPid, ChannelId, Data) ->
    case is_ws_alive(WsPid) of
        false -> {error, noproc};
        true ->
            WsPid ! {push, ChannelId, Data, []},
            ok
    end.

-spec process_response(term(), list()) -> ok.
process_response(Response, Options) ->
    lists:foreach(
        fun({Key, Fun}) ->
            case proplists:get_value(Key, Options) of
                undefined -> pass;
                Value -> Fun(Value)
            end
        end,
        [{sender, fun(SenderPid) -> SenderPid ! {ws_proc_response, Response} end}]),
    ok.
          
-spec is_ws_alive(pid()) -> boolean().
is_ws_alive(Pid) ->
    case erlang:process_info(Pid) of
        undefined -> false;
        _ -> true
    end.

-spec start_link_fsm(handler(), map()) -> {ok, pid()}.
start_link_fsm(#handler{role = initiator, host=Host, port=Port}, Opts) ->
    {ok, _Pid} = aesc_fsm:initiate(Host, Port, Opts);
start_link_fsm(#handler{role = responder, port=Port}, Opts) ->
    {ok, _Pid} = aesc_fsm:participate(Port, Opts).

set_field(H, host, Val) -> H#handler{host = Val};
set_field(H, role, Val) -> H#handler{role = Val};
set_field(H, port, Val) -> H#handler{port = Val}.

-spec read_param(binary(), atom(), map()) -> fun((map()) -> {ok, term()} |
                                                            not_set |
                                                            {error, atom()}).
read_param(ParamName, RecordField, Options) ->
    fun(Params) ->
        Mandatorary = maps:get(mandatorary, Options, true),
        case maps:get(ParamName, Params) of
            undefined when Mandatorary ->
                ErrAtom = list_to_atom(atom_to_list(RecordField) ++ "_missing"),
                {error, ErrAtom};
            undefined when not Mandatorary ->
                not_set;
            Val0 ->
                Type = maps:get(type, Options, binary),
                case parse_by_type(Type, Val0, RecordField) of
                    {error, _} = Err -> Err;
                    {ok, Val} ->
                        case maps:get(enum, Options, undefined) of
                            undefined ->  {ok, Val};
                            AllowedVals when is_list(AllowedVals) ->
                                case lists:member(Val, AllowedVals) of
                                    true -> {ok, Val};
                                    false ->
                                        ErrAtom = list_to_atom("invalid_" ++
                                                               atom_to_list(RecordField)),
                                        {error, ErrAtom}
                                end
                        end
                end
        end
    end.

parse_by_type(binary, V, _) when is_binary(V) ->
    {ok, V};
parse_by_type(string, V, _) when is_binary(V) ->
    {ok, binary_to_list(V)};
parse_by_type(atom, V, _) when is_binary(V) ->
    {ok, binary_to_existing_atom(V, utf8)};
parse_by_type(integer, V, _) when is_binary(V) ->
    {ok, list_to_integer(binary_to_list(V))};
parse_by_type({hash, Type}, V, RecordField) when is_binary(V) ->
    case aec_base58c:safe_decode(Type, V) of
        {error, _} ->
            ErrAtom = list_to_atom("encoding_" ++ atom_to_list(RecordField)),
            {error, ErrAtom};
        {ok, _} = OK -> OK
    end.

-spec process_incoming(map(), handler()) -> no_reply | {reply, map()} | {error, atom()}.
process_incoming(#{<<"action">> := ActorSigned,
                   <<"payload">> := #{<<"tx">> := EncodedTx}}, State)
    when ActorSigned =:= <<"initiator_signed">> orelse
         ActorSigned =:= <<"responder_signed">> ->
    Tag =
        case ActorSigned of
            <<"initiator_signed">> -> create_tx;
            <<"responder_signed">> -> funding_created
        end,
    case aec_base58c:safe_decode(transaction, EncodedTx) of
        {error, _} ->
            lager:warning("Channel WS: broken ~p tx ~p", [ActorSigned, EncodedTx]),
            {error, invalid_tx};
        {ok, TxBin} ->
             SignedTx = aetx_sign:deserialize_from_binary(TxBin),%TODO: check tx
             aesc_fsm:signing_response(fsm_pid(State), Tag, SignedTx),
             no_reply
    end;
process_incoming(#{<<"action">> := Unhandled}, _State) ->
    lager:warning("Channel WS: unhandled action received ~p", [Unhandled]),
    {error, unhandled};
process_incoming(Msg, _State) ->
    lager:warning("Channel WS: missing action received ~p", [Msg]),
    {error, unhandled}.

-spec process_fsm(term()) -> no_reply | {reply, map()} | {error, atom()}.
process_fsm({info, Event}) ->
    {reply, #{action => <<"info">>,
              payload => #{event => Event}}};
process_fsm({sign, Tag, Tx}) when Tag =:= create_tx
                           orelse Tag =:= funding_created ->
    EncTx = aec_base58c:encode(transaction, aetx:serialize_to_binary(Tx)),
    {reply, #{action => <<"sign">>,
              payload => #{tx => EncTx}}}.

prepare_handler(Params) ->
    Read =
        fun(Key, RecordField, Opts) -> 
            fun(H) ->
                case (read_param(Key, RecordField, Opts))(Params) of
                    not_set -> H;
                    {ok, Val} -> set_field(H, RecordField, Val);
                    {error, _} = Err -> Err
                end
            end
        end,
    Validators =
        [fun(H) ->
            case jobs:ask(ws_handlers_queue) of
                {ok, JobId} ->
                    H#handler{job_id = JobId};
                {error, _} ->
                    {error, too_much_ws_sockets}
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
        #handler{}, Validators).

read_channel_options(Params) ->
    Read =
        fun(KeyBin, Key, Opts) -> 
            fun(M) ->
                case (read_param(KeyBin, Key, Opts))(Params) of
                    not_set -> M;
                    {ok, Val} -> maps:put(Key, Val, M);
                    {error, _} = Err -> Err
                end
            end
        end,
    Put =
        fun(K, V) ->
            fun(M) -> maps:put(K, V, M) end
        end,
    lists:foldl(
        fun(_, {error, _} = Err) -> Err;
            (Fun, Accum) -> Fun(Accum)
        end,
        #{},
        [Read(<<"initiator">>, initiator, #{type => {hash, account_pubkey}}),
         Read(<<"responder">>, participant, #{type => {hash, account_pubkey}}),
         Read(<<"lock_period">>, lock_period, #{type => integer}),
         Read(<<"push_amount">>, push_amount, #{type => integer}),
         Read(<<"initiator_amount">>, initiator_amount, #{type => integer}),
         Read(<<"responder_amount">>, participant_amount, #{type => integer}),
         Read(<<"channel_reserve">>, channel_reserve, #{type => integer}),
         Read(<<"ttl">>, ttl, #{type => integer}),
         Put(noise, [{noise, <<"Noise_NN_25519_ChaChaPoly_BLAKE2b">>}]),
         Put(report_info, true)
        ]).

