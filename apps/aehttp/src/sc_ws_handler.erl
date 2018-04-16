-module(sc_ws_handler).

%% API
-export([push/3,
         push_async/3]).
-export([default_opts/0]).

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
    Res =
        lists:foldl(
            fun(_, {error, _} = Err) -> Err;
               (Fun, Accum) -> Fun(Accum)
            end,
            #handler{}, Validators),
    case Res of
        {error, Err} ->
            lager:info("Channel WS failed to start because of ~p; params ~p",
                       [Err, Params]),
            {stop, undefined};
        Handler ->
            lager:info("Starting Channel WS with params ~p", [Params]),
            {ok, FsmPid} = start_link_fsm(Handler),
            {ok, Handler#handler{fsm_pid = FsmPid}}
    end.

-spec websocket_handle(term(), handler()) -> {ok, handler()}.
websocket_handle({text, Msg}, State) ->
	  {reply, {text, << "That's what she said! ", Msg/binary >>}, State};
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

-spec channel_id(handler()) -> aesc_channels:id().
channel_id(#handler{channel_id = ChannelId}) ->
    ChannelId.

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

-spec start_link_fsm(handler()) -> {ok, pid()}.
start_link_fsm(#handler{role = initiator, host=Host, port=Port}) ->
    {ok, _Pid} = aesc_fsm:initiate(Host, Port, default_opts());
start_link_fsm(#handler{role = responder, port=Port}) ->
    {ok, _Pid} = aesc_fsm:participate(Port, default_opts()).

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
                Val =
                    case Type of
                        binary -> Val0;
                        string -> binary_to_list(Val0);
                        atom -> binary_to_existing_atom(Val0, utf8);
                        integer -> list_to_integer(binary_to_list(Val0))
                    end,
                case maps:get(enum, Options, undefined) of
                    undefined ->  {ok, Val};
                    AllowedVals when is_list(AllowedVals) ->
                        case lists:member(Val, AllowedVals) of
                            true -> {ok, Val};
                            false ->
                                ErrAtom = list_to_atom("invalid_" ++ atom_to_list(RecordField)),
                                {error, ErrAtom}
                        end
                end
        end
    end.

default_opts() ->
    #{ lock_period        => 10
     , initiator          => random_hash()
     , participant        => random_hash()
     , push_amount        => 10 
     , initiator_amount   => 10
     , participant_amount => 10
     , channel_reserve    => 15
     , noise              => [{noise, <<"Noise_NN_25519_ChaChaPoly_BLAKE2b">>}]
     , report_info        => true}.

random_hash() ->
    HList =
        lists:map(
            fun(_) -> rand:uniform(255) end,
            lists:seq(1, 65)),
    list_to_binary(HList).
