-module(pubsub_ws_handler).

%% WS API
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).


-record(handler, {
                  protocol           :: pubsub_ws_api:protocol(),
                  state              :: pubsub_ws_api:state()}).

-opaque handler() :: #handler{}.
-export_type([handler/0]).

%% ===========================================================================

init(Req, _Opts) ->
    Parsed = cowboy_req:parse_qs(Req),
    lager:debug("init(~p, ~p), Parsed ~p",
        [Req, _Opts, Parsed]),
    {cowboy_websocket, Req,
     maps:from_list(Parsed)}.

-spec websocket_init(map()) -> {ok, handler()} | {stop, undefined}.
websocket_init(Params) ->
    Handler = prepare_handler(Params),
    {ok, Handler}.


websocket_handle({text, MsgBin}, #handler{protocol = Protocol
                                         ,state    = State} = Handler) ->
    case pubsub_ws_api:process_from_client(Protocol, MsgBin, State) of
        no_reply                 -> {ok, Handler};
        {reply, Reply}           ->
            {reply, {text, Reply}, Handler};
        {reply, Reply, NewState} ->
            {reply, {text, Reply}, Handler#handler{state = NewState}}
    end;
websocket_handle(_Data, Handler) ->
    {ok, Handler}.

websocket_info({gproc_ps_event, _, _} = Msg, #handler{protocol = Protocol
                                                     ,state    = State} = Handler) ->
    case pubsub_ws_api:process_from_event(Protocol, Msg, State) of
        no_reply                 -> {ok, Handler};
        {reply, Reply}           ->
            {reply, {text, Reply}, Handler}
    end;
websocket_info(_Info, Handler) ->
    {ok, Handler}.

terminate(Reason, _PartialReq, #{} = _H) ->
    lager:debug("WebSocket dying because of ~p", [Reason]),
    % not initialized yet
    ok;
terminate(Reason, _PartialReq, _State) ->
    lager:debug("WebSocket dying because of ~p", [Reason]),
    ok.

prepare_handler(#{<<"protocol">> := ProtocolBin}) ->
    Protocol = pubsub_ws_api:protocol(ProtocolBin),
    State = pubsub_ws_api:init(Protocol),
    #handler{protocol = Protocol, state = State};
prepare_handler(_Protocol) ->
    {error, {protocol, missing}}.
