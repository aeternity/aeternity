-module(ws_task_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([execute/1]).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {}).

execute(Msg) ->
    WsPid = self(),
    spawn(fun() -> ws_task_worker_sup:spawn_child_for_msg(Msg, WsPid) end).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init([ws_task_worker_args]) ->
    {ok, #state{}}.

handle_call({handle_msg, {MsgBin, WsPid}}, _From, #state{}=State) ->
    #{<<"target">> := Target0,
      <<"action">> := Action0} = Msg = jsx:decode(MsgBin, [return_maps]),
    Payload = maps:get(<<"payload">>, Msg, #{}),
    Target = binary_to_existing_atom(Target0, utf8),
    Action = binary_to_existing_atom(Action0, utf8),
    Response0 = ws_int_dispatch:execute(Target, Action, Payload#{<<"ws_pid">> => WsPid}),
    case Response0 of
        {error, ErrMsg} ->
            lager:info("WS request with target ~p, action ~p and payload ~p failed with reason ~p",
                       [Target, Action, Payload, ErrMsg]),
            pass;
        {ok, O, A, P} ->
            Tag = maps:get(<<"tag">>, Msg, untagged),
            ws_handler:send_msg(WsPid, O, A, Tag, P)
    end,
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

