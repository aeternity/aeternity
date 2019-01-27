-module(aestratum_dummy_handler).

-behaviour(gen_server).

-export([start_link/1,
         handle_event/2,
         state_to_map/2,
         stop/1
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {
          pid,
          module,
          session
         }).

start_link(Module) ->
    gen_server:start_link(?MODULE, [Module], []).

handle_event(Pid, Event) ->
    gen_server:call(Pid, Event).

state_to_map(Pid, Session) ->
    gen_server:call(Pid, {state_to_map, Session}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

init([Module]) ->
    {ok, #state{pid = self(), module = Module, session = Module:new()}}.

handle_call({conn, Event}, _From,
            #state{module = Module, session = Session} = State) ->
	Res = Module:handle_event({conn, Event}, Session),
    {reply, result(Res, State), State#state{session = session(Res)}};
handle_call({chain, _Event}, Pid, #state{pid = Pid} = State) ->
    %% The chain events originated in the session module will be ignored,
    %% we want to simulate these events from the test process.
    {noreply, State};
handle_call({chain, Event}, _From,
            #state{module = Module, session = Session} = State) ->
	Res = Module:handle_event({chain, Event}, Session),
    {reply, result(Res, State), State#state{session = session(Res)}};
handle_call({state_to_map, Session}, _From,
            #state{module = Module} = State) ->
    {reply, Module:state(Session), State}.

handle_cast(stop, #state{} = State) ->
    {stop, normal, State}.

handle_info(timeout, #state{} = State) ->
    %% NOTE: this shouldn't happen here. The timeouts are
    %% simulated by handle_event.
    {noreply, State};
handle_info(_Info, #state{} = State) ->
    {noreply, State}.

terminate(_Rsn, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


result({send, Data, Session} = Res, #state{module = Module} = State) ->
    case send_data(Data) of
        ok ->
            Res;
        {error, _Rsn} ->
            Res1 = Module:handle_event({conn, close}, Session),
            result(Res1, State)
    end;
result({no_send, _Session} = Res, _State) ->
    Res;
result({stop, _Session} = Res, _State) ->
    Res.

session({_Action, Session}) ->
    Session;
session({send, _Data, Session}) ->
    Session.

send_data(_Data) ->
    ok.
