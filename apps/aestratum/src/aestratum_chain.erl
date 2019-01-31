-module(aestratum_chain).

-behaviour(gen_server).

%% TODO: eunit
%% TODO: add new event to the node
%% TODO: type specs

%% API.
-export([start_link/0,
         subscribe/0
%%         submit/1
        ]).

%% gen_server.
-export([init/1,
         handle_call/3,
         handle_cast/2
        ]).

-define(SERVER, ?MODULE).

-record(state, {
          header_hash,
          target
         }).

%% API.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

subscribe() ->
    aestratum_pubsub:subscribe(new_block).

unsubsribe() ->
    aestratum_pubsub:unsubscribe(new_block).

%% Callbacks.

init([]) ->
    %% TODO: create an event in the node
    aec_events:subscribe(new_pending_key_block),
    {ok, undefined}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

%% micro block candidate
handle_info({gproc_ps_event, new_pending_key_block, #{info := Info}}, State) ->
    {noreply, handle_new_pending_key_block(Info, State)}.

%% Internal functions.

handle_new_pending_key_block({HdrHash, Target}, State) ->
    Info = #{header_hash => HdrHash, target => Target},
    aestratum_pubsub:publish(new_block, Info),
    State#state{header_hash = HdrHash, target = Target}.

