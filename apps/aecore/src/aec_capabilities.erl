-module(aec_capabilities).

-behavior(gen_server).


-export([ set_capability/2
        , get_capability/1
        , delete_capability/1
        , get_capabilities/0
        , register_peer/2
        , remove_peer/1
        , peers_with_capability/1 ]).

%% Gen_server stuff
-export([ start_link/0 ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_capability(Key) ->
    call({get_capability, Key}).

set_capability(Key, Value) ->
    call({set_capability, Key, Value}).

delete_capability(Key) ->
    call({delete_capability, Key}).

get_capabilities() ->
    call(get_capabilities).

register_peer(Peer, Capabilities) when is_list(Capabilities) ->
    call({register_peer, Peer, maps:from_list(Capabilities)}).

remove_peer(Peer) ->
    call({remove_peer, Peer}).

peers_with_capability(Key) ->
    call({peers_with_capability, Key}).

call(Req) ->
    gen_server:call(?MODULE, Req).


init([]) ->
    {ok, #{ peers => #{}
          , capabilities => #{} }}.

handle_call({get_capability, Key}, _From, #{capabilities := Cs} = St) ->
    {reply, maps:find(Key, Cs), St};
handle_call({set_capability, Key, Value}, _From, #{capabilities := Cs} = St) ->
    {reply, ok, St#{capabilities := Cs#{Key => Value}}};
handle_call({delete_capability, Key}, _From, #{capabilities := Cs} = St) ->
    {reply, ok, St#{capabilities := maps:remove(Key, Cs)}};
handle_call(get_capabilities, _From, #{capabilities := Cs} = St) ->
    {reply, maps:to_list(Cs), St};
handle_call({register_peer, PeerId, Capabilities}, _From, #{peers := Peers} = St) ->
    {reply, ok, St#{peers := Peers#{PeerId => Capabilities}}};
handle_call({peers_with_capability, Key}, _From, #{peers := Peers} = St) ->
    Result = maps:fold(fun(PeerId, #{Key := Caps}, Acc) ->
                            [{PeerId, Caps}|Acc];
                           (_,_, Acc) -> Acc
                       end, [], Peers),
    {reply, Result, St}.

handle_cast(_, St) ->
    {noreply, St}.

handle_info(_, St) ->
    {noreply, St}.

terminate(_Reason, _State) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.
