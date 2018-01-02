-module(aec_metrics_rpt_dest).
-behaviour(gen_server).

%% Load rules from config
-export([check_config/0]).

%% Query api
-export([get_destinations/2,
         get_destinations/3,
         add_destination/3,
         del_destination/3]).

%% start function
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(metric, {key,
                 destinations = []}).

-record(st, {tab}).

-define(IS_DEST(D), D==log; D==send).
-define(TAB, ?MODULE).
-define(SERVER, ?MODULE).

get_destinations(Name, DP) ->
    %% TODO: come up with a more efficient handling of default datapoints
    case ets:lookup(?TAB, {Name, DP}) of
        [#metric{destinations = Ds}] ->
            Ds;
        [] ->
            case ets:lookup(?TAB, {Name, default}) of
                [#metric{destinations = Ds}] ->
                    Ds;
                [] ->
                    undefined
            end
    end.

get_destinations(Name, DP, Default) when is_list(Default) ->
    case get_destinations(Name, DP) of
        undefined ->
            Default;
        Ds ->
            Ds
    end.

add_destination(Name, DP, Dest) when ?IS_DEST(Dest) ->
    gen_server:call(?SERVER, {add_destination, Name, DP, Dest}).

del_destination(Name, DP, Dest) when ?IS_DEST(Dest) ->
    gen_server:call(?SERVER, {del_destination, Name, DP, Dest}).

check_config() ->
    gen_server:call(?SERVER, check_config).


start_link() ->
    T = ensure_tab(),
    case gen_server:start_link({local, ?SERVER}, ?MODULE, T, []) of
        {ok, Pid} = Ok ->
            ets:give_away(T, Pid, []),
            Ok;
        Other ->
            Other
    end.

init(Tab) ->
    {ok, #st{tab = Tab}}.

handle_call({add_destination, Name, DP, Dest}, _From, #st{tab = T} = St) ->
    case ets:lookup(T, {Name, DP}) of
        [#metric{destinations = Ds} = M] ->
            case lists:member(Dest, Ds) of
                true ->
                    {reply, ok, St};
                false ->
                    M1 = M#metric{destinations = [Dest|Ds]},
                    ets:insert(T, M1),
                    {reply, ok, St}
            end;
        [] ->
            ets:insert(T, #metric{key = {Name, DP},
                                  destinations = [Dest]}),
            {reply, ok, St}
    end;
handle_call({del_destination, Name, DP, Dest}, _From, #st{tab = T} = St) ->
    Res = case ets:lookup(T, {Name, DP}) of
              [#metric{destinations = Ds} = M] ->
                  case lists:member(Dest, Ds) of
                      true ->
                          ets:insert(T, M#metric{destinations = Ds -- [Dest]}),
                          true;
                false ->
                    false
                  end;
              [] ->
                  false
          end,
    {reply, Res, St};
handle_call(check_config, _From, #st{tab = T} = St) ->
    populate_tab(T),
    lager:debug("Dest tab = ~p", [ets:tab2list(T)]),
    {reply, ok, St};
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_From, St, _Extra) ->
    {ok, St}.


%% ======================================================================
%% Private functions
%% ======================================================================

ensure_tab() ->
    case ets:info(?TAB, size) of
        undefined ->
            T = ets:new(?TAB, [set, public, named_table,
                               {keypos, #metric.key},
                               {heir, self(), []}]),
            T;
        _ ->
            ?TAB
    end.

populate_tab(Tab) ->
    case aeu_env:user_map([<<"metrics">>,<<"rules">>]) of
        {ok, Rules} ->
            lists:foreach(
              fun(#{} = R) ->
                      Name = maps:get(<<"name">>, R, <<"*">>),
                      Type = maps:get(<<"type">>, R, <<"*">>),
                      DPs  = maps:get(<<"datapoints">>, R, <<"default">>),
                      Actions = maps:get(<<"actions">>, R, <<"log,send">>),
                      Cmd = <<"type=",Type/binary,"/",
                              Name/binary,"/",
                              DPs/binary>>,
                      try
                          Found = aec_metrics_lib:find_entries_cmd([Cmd]),
                          make_destinations(Found, Actions, Tab)
                      catch
                          error:E ->
                              lager:error("CRASH: ~p~n~p~n",
                                          [E, erlang:get_stacktrace()])
                      end
              end, Rules);
        _ ->
            lager:debug("no metrics rules defined", [])
    end,
    ok.

make_destinations(Found, Actions0, Tab) ->
    Dests = mk_dests_(Actions0),
    lists:foreach(
      fun({Metrics, DPs}) ->
              [ets:insert(Tab, #metric{key = {N,D}, destinations = Dests})
               || {N, _, _} <- Metrics,
                  D <- DPs]
      end, Found).

mk_dests_(Txt) ->
    case ordsets:from_list([binary_to_existing_atom(B, latin1)
                            || B <- re:split(Txt, ",", [{return,binary}])]) of
        [none] ->
            [];
        Other ->
            Other
    end.
