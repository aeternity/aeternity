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

-record(st, {rules = [],
             tab}).

-define(IS_DEST(D), D==log; D==send).
-define(TAB, ?MODULE).
-define(SERVER, ?MODULE).

get_destinations(Name, DP) ->
    %% TODO: come up with a more efficient handling of default datapoints
    case get_destinations_(Name, DP) of
        undefined ->
            gen_server:call(?SERVER, {check_destination, Name}),
            get_destinations_(Name, DP);
        Dests ->
            Dests
    end.

get_destinations_(Name, DP) ->
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
    {ok, update_rules(#st{tab = Tab})}.

handle_call({add_destination, Name, DP, Dest}, _From, #st{tab = T} = St) ->
    case ets:lookup(T, {Name, DP}) of
        [#metric{destinations = Ds} = M] ->
            case lists:member(Dest, Ds) of
                true ->
                    {reply, ok, St};
                false ->
                    M1 = M#metric{destinations = [Dest|Ds]},
                    ets_insert(T, M1),
                    {reply, ok, St}
            end;
        [] ->
            ets_insert(T, #metric{key = {Name, DP},
                                  destinations = [Dest]}),
            {reply, ok, St}
    end;
handle_call({del_destination, Name, DP, Dest}, _From, #st{tab = T} = St) ->
    Res = case ets:lookup(T, {Name, DP}) of
              [#metric{destinations = Ds} = M] ->
                  case lists:member(Dest, Ds) of
                      true ->
                          ets_insert(T, M#metric{destinations = Ds -- [Dest]}),
                          true;
                false ->
                    false
                  end;
              [] ->
                  false
          end,
    {reply, Res, St};
handle_call({check_destination, Name}, _From, #st{} = St) ->
    case exometer:info(Name, entry) of
        undefined ->
            {reply, ok, St};
        E ->
            Type = exometer:info(E, type),
            Status = exometer:info(E, status),
            St1 = check_and_cache_destination([{Name, Type, Status}], St),
            {reply, ok, St1}
    end;
handle_call(check_config, _From, #st{} = St) ->
    St1 = update_rules(St),
    St2 = populate_tab(St1),
    lager:debug("Dest tab = ~p", [ets:tab2list(St2#st.tab)]),
    {reply, ok, St2};
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

update_rules(#st{} = St) ->
    case aeu_env:user_map([<<"metrics">>,<<"rules">>]) of
        {ok, Rules} ->
            St#st{rules = Rules};
        _ ->
            lager:debug("no metrics rules defined", []),
            St#st{rules = []}
    end.

check_and_cache_destination(Entries, St) ->
    populate_tab(fun(Cmds) ->
                         aec_metrics_lib:filter_entries(
                           Entries, Cmds)
                 end, St).

populate_tab(#st{} = St) ->
    populate_tab(fun(Cmds) ->
                         aec_metrics_lib:find_entries_cmd(Cmds)
                 end, St).

populate_tab(Select, #st{rules = Rules, tab = Tab} = St) ->
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
                  Found = Select([Cmd]),
                  make_destinations(Found, Actions, Tab)
              catch
                  error:E ->
                      lager:error("CRASH: ~p; ~p",
                                  [E, erlang:get_stacktrace()]),
                      []
              end
      end, Rules),
    St.

make_destinations(Found, Actions0, Tab) ->
    Dests = mk_dests_(Actions0),
    lists:foreach(
      fun({Metrics, DPs}) ->
              [insert_new(Tab, #metric{key = {N,D}, destinations = Dests})
               || {N, _, _} <- Metrics,
                  D <- DPs]
      end, Found).

insert_new(Tab, #metric{} = M) ->
    case ets_insert_new(Tab, M) of
        false ->
            [Obj] = ets_lookup(Tab, M#metric.key),
            Obj;
        true ->
            M
    end.

%% wrapper functions around ets ops to facilitate tracing
ets_insert(Tab, Obj) ->
    ets:insert(Tab, Obj).

ets_insert_new(Tab, Obj) ->
    ets:insert_new(Tab, Obj).

ets_lookup(Tab, Key) ->
    ets:lookup(Tab, Key).

mk_dests_(Txt) ->
    case ordsets:from_list([binary_to_existing_atom(B, latin1)
                            || B <- re:split(Txt, ",", [{return,binary}])]) of
        [none] ->
            [];
        Other ->
            Other
    end.
