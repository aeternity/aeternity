-module(aestratum_user_register).

-behaviour(gen_server).

%% API.
-export([start_link/0,
         stop/0,
         add/3,
         add_worker/2,
         del/1,
         member/1, member/2,
         find/1,
         notify/1,
         conn_pids/0,
         size/0
        ]).

%% gen_server.
-export([init/1,
         handle_call/3,
         handle_cast/2
        ]).

-export_type([user/0,
              account/0,
              worker/0
             ]).

-define(SERVER, ?MODULE).

-define(TAB, aestratum_user_register).
-define(TAB_REV, aestratum_user_register_reverse).

-type user()     :: {account(), worker()}.

-type account()  :: binary().

-type worker()   :: binary().

-type conn_pid() :: pid().

-type value()    :: map().

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

-spec add(account(), worker(), conn_pid()) -> ok | {error, already_present}.
add(Account, Worker, ConnPid) when
      is_binary(Account) and is_binary(Worker) and is_pid(ConnPid) ->
    %% Write access to the tables is serialized to avoid race conditions.
    gen_server:call(?SERVER, {add, Account, Worker, ConnPid}).

-spec add_worker(account(), worker()) ->
    ok | {error, worker_count_exhausted | worker_already_present | account_not_found}.
add_worker(Account, Worker) when is_binary(Account) and is_binary(Worker) ->
    gen_server:call(?SERVER, {add_worker, Account, Worker}).

-spec del(account() | conn_pid()) -> ok | {error, not_found}.
del(Account) when is_binary(Account) ->
    gen_server:call(?SERVER, {del_user, Account});
del(ConnPid) when is_pid(ConnPid) ->
    gen_server:call(?SERVER, {del_conn_pid, ConnPid}).

-spec member(account() | conn_pid()) -> boolean().
member(Account) when is_binary(Account) ->
    ets:member(?TAB, Account);
member(ConnPid) when is_pid(ConnPid) ->
    ets:member(?TAB_REV, ConnPid).

-spec member(account(), conn_pid()) -> both | account_only | neither.
member(Account, ConnPid) when is_binary(Account) and is_pid(ConnPid) ->
    case ets:lookup(?TAB, Account) of
        [{Account, #{conn_pid := ConnPid}}] ->
            both;
        [{Account, _Val}] ->
            account_only;
        [] ->
            neither
    end.

-spec find(account() | conn_pid()) -> {ok, value()} | {error, not_found}.
find(Account) when is_binary(Account) ->
    case ets:lookup(?TAB, Account) of
        [{Account, Value}] ->
            {ok, Value};
        [] ->
            {error, not_found}
    end;
find(ConnPid) when is_pid(ConnPid) ->
    case ets:lookup(?TAB_REV, ConnPid) of
        [{ConnPid, Account}] ->
            case ets:lookup(?TAB, Account) of
                [{Account, Value}] ->
                    {ok, maps:without([conn_pid], Value#{account => Account})};
                [] ->
                    {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

-spec notify(term()) -> ok.
notify(Msg) ->
    gen_server:cast(?SERVER, {notify, Msg}).

-spec conn_pids() -> [pid()].
conn_pids() ->
    gen_server:call(?SERVER, conn_pids).

-spec size() -> non_neg_integer().
size() ->
    proplists:get_value(size, ets:info(?TAB)).

%% Callbacks.

init([]) ->
    ?TAB = ets:new(?TAB, [set, public, named_table]),
    ?TAB_REV = ets:new(?TAB_REV, [set, public, named_table]),
    {ok, undefined}.

handle_call({add, Account, Worker, ConnPid}, _From, State) ->
    handle_add(Account, Worker, ConnPid, State);
handle_call({add_worker, Account, Worker}, _From, State) ->
    handle_add_worker(Account, Worker, State);
handle_call({del_user, Account}, _From, State) ->
    handle_del_user(Account, State);
handle_call({del_conn_pid, ConnPid}, _From, State) ->
    handle_del_conn_pid(ConnPid, State);
handle_call(conn_pids, _From, State) ->
    handle_conn_pids(State).

handle_cast({notify, Msg}, State) ->
    handle_notify(Msg, State).

%% Internal functions.

handle_add(Account, Worker, ConnPid, State) ->
    Reply =
        case (not ets:member(?TAB, Account)) andalso
             (not ets:member(?TAB_REV, ConnPid)) of
            true ->
                add_entries(Account, Worker, ConnPid);
            false ->
                {error, already_present}
        end,
    {reply, Reply, State}.

handle_add_worker(Account, Worker, State) ->
    Reply =
        case ets:lookup(?TAB, Account) of
            [{Account, #{workers := Workers} = AValue}] ->
                case maps:find(Worker, Workers) of
                    error ->
                        MaxWorkers = aestratum_env:get(max_workers),
                        case maps:size(Workers) >= MaxWorkers of
                            true ->
                                {error, worker_count_exhausted};
                            false ->
                                Workers1 = Workers#{Worker => timestamp()},
                                AValue1 = AValue#{workers => Workers1},
                                update_entries(Account, AValue1)
                        end;
                    {ok, _WValue} ->
                        {error, worker_already_present}
                end;
            [] ->
                {error, account_not_found}
        end,
    {reply, Reply, State}.

handle_del_user(Account, State) ->
    Reply =
        case ets:lookup(?TAB, Account) of
            [{Account, #{conn_pid := ConnPid}}] ->
                del_entries(Account, ConnPid);
            [] ->
                {error, not_found}
        end,
    {reply, Reply, State}.

handle_del_conn_pid(ConnPid, State) ->
    Reply =
        case ets:lookup(?TAB_REV, ConnPid) of
            [{ConnPid, Account}] ->
                del_entries(Account, ConnPid);
            [] ->
                {error, not_found}
        end,
    {reply, Reply, State}.

handle_conn_pids(State) ->
    ets:safe_fixtable(?TAB, true),
    Reply = get_pids(ets:first(?TAB), []),
    ets:safe_fixtable(?TAB, false),
    {reply, Reply, State}.

handle_notify(Msg, State) ->
    ets:safe_fixtable(?TAB, true),
    send_notify(ets:first(?TAB), Msg),
    ets:safe_fixtable(?TAB, false),
    {noreply, State}.

add_entries(Account, Worker, ConnPid) ->
    Entry = {Account, #{conn_pid => ConnPid,
                        workers => #{Worker => timestamp()}}},
    ets:insert(?TAB, Entry),
    ets:insert(?TAB_REV, {ConnPid, Account}),
    ok.

update_entries(Account, Value) ->
    Entry = {Account, Value},
    ets:insert(?TAB, Entry),
    ok.

del_entries(Account, ConnPid) ->
    ets:delete(?TAB, Account),
    ets:delete(?TAB_REV, ConnPid),
    ok.

get_pids(Account, Acc) when Account =/= '$end_of_table' ->
    [{Account, #{conn_pid := ConnPid}}] = ets:lookup(?TAB, Account),
    get_pids(ets:next(?TAB, Account), [ConnPid | Acc]);
get_pids('$end_of_table', Acc) ->
    Acc.

send_notify(Account, Msg) when Account =/= '$end_of_table' ->
    [{Account, #{conn_pid := ConnPid}}] = ets:lookup(?TAB, Account),
    ConnPid ! Msg,
    send_notify(ets:next(?TAB, Account), Msg);
send_notify('$end_of_table', _Msg) ->
    ok.

timestamp() ->
    aestratum_utils:timestamp().

