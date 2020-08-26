-module(aehc_chain_sim_connector).

-behaviour(aehc_connector).

-behaviour(gen_server).

%% API.

-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-export([send_tx/1, get_block/1]).

%% API.

id() -> 
    ?MODULE.

-spec start_link() ->
                        {ok, pid()} | ingnore | {error, term()}.
start_link() ->
    Id = id(),
    gen_server:start_link({local, Id}, ?MODULE, [], []).

%%%===================================================================
%%%  aehc_connector behaviour
%%%===================================================================

-spec send_tx(Tx::aetx:tx()) -> binary().
send_tx(Tx) ->
    gen_server:call(id(), {send_tx, Tx}).

-spec get_block(Num::integer()) -> aehc_connector:block().
get_block(Num) ->
    gen_server:call(id(), {get_block, Num}).

%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

-record(state, { pid::pid() }).

init([]) ->
    %% TODO TO ask opt from config;
    Opts = #{},
    {ok, Pid} = aec_chain_sim:start(Opts),
    io:fwrite("Parent chain's connector ~p is attached: ~p", [?MODULE, Pid]),
    {ok, #state{ pid=Pid }}.

handle_call({send_tx, Tx}, _From, State) ->
    Tx = #{},
    Res = aec_chain_sim:push(Tx),
    %% TODO Could we optimize aec_chain_sim:push/1 to return hash instead of ok?
    %% TODO To fill the Tx by actual data;
    {reply, Res, State};

handle_call({get_block, _Num}, _From, State) ->
    Header = aehc_connector:header(<<>>, 0),
    Res = aehc_connector:block(Header, []),
    {reply, Res, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
