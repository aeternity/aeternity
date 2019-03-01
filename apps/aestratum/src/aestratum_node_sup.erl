-module(aestratum_node_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->

    %% TODO: read N rounds back from config file
    LastN = 2,

    %% TODO: read beneficiaries from config file
    Beneficiaries = #{<<"benef_1">> => 1,
                      <<"benef_2">> => 2,
                      <<"benef_4">> => 4,
                      <<"benef_8">> => 8},

    Childs = [{aestratum_chain, {aestratum_chain, start_link, []},
               permanent, 5000, worker, [aestratum_chain]},
              {aestratum_reward, {aestratum_reward, start_link, [LastN, Beneficiaries]},
               permanent, 5000, worker, [aestratum_reward]}],
    {ok, {{rest_for_one, 5, 10}, Childs}}.
