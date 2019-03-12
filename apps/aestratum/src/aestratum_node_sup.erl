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
    {ok, Conf} = aeu_env:user_config(<<"stratum">>),
    #{<<"beneficiaries">> := Benefs,
      <<"reward_last_rounds">> := LastN} = maps:from_list(Conf),
    Beneficiaries =
        lists:foldl(fun (Benef, M) ->
                            [Addr, ShareBin] = binary:split(Benef, <<":">>),
                            Share = binary_to_number(ShareBin),
                            M#{Addr => maps:get(Addr, M, 0) + Share}
                    end, #{}, Benefs),
    Childs = [{aestratum_chain, {aestratum_chain, start_link, []},
               permanent, 5000, worker, [aestratum_chain]},
              {aestratum_reward, {aestratum_reward, start_link, [LastN, Beneficiaries]},
               permanent, 5000, worker, [aestratum_reward]}],
    {ok, {{rest_for_one, 5, 10}, Childs}}.


binary_to_number(B) ->
    case catch binary_to_integer(B) of
        {'EXIT', {badarg, _}} -> binary_to_float(B);
        I -> I
    end.
