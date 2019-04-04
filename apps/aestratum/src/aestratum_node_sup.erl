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
    #{<<"beneficiaries">> := BenefAddrPctShares,
      <<"reward_last_rounds">> := LastN,
      <<"keys">> := [{<<"dir">>, Dir}]} = maps:from_list(Conf),
    ReadKey = fun (F) -> file:read_file(filename:join([code:priv_dir(aestratum), Dir, F])) end,
    {ok, PubKey}  = ReadKey("sign_key.pub"),
    {ok, PrivKey} = ReadKey("sign_key"),
    Benefs = lists:foldl(fun (Benef, Acc) ->
                                 [Address, PctShare0] = binary:split(Benef, <<":">>),
                                 {account_pubkey, BenefPK} = aehttp_api_encoder:decode(Address),
                                 PctShare = binary_to_number(PctShare0),
                                 true = PctShare > 0,
                                 Acc#{BenefPK => maps:get(BenefPK, Acc, 0) + PctShare}
                         end, #{}, BenefAddrPctShares),
    SumPcts = lists:sum(maps:values(Benefs)),
    SumPcts < 100 orelse error("sum of beneficiaries' percent shares is too high"),
    Childs = [{aestratum_chain, {aestratum_chain, start_link, [SumPcts, {PubKey, PrivKey}]},
               permanent, 5000, worker, [aestratum_chain]},
              {aestratum_reward, {aestratum_reward, start_link, [LastN, {SumPcts, Benefs}]},
               permanent, 5000, worker, [aestratum_reward]}],
    {ok, {{rest_for_one, 5, 10}, Childs}}.


binary_to_number(B) ->
    case catch binary_to_integer(B) of
        {'EXIT', {badarg, _}} -> binary_to_float(B);
        I -> I
    end.
