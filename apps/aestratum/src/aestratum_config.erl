-module(aestratum_config).

-export([read/0]).

-spec read() -> {ok, map()} | {error, term()}.
read() ->
    {ok, Cfg} = aeu_env:user_config(<<"stratum">>),
    Cfg1 = maps:from_list(Cfg),
    Cfg2 = #{conn_cfg    => conn_config(Cfg1),
             session_cfg => session_config(Cfg1),
             reward_cfg  => reward_config(Cfg1)},
    case validate_config(Cfg2) of
        {ok, C} = Res ->
            setup_env(C),
            Res;
        {error, _Rsn} = Res ->
            Res
    end.

conn_config(Cfg) ->
    Host = maps:get(<<"host">>, Cfg, <<"pool.aeternity.com">>),
    Port = maps:get(<<"port">>, Cfg, 9999),
    Transport = maps:get(<<"transport">>, Cfg, <<"tcp">>),
    MaxConns = maps:get(<<"max_connections">>, Cfg, 1024),
    NAcceptors = maps:get(<<"num_acceptors">>, Cfg, 100),
    #{host       => Host,
      port       => Port,
      transport  => Transport,
      max_conns  => MaxConns,
      nacceptors => NAcceptors}.

session_config(Cfg) ->
    ExtraNonceNBytes = maps:get(<<"extra_nonce_bytes">>, Cfg, 4),
    %% TODO: what the initial share target should be?
    InitialShareTarget = maps:get(<<"initial_share_target">>, Cfg, 1),
    %% TODO: what the max share target should be?
    MaxShareTarget = maps:get(<<"max_share_target">>, Cfg, aestratum_target:max()),
    DesiredSolveTime = maps:get(<<"desired_solve_time">>, Cfg, 30) * 1000,
    MaxSolveTime = maps:get(<<"max_solve_time">>, Cfg, 60) * 1000,
    ShareTargetDiffThreshold = maps:get(<<"share_target_diff_threshold">>, Cfg, 5.0),
    MsgTimeout = maps:get(<<"msg_timeout">>, Cfg, 15) * 1000,
    MaxJobs = maps:get(<<"max_jobs">>, Cfg, 20),
    #{extra_nonce_nbytes          => ExtraNonceNBytes,
      initial_share_target        => InitialShareTarget,
      max_share_target            => MaxShareTarget,
      desired_solve_time          => DesiredSolveTime,
      max_solve_time              => MaxSolveTime,
      share_target_diff_threshold => ShareTargetDiffThreshold,
      max_jobs                    => MaxJobs,
      msg_timeout                 => MsgTimeout}.

reward_config(Cfg) ->
    BeneficiariesAddrPctShares = maps:get(<<"beneficiaries">>, Cfg),
    RewardLastNRounds = maps:get(<<"reward_last_rounds">>, Cfg, 2),
    [{<<"dir">>, Dir}] = maps:get(<<"keys">>, Cfg),
    {ok, PubKey}  = read_key(Dir, "sign_key.pub"),
    {ok, PrivKey} = read_key(Dir, "sign_key"),
    Beneficiaries =
        lists:foldl(fun (Bnf, Acc) ->
                            [BnfPK0, PctShare0] = binary:split(Bnf, <<":">>),
                            %% TODO: get rid of aehttp_api_encoder
                            {account_pubkey, BnfPK} = aehttp_api_encoder:decode(BnfPK0),
                            PctShare = binary_to_number(PctShare0),
                            Acc#{BnfPK => maps:get(BnfPK, Acc, 0) + PctShare}
                    end, #{}, BeneficiariesAddrPctShares),
    #{reward_last_nrounds  => RewardLastNRounds,
      key_pair             => {PubKey, PrivKey},
      beneficiaries        => Beneficiaries,
      beneficiaries_reward => lists:sum(maps:values(Beneficiaries))}.

validate_config(Cfg) ->
    case run([fun check_extra_nonce_nbytes/1,
              fun check_beneficiaries_reward/1], Cfg) of
        ok                  -> {ok, Cfg};
        {error, _Rsn} = Res -> Res
    end.

setup_env(#{conn_cfg := ConnCfg, session_cfg := SessionCfg}) ->
    set_env(host, ConnCfg),
    set_env(port, ConnCfg),
    set_env(msg_timeout, SessionCfg),
    set_env(extra_nonce_nbytes, SessionCfg),
    set_env(initial_share_target, SessionCfg),
    set_env(share_target_diff_threshold, SessionCfg),
    set_env(desired_solve_time, SessionCfg),
    set_env(max_solve_time, SessionCfg),
    set_env(max_jobs, SessionCfg).

set_env(Key, Cfg) ->
    application:set_env(aestratum, Key, maps:get(Key, Cfg)).

run([Fun | Funs], Cfg) ->
    case Fun(Cfg) of
        ok                  -> run(Funs, Cfg);
        {error, _Rsn} = Res -> Res
    end;
run([], _Cfg) ->
    ok.

check_extra_nonce_nbytes(#{conn_cfg := ConnCfg, session_cfg := SessionCfg}) ->
    %% The maximum number of connections is a soft limit. In practice, it can
    %% reach max_connections + the number of acceptors.
    %% Each client is assigned an unique extra nonce, there needs to be (much)
    %% more extra nonce values available than possible connections.
    MaxConns = maps:get(max_conns, ConnCfg),
    NAcceptors = maps:get(nacceptors, ConnCfg),
    ExtraNonceNBytes = maps:get(extra_nonce_nbytes, SessionCfg),
    MaxExtraNonce = aestratum_nonce:max(ExtraNonceNBytes),
    case ((MaxConns + NAcceptors) * 2) < MaxExtraNonce of
        true  -> ok;
        false -> {error, insufficent_extra_nonce_nbytes}
    end.

check_beneficiaries_reward(#{reward_cfg := RewardCfg}) ->
    BeneficiariesReward = maps:get(beneficiaries_reward, RewardCfg),
    case BeneficiariesReward of
        N when N < 100.0 -> ok;
        _Other           -> {error, beneficiaries_reward_too_high}
    end.

read_key(Dir, File) ->
    file:read_file(filename:join([code:priv_dir(aestratum), Dir, File])).

binary_to_number(B) ->
    case catch binary_to_integer(B) of
        {'EXIT', {badarg, _}} -> binary_to_float(B);
        I -> I
    end.

