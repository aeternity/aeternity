-module(aestratum_config).

-export([read/0]).

-include("aestratum.hrl").
-include("aestratum_log.hrl").

-export_type([config/0]).

-type config() :: map().

-spec read() -> {ok, config()} | {error, term()}.
read() ->
    {ok, Cfg} = aeu_env:user_config(<<"stratum">>),
    case maps:from_list(Cfg) of
        #{<<"enabled">> := true} = Cfg1 ->
            ToMap = fun (Section) -> maps:from_list(maps:get(Section, Cfg1, [])) end,
            Cfg2 = #{enabled     => true,
                     conn_cfg    => conn_config(ToMap(<<"connection">>)),
                     session_cfg => session_config(ToMap(<<"session">>)),
                     reward_cfg  => reward_config(ToMap(<<"reward">>))},
            case validate_config(Cfg2) of
                {ok, C} = Res ->
                    setup_env(C),
                    Res;
                {error, _Rsn} = Res ->
                    Res
            end;
        #{<<"enabled">> := false} ->
            {ok, #{enabled => false}}
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
    ContractAddress = case aec_governance:get_network_id() of
                          <<"ae_uat">> -> ?PAYMENT_CONTRACT_TESTNET_ADDRESS;
                          <<"ae_mainnet">> -> ?PAYMENT_CONTRACT_MAINNET_ADDRESS
                      end,
    Beneficiaries =
        lists:foldl(fun (Bnf, Acc) ->
                            [<<"ak_", _/binary>> = AccountAddr, PctShareBin] =
                                binary:split(Bnf, <<":">>),
                            PctShare = binary_to_number(PctShareBin),
                            Acc#{AccountAddr => maps:get(AccountAddr, Acc, 0) + PctShare}
                    end, #{}, BeneficiariesAddrPctShares),
    #{reward_last_nrounds  => RewardLastNRounds,
      key_pair             => {PubKey, PrivKey},
      contract_address     => ContractAddress,
      beneficiaries        => Beneficiaries,
      beneficiaries_reward => lists:sum(maps:values(Beneficiaries))}.

validate_config(Cfg) ->
    Checks = [fun check_extra_nonce_nbytes/1,
              fun check_beneficiaries_reward/1,
              fun check_keypair/1,
              fun check_account/1,
              fun check_contract/1],
    case run(Checks, Cfg) of
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

check_keypair(#{reward_cfg := #{key_pair := KeyPair}}) ->
    {<<PK:32/binary>>, <<SK:64/binary>>} = KeyPair,
    Bin = crypto:strong_rand_bytes(100),
    Sig = enacl:sign_detached(Bin, SK),
    case enacl:sign_verify_detached(Sig, Bin, PK) of
        {ok, _} ->
            ok;
        {error, _} ->
            {error, invalid_stratum_keys}
    end.

check_account(#{reward_cfg := #{key_pair := {PK, _}}}) ->
    case aec_chain:get_account(PK) of
        {value, _} ->
            ok;
        none ->
            Address = aehttp_api_encoder:encode(account_pubkey, PK),
            {error, {stratum_account_not_found, Address}}
    end.

check_contract(#{reward_cfg := #{contract_address := ContractAddress}}) ->
    case aehttp_api_encoder:decode(ContractAddress) of
        {contract_pubkey, _} ->
            ok;
        _ ->
            {error, {stratum_contract_invalid, ContractAddress}}
    end.


read_key(Dir, File) ->
    file:read_file(filename:join([code:priv_dir(aestratum), Dir, File])).

binary_to_number(B) ->
    case catch binary_to_integer(B) of
        {'EXIT', {badarg, _}} -> binary_to_float(B);
        I -> I
    end.
