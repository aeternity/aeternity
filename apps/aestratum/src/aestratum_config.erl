-module(aestratum_config).

-export([setup_env/1,
         read_keys/1,
         read_keys/2]).

-import(aestratum_fn, [ok_val_err/2, is_ok/1, ok_err/2, val/3, key_val/2]).

-include("aestratum.hrl").
-include("aestratum_log.hrl").

-define(DEFAULT_HOST, <<"localhost">>).
-define(DEFAULT_PORT, 9999).
-define(DEFAULT_TRANSPORT, <<"tcp">>).
-define(DEFAULT_MAX_CONNECTIONS, 1024).
-define(DEFAULT_NUM_ACCEPTORS, 100).

-define(DEFAULT_EXTRA_NONCE_BYTES, 4).
-define(DEFAULT_SKIP_NUM_BLOCKS, 10).
-define(DEFAULT_INITIAL_SHARE_TARGET, aestratum_target:max()).
-define(DEFAULT_MAX_SHARE_TARGET, aestratum_target:max()).
-define(DEFAULT_DESIRED_SOLVE_TIME, 30000).
-define(DEFAULT_MAX_SOLVE_TIME, 60000).
-define(DEFAULT_SHARE_TARGET_DIFF_THRESHOLD, 5.0).
-define(DEFAULT_EDGE_BITS, 29).
-define(DEFAULT_MSG_TIMEOUT, 15000).
-define(DEFAULT_MAX_JOBS, 20).
-define(DEFAULT_MAX_WORKERS, 20).

-define(DEFAULT_REWARD_LAST_ROUNDS, 2).

-define(DEFAULT_REWARD_KEYBLOCK_DELAY, 180).
-define(DEFAULT_PAYOUT_KEYBLOCK_DELAY, 180).


setup_env(UserConfig) when is_list(UserConfig) ->
    case maps:from_list(UserConfig) of
        #{<<"enabled">> := true} = StratumCfg ->
            Put = fun (K, M) -> maps:put(K, section_map(K, StratumCfg), M) end,
            Cfg = lists:foldl(Put, #{}, section_keys()),
            case configure(Cfg#{enabled => true}) of
                {ok, ConfigMap} ->
                    aestratum_env:set(ConfigMap),
                    {ok, ConfigMap};
                {error, Error} ->
                    aestratum_env:reset(#{enabled => false}),
                    {error, Error}
            end;
        #{<<"enabled">> := false} ->
            {ok, aestratum_env:reset(#{enabled => false})}
    end.


section_keys() ->
    [connection, session, reward].

defaults_schema(connection) ->
    [{host, ?DEFAULT_HOST},
     {port, ?DEFAULT_PORT},
     {transport, ?DEFAULT_TRANSPORT},
     {max_connections, ?DEFAULT_MAX_CONNECTIONS},
     {num_acceptors, ?DEFAULT_NUM_ACCEPTORS}];
defaults_schema(session) ->
    [{extra_nonce_bytes, ?DEFAULT_EXTRA_NONCE_BYTES},
     {skip_num_blocks, ?DEFAULT_SKIP_NUM_BLOCKS},
     {initial_share_target, ?DEFAULT_INITIAL_SHARE_TARGET},
     {max_share_target, ?DEFAULT_MAX_SHARE_TARGET},
     {desired_solve_time, ?DEFAULT_DESIRED_SOLVE_TIME},
     {max_solve_time, ?DEFAULT_MAX_SOLVE_TIME},
     {share_target_diff_threshold, ?DEFAULT_SHARE_TARGET_DIFF_THRESHOLD},
     {edge_bits, ?DEFAULT_EDGE_BITS},
     {msg_timeout, ?DEFAULT_MSG_TIMEOUT},
     {max_jobs, ?DEFAULT_MAX_JOBS},
     {max_workers, ?DEFAULT_MAX_WORKERS}];  %% Per connection.
defaults_schema(reward) ->
    [beneficiaries,
     keys,
     {reward_last_rounds, ?DEFAULT_REWARD_LAST_ROUNDS}].


section_map(Section, Cfg) ->
    config_map(defaults_schema(Section), maps:from_list(val(Section, Cfg, []))).


configure(RawConfigMap) ->
    try lists:foldl(fun configure/2, RawConfigMap, section_keys()) of
        #{} = Result ->
            {ok, Result}
    catch
        _:{error, _} = ErrReason -> ErrReason
    end.

configure(_Section, {error, Reason}) ->
    {error, Reason};
configure(connection, #{connection := #{max_connections := MaxConnections,
                                        num_acceptors := NumAcceptors,
                                        transport := Transport} = ConnCfg,
                        session := #{extra_nonce_bytes := ExtraNonceBytes}} = Result) ->
    check_extra_nonce_bytes(MaxConnections, NumAcceptors, ExtraNonceBytes)
        orelse error(insufficent_extra_nonce_bytes),
    maps:merge(maps:without([connection], Result),
               maps:put(transport, binary_to_atom(Transport, utf8), ConnCfg));
configure(session, #{session := #{desired_solve_time := DesiredSolveTime,
                                  max_solve_time := MaxSolveTime} = SessionCfg} = Result) ->
    SessionCfg1 = SessionCfg#{desired_solve_time => DesiredSolveTime * 1000,
                              max_solve_time => MaxSolveTime * 1000},
    maps:merge(maps:without([session], Result), SessionCfg1);
configure(reward, #{reward := #{beneficiaries := PoolShareBins,
                                reward_last_rounds := LastN,
                                keys := [{<<"dir">>, KeysDir}]}} = Result) ->
    (is_integer(LastN) andalso LastN > 0 andalso LastN < 10)
        orelse error({invalid, reward_last_rounds}),

    ContractAddr = case aec_governance:get_network_id() of
                       <<"ae_mainnet">> -> ?PAYMENT_CONTRACT_MAINNET_ADDRESS;
                       _Other -> ?PAYMENT_CONTRACT_TESTNET_ADDRESS
                   end,
    ContractPK   = aestratum_conv:contract_address_to_pubkey(ContractAddr),

    {CallerPK, CallerSK} = CallerKeyPair = read_keys(KeysDir, await),
    CallerAddr   = aestratum_conv:account_pubkey_to_address(CallerPK),
    check_keypair_roundtrips(CallerKeyPair) orelse error(invalid_keypair),

    PoolPercentShares =
        lists:foldl(fun (Bnf, Acc) ->
                            [<<"ak_", _/binary>> = AccountAddr, PctShareBin] =
                                binary:split(Bnf, <<":">>),
                            PctShare = aestratum_conv:binary_to_number(PctShareBin),
                            Acc#{AccountAddr => maps:get(AccountAddr, Acc, 0) + PctShare}
                    end, #{}, PoolShareBins),
    PoolPercentSum = lists:sum(maps:values(PoolPercentShares)),
    PoolPercentSum =< 100.0 orelse error(beneficiaries_reward_too_high),

    maps:merge(maps:without([reward], Result),
               #{last_n                => LastN,
                 contract_pubkey       => ContractPK,
                 contract_address      => ContractAddr,
                 caller_pubkey         => CallerPK,
                 caller_privkey        => CallerSK,
                 caller_address        => CallerAddr,
                 pool_percent_sum      => PoolPercentSum,
                 pool_percent_shares   => PoolPercentShares,
                 reward_keyblock_delay => ?DEFAULT_REWARD_KEYBLOCK_DELAY,
                 payout_keyblock_delay => ?DEFAULT_PAYOUT_KEYBLOCK_DELAY}).


check_extra_nonce_bytes(MaxConnections, NumAcceptors, ExtraNonceBytes) ->
    ((MaxConnections + NumAcceptors) * 2) < aestratum_nonce:max(ExtraNonceBytes).

check_keypair_roundtrips({PK, SK}) ->
    Sig = enacl:sign_detached(<<"roundtrip">>, SK),
    is_ok(enacl:sign_verify_detached(Sig, <<"roundtrip">>, PK)).

read_keys(Dir) ->
    AbsDir = case filename:pathtype(Dir) of
                 relative -> filename:join(code:priv_dir(aestratum), Dir);
                 absolute -> Dir
             end,
    PKPath = filename:join(AbsDir, <<"sign_key.pub">>),
    SKPath = filename:join(AbsDir, <<"sign_key">>),
    {ok_val_err(file:read_file(PKPath), {no_keys, AbsDir}),
     ok_val_err(file:read_file(SKPath), {no_keys, AbsDir})}.

read_keys(Dir, create) ->
    try read_keys(Dir) of
        {PK, SK} -> {PK, SK}
    catch
        _:{{no_keys, AbsDir}, _} ->
            #{public := PK, secret := SK} = enacl:sign_keypair(),
            ok_err(filelib:ensure_dir(filename:join(AbsDir, "_")),
                   {cound_not_create_dir_for_stratum_keys, AbsDir}),
            ok = file:write_file(filename:join(AbsDir, <<"sign_key.pub">>), PK),
            ok = file:write_file(filename:join(AbsDir, <<"sign_key">>), SK),
            {PK, SK}
    end;
read_keys(Dir, await) ->
    try read_keys(Dir) of
        {PK, SK} -> {PK, SK}
    catch
        _:{{no_keys, _AbsDir}, _} ->
            timer:sleep(1000),
            read_keys(Dir, await)
    end.

%%%%%%%%%%

put_from_default(KSD, M, Acc) ->
    {K, Val} = key_val(KSD, M),
    maps:put(K, Val, Acc).

config_map(KeysSomeVals, M) ->
    lists:foldl(resolver(M), #{}, KeysSomeVals).

resolver(#{} = M) ->
    fun (KSD, Acc) -> put_from_default(KSD, M, Acc) end.
