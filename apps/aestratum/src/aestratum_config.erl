-module(aestratum_config).

-export([setup_env/0]).

-import(aestratum_fn, [ok_val_err/2, is_ok/1, val/3, key_val/2]).

-include("aestratum.hrl").
-include("aestratum_log.hrl").

setup_env() ->
    {ok, StratumCfg0} = aeu_env:user_config(<<"stratum">>),
    case maps:from_list(StratumCfg0) of
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
    [{host, <<"pool.aeternity.com">>},
     {port, 9999},
     {transport, <<"tcp">>},
     {max_connections, 1024},
     {num_acceptors, 100}];
defaults_schema(session) ->
    [{extra_nonce_bytes, 4},
     {initial_share_target, 1},
     {max_share_target, aestratum_target:max()},
     {desired_solve_time, 301000},
     {max_solve_time, 601000},
     {share_target_diff_threshold, 5.0},
     {msg_timeout, 15100},
     {max_jobs, 20}];
defaults_schema(reward) ->
    [beneficiaries,
     keys,
     {reward_last_rounds, 2}].


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
        orelse error(insufficent_extra_nonce_nbytes),
    maps:merge(maps:without([connection], Result),
               maps:put(transport, binary_to_atom(Transport, utf8), ConnCfg));
configure(session, #{session := SessionCfg} = Result) ->
    maps:merge(maps:without([session], Result), SessionCfg);
configure(reward, #{reward := #{beneficiaries := PoolShareBins,
                                reward_last_rounds := LastN,
                                keys := [{<<"dir">>, KeysDir}]}} = Result) ->
    (is_integer(LastN) andalso LastN > 0 andalso LastN < 10)
        orelse error({invalid, reward_last_rounds}),

    ContractAddr = case aec_governance:get_network_id() of
                       <<"ae_uat">> -> ?PAYMENT_CONTRACT_TESTNET_ADDRESS;
                       <<"ae_mainnet">> -> ?PAYMENT_CONTRACT_MAINNET_ADDRESS
                   end,
    ContractPK   = aestratum_util:contract_address_to_pubkey(ContractAddr),
    ContractPath = filename:join(code:priv_dir(aestratum), "Payout.aes"),
    Contract     = ok_val_err(aeso_compiler:file(ContractPath), contract_compilation),

    {CallerPK, CallerSK} = CallerKeyPair = read_keys(KeysDir),
    CallerAddr   = aestratum_util:account_pubkey_to_address(CallerPK),
    check_keypair_roundtrips(CallerKeyPair) orelse error(invalid_keypair),

    PoolPercentShares =
        lists:foldl(fun (Bnf, Acc) ->
                            [<<"ak_", _/binary>> = AccountAddr, PctShareBin] =
                                binary:split(Bnf, <<":">>),
                            PctShare = aestratum_util:binary_to_number(PctShareBin),
                            Acc#{AccountAddr => maps:get(AccountAddr, Acc, 0) + PctShare}
                    end, #{}, PoolShareBins),
    PoolPercentSum = lists:sum(maps:values(PoolPercentShares)),
    PoolPercentSum < 100.0 orelse error(beneficiaries_reward_too_high),

    maps:merge(maps:without([reward], Result),
               #{last_n               => LastN,
                 contract             => Contract,
                 contract_pubkey      => ContractPK,
                 contract_address     => ContractAddr,
                 caller_pubkey        => CallerPK,
                 caller_privkey       => CallerSK,
                 caller_address       => CallerAddr,
                 pool_percent_sum     => PoolPercentSum,
                 pool_percent_shares  => PoolPercentShares}).


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
    {ok_val_err(file:read_file(PKPath), <<"no public key at ", PKPath/binary>>),
     ok_val_err(file:read_file(SKPath), <<"no privite key at ", SKPath/binary>>)}.

%%%%%%%%%%

put_from_default(KSD, M, Acc) ->
    {K, Val} = key_val(KSD, M),
    maps:put(K, Val, Acc).

config_map(KeysSomeVals, M) ->
    lists:foldl(resolver(M), #{}, KeysSomeVals).

resolver(#{} = M) ->
    fun (KSD, Acc) -> put_from_default(KSD, M, Acc) end.
