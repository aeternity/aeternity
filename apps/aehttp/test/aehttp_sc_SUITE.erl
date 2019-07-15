-module(aehttp_sc_SUITE).

-import(aecore_suite_utils, [http_request/4, internal_address/0, external_address/0,
                             rpc/3, rpc/4]).

-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

-export(
   [sc_ws_timeout_open/1,
    sc_ws_attach_initiator/1,
    sc_ws_attach_responder/1,
    sc_ws_min_depth_not_reached_timeout/1,
    sc_ws_min_depth_is_modifiable/1,
    sc_ws_basic_open_close/1,
    sc_ws_basic_open_close_server/1,
    sc_ws_failed_update/1,
    sc_ws_generic_messages/1,
    sc_ws_update_conflict/1,
    sc_ws_close_mutual/1,
    sc_ws_close_solo/1,
    sc_ws_slash/1,
    sc_ws_leave_reestablish/1,
    sc_ws_ping_pong/1,
    sc_ws_deposit/1,
    sc_ws_withdraw/1,
    sc_ws_contracts/1,
    sc_ws_oracle_contract/1,
    sc_ws_nameservice_contract/1,
    sc_ws_environment_contract/1,
    sc_ws_remote_call_contract/1,
    sc_ws_remote_call_contract_refering_onchain_data/1,
    sc_ws_wrong_call_data/1
   ]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").

-define(NODE, dev1).
-define(DEFAULT_TESTS_COUNT, 5).
-define(WS, aehttp_ws_test_utils).
-define(DEFAULT_MIN_DEPTH, 4).
-define(MAX_MINED_BLOCKS, 20).
-define(BOGUS_STATE_HASH, <<42:32/unit:8>>).
-define(SPEND_FEE, 20000 * aec_test_utils:min_gas_price()).

-define(SLOGAN, slogan(?FUNCTION_NAME, ?LINE)).
-define(SLOGAN(Param), slogan(?FUNCTION_NAME, ?LINE, Param)).

-define(ROLES, [initiator, responder]).

-define(SIMPLE_AUTH_GA_SECRET, "42").

-define(ALICE, {
    <<177,181,119,188,211,39,203,57,229,94,108,2,107,214, 167,74,27,
      53,222,108,6,80,196,174,81,239,171,117,158,65,91,102>>,
    <<145,69,14,254,5,22,194,68,118,57,0,134,66,96,8,20,124,253,238,
      207,230,147,95,173,161,192,86,195,165,186,115,251,177,181,119,
      188,211,39,203,57,229,94,108,2,107,214,167,74,27,53,222,108,6,
      80,196,174,81,239,171,117,158,65,91,102>>}).

-define(BOB, {
    <<103,28,85,70,70,73,69,117,178,180,148,246,81,104,
      33,113,6,99,216,72,147,205,210,210,54,3,122,84,195,
      62,238,132>>,
    <<59,130,10,50,47,94,36,188,50,163,253,39,81,120,89,219,72,88,68,
      154,183,225,78,92,9,216,215,59,108,82,203,25,103,28,85,70,70,
      73,69,117,178,180,148,246,81,104,33,113,6,99,216,72,147,205,
      210,210,54,3,122,84,195,62,238,132>>}).

-define(CAROL, {
    <<200,171,93,11,3,93,177,65,197,27,123,127,177,165,
      190,211,20,112,79,108,85,78,88,181,26,207,191,211,
      40,225,138,154>>,
    <<237,12,20,128,115,166,32,106,220,142,111,97,141,104,201,130,56,
      100,64,142,139,163,87,166,185,94,4,159,217,243,160,169,200,171,
      93,11,3,93,177,65,197,27,123,127,177,165,190,211,20,112,79,108,
      85,78,88,181,26,207,191,211,40,225,138,154>>}).

all() -> [{group, plain}, {group, aevm}, {group, fate}].

groups() ->
    [{plain, [],
      [ sc_ws_timeout_open,
        sc_ws_min_depth_not_reached_timeout,
        sc_ws_min_depth_is_modifiable,
        sc_ws_basic_open_close,
        sc_ws_basic_open_close_server,
        %% both can start close mutual
        sc_ws_close_mutual,
        %% both can solo-close
        sc_ws_close_solo,
        %% fsm informs of slash potential
        sc_ws_slash,
        %% possible to leave and reestablish channel
        sc_ws_leave_reestablish,
        {group, with_open_channel}
      ]},

     {with_open_channel, [sequence],
      [ sc_ws_ping_pong,
        %% both can deposit
        sc_ws_deposit,
        %% both can withdraw
        sc_ws_withdraw,
        %% ensure port is reusable
        sc_ws_failed_update,
        sc_ws_generic_messages,
        sc_ws_update_conflict
      ]},

     {aevm, [], [{group, sc_contracts}, {group, sc_ga}]},
     {fate, [], [{group, sc_contracts}, {group, sc_ga}]},

     {sc_contracts, [sequence],
      [ %% both can refer on-chain objects - oracle
        sc_ws_oracle_contract,
        %% both can refer on-chain objects - name service
        sc_ws_nameservice_contract,
        %% both can refer on-chain objects - chain environment
        sc_ws_environment_contract,
        %% both can call a remote contract
        sc_ws_remote_call_contract,
        %% both can call a remote contract refering on-chain data
        sc_ws_remote_call_contract_refering_onchain_data,
        sc_ws_wrong_call_data
      ]},

     {sc_ga, [sequence],
      [ {group, ga_initiator},
        {group, ga_responder},
        {group, ga_both}
      ]},

     {ga_initiator, [sequence],
      [ sc_ws_attach_initiator,
        sc_ws_basic_open_close
      ]},

     {ga_responder, [sequence],
      [ sc_ws_attach_responder,
        sc_ws_basic_open_close
      ]},

     {ga_both, [sequence],
      [ sc_ws_attach_initiator,
        sc_ws_attach_responder,
        sc_ws_basic_open_close
      ]}

    ].

suite() -> [].

init_per_suite(Config) ->
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => true,
                     <<"hard_forks">> => Forks},
               <<"mining">> =>
                   #{<<"micro_block_cycle">> => 1}},
    {ok, StartedApps} = application:ensure_all_started(gproc),
    Config1 = aecore_suite_utils:init_per_suite([?NODE], DefCfg, [{symlink_name, "latest.http_sc"}, {test_module, ?MODULE}] ++ Config),
    start_node([ {nodes, [aecore_suite_utils:node_tuple(?NODE)]}
               , {started_apps, StartedApps}
               , {ws_port, 12340}] ++ Config1).

end_per_suite(Config) ->
    [application:stop(A) ||
        A <- lists:reverse(
               proplists:get_value(started_apps, Config, []))],
    stop_node(Config).

init_per_group(VM, Config) when VM == aevm; VM == fate ->
    aect_test_utils:init_per_group(VM, inc_group_port(VM, Config));
init_per_group(sc_ga, Config) ->
    case aect_test_utils:latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> {skip, generalized_accounts_not_in_roma};
        ?MINERVA_PROTOCOL_VSN -> {skip, generalized_accounts_not_in_minerva};
        Vsn when Vsn >= ?FORTUNA_PROTOCOL_VSN -> Config
    end;
init_per_group(with_open_channel, Config) ->
    sc_ws_open_(Config);
init_per_group(Grp, Config) when Grp == ga_both; Grp == ga_initiator; Grp == ga_responder ->
    reset_participants(Grp, Config);
init_per_group(sc_contracts, Config) ->
    sc_ws_open_(reset_participants(sc_contracts, Config));
init_per_group(plain, Config) ->
    reset_participants(plain, Config);
init_per_group(_Grp, Config) ->
    Config.

end_per_group(sc_contracts, Config) ->
    sc_ws_close_(Config);
end_per_group(with_open_channel, Config) ->
    sc_ws_close_(Config);
end_per_group(_Grp, _Config) ->
    ok.

-define(DOC_LOG, [ sc_ws_ping_pong, sc_ws_deposit, sc_ws_withdraw,
                   sc_ws_failed_update, sc_ws_generic_messages, sc_ws_update_conflict,
                   sc_ws_contracts, sc_ws_oracle_contract, sc_ws_nameservice_contract,
                   sc_ws_environment_contract, sc_ws_remote_call_contract,
                   sc_ws_remote_call_contract_refering_onchain_data, sc_ws_wrong_call_data ]).

init_per_testcase(Case, Config) ->
    case proplists:is_defined(sophia_version, Config) of
        true  -> aect_test_utils:setup_testcase(Config);
        false -> ok
    end,
    case lists:member(Case, ?DOC_LOG) of
        true ->
            LogFile = docs_log_file(Config),
            #{initiator := IConnPid,
              responder := RConnPid} = proplists:get_value(channel_clients, Config),
            ?WS:set_log_file(IConnPid, LogFile),
            ?WS:set_log_file(RConnPid, LogFile);
        false ->
            ok
    end,
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, 100),
    [{tc_start, os:timestamp()} | Config].

end_per_testcase(_Case, Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:unmock_mempool_nonce_offset(Node),
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p",
           [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
             || {_,N} <- ?config(nodes, Config)]]),
    ok.

start_node(Config) ->
    aecore_suite_utils:start_node(?NODE, Config),
    Node = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:connect(Node),

    {ok, 404, _} = get_balance_at_top(),
    aecore_suite_utils:mine_key_blocks(Node, 10),

    Config1 = lists:keystore(sc_ws_protocol, 1, Config, {sc_ws_protocol, <<"json-rpc">>}),

    [{node, Node} | Config1].

reset_participants(Grp, Config) ->
    Node = ?config(node, Config),

    StartAmt = 50000000000 * aec_test_utils:min_gas_price(),

    Initiator = {IPub, IPriv} = aecore_suite_utils:generate_key_pair(),
    Responder = {RPub, RPriv} = aecore_suite_utils:generate_key_pair(),

    ITx = initialize_account(StartAmt, Initiator, false),
    RTx = initialize_account(StartAmt, Responder, false),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [ITx, RTx], ?MAX_MINED_BLOCKS),

    Participants = #{initiator => #{pub_key => IPub,
                                    priv_key => IPriv,
                                    start_amt => StartAmt},
                     responder => #{pub_key => RPub,
                                    priv_key => RPriv,
                                    start_amt => StartAmt}},
    %% New participants need to bump the port to not risk colliding.
    Config1 = [{participants, Participants} | proplists:delete(participants, Config)],
    inc_group_port(Grp, Config1).

inc_group_port(Grp, Config) ->
    %% Note that we are dealing with a group hierarchy that will add different
    %% offsets for each level.
    Offset = erlang:phash2(Grp, 999) + 1,
    [{ws_port, ?config(ws_port, Config) + Offset}|Config].


stop_node(Config) ->
    RpcFun = fun(M, F, A) -> rpc(?NODE, M, F, A) end,
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(RpcFun),
    aecore_suite_utils:stop_node(?NODE, Config),
    aecore_suite_utils:delete_node_db_if_persisted(DbCfg).

%% ============================================================
%% Test cases
%% ============================================================
get_key_blocks_current_sut() ->
    Host = external_address(),
    http_request(Host, get, "key-blocks/current", []).

get_accounts_by_pubkey_sut(Id) ->
    Host = external_address(),
    http_request(Host, get, "accounts/" ++ http_uri:encode(Id), []).

get_transactions_by_hash_sut(Hash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/" ++ http_uri:encode(Hash), []).

post_transactions_sut(Tx) ->
    Host = external_address(),
    http_request(Host, post, "transactions", #{tx => Tx}).

get_names_entry_by_name_sut(Name) ->
    Host = external_address(),
    http_request(Host, get, "names/" ++ Name, []).

get_oracles_by_pubkey_sut(Pubkey) ->
    Host = external_address(),
    http_request(Host, get, "oracles/" ++ http_uri:encode(Pubkey), []).

get_contract_sut(PubKey) ->
    Host = external_address(),
    http_request(Host, get, "contracts/" ++ binary_to_list(PubKey), []).

get_balance_at_top() ->
    EncodedPubKey = get_pubkey(),
    get_accounts_by_pubkey_sut(EncodedPubKey).

post_tx(TxHash, Tx) ->
    {ok, 200, Resp} = post_transactions_sut(Tx),
    ?assertEqual(TxHash, maps:get(<<"tx_hash">>, Resp)),
    Fun = fun() -> tx_in_mempool(TxHash) end,
    {ok, true} = aec_test_utils:wait_for_it_or_timeout(Fun, true, 5000),
    ok.

%% ============================================================
%% Websocket tests
%% ============================================================

%%
%% Channels
%%
assert_balance(Pubkey, ExpectedBalance) ->
    assert_balance(Pubkey, ExpectedBalance, equal).

assert_balance_at_most(Pubkey, ExpectedBalance) ->
    assert_balance(Pubkey, ExpectedBalance, equal_or_less).

assert_balance_at_least(Pubkey, MaxExpectedBalance) ->
    assert_balance(Pubkey, MaxExpectedBalance, equal_or_greater).

assert_balance(Pubkey, ExpectedBalance, Action) ->
    Address = aeser_api_encoder:encode(account_pubkey, Pubkey),
    {ok, 200, #{<<"balance">> := ActualBalance}} =
        get_accounts_by_pubkey_sut(Address),
    Res =
        case Action of
            equal -> ExpectedBalance =:= ActualBalance;
            equal_or_greater -> ActualBalance >= ExpectedBalance;
            equal_or_less -> ActualBalance =< ExpectedBalance
        end,
    {true, _, _, _} = {Res, ActualBalance, Action, ExpectedBalance}.

assert_trees_balance(Trees, Pubkey, ExpectedBalance) ->
    AccTrees = aec_trees:accounts(Trees),
    {value, Account} = aec_accounts_trees:lookup(Pubkey, AccTrees),
    ExpectedBalance = aec_accounts:balance(Account).

channel_sign_tx(Pubkey, ConnPid, Privkey, Method, Config) ->
    {ok, Tag, #{<<"signed_tx">> := EncSignedTx0,
                <<"updates">>   := Updates}} = wait_for_channel_event(ConnPid, sign, Config),
    Method = <<"channels.", (bin(Tag))/binary>>,
    {ok, SignedTxBin} = aeser_api_encoder:safe_decode(transaction, EncSignedTx0),
    SignedTx0 = aetx_sign:deserialize_from_binary(SignedTxBin),
    Tx = aetx_sign:innermost_tx(SignedTx0),
    SignedTx =
        case account_type(Pubkey) of
            basic -> aec_test_utils:co_sign_tx(SignedTx0, Privkey);
            generalized ->
                MetaTx = simple_auth_meta(Pubkey, ?SIMPLE_AUTH_GA_SECRET,
                                          "1", % make this incremental
                                          Tx,
                                          SignedTx0),
                MetaTx
        end,
    EncSignedTx =
        aeser_api_encoder:encode(transaction,
                                 aetx_sign:serialize_to_binary(SignedTx)),
    ws_call_async_method(ConnPid, Method, #{signed_tx => EncSignedTx}, Config),
    #{tx => Tx, signed_tx => SignedTx, updates => Updates}.

sc_ws_open_(Config) ->
    sc_ws_open_(Config, #{}).

sc_ws_open_(Config, Opts) ->
    sc_ws_open_(Config, Opts, ?DEFAULT_MIN_DEPTH).

sc_ws_open_(Config, ChannelOpts0, MinBlocksToMine) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),

    {ok, 200, #{<<"balance">> := IStartAmt}} =
                 get_accounts_by_pubkey_sut(aeser_api_encoder:encode(account_pubkey, IPubkey)),
    {ok, 200, #{<<"balance">> := RStartAmt}} =
                 get_accounts_by_pubkey_sut(aeser_api_encoder:encode(account_pubkey, RPubkey)),
    IAmt = 70000 * aec_test_utils:min_gas_price(),
    RAmt = 40000 * aec_test_utils:min_gas_price(),

    {IStartAmt, RStartAmt} = channel_participants_balances(IPubkey, RPubkey),

    ChannelOpts = channel_options(IPubkey, RPubkey, IAmt, RAmt, ChannelOpts0, Config),
    {ok, IConnPid} = channel_ws_start(initiator,
                                           maps:put(host, <<"localhost">>, ChannelOpts), Config),
    ok = ?WS:register_test_for_channel_events(IConnPid, [info, get, sign, on_chain_tx]),

    {ok, RConnPid} = channel_ws_start(responder, ChannelOpts, Config),

    ok = ?WS:register_test_for_channel_events(RConnPid, [info, get, sign, on_chain_tx]),

    channel_send_conn_open_infos(RConnPid, IConnPid, Config),

    ChannelCreateFee = channel_create(Config, IConnPid, RConnPid),
    {ok, {IBal, RBal}} = sc_ws_get_both_balances(IConnPid,
                                                 IPubkey,
                                                 RPubkey,
                                                 Config),
    %% assert off-chain balances
    PushAmt = maps:get(push_amount, ChannelOpts),
    IBal = IAmt - PushAmt,
    RBal = RAmt + PushAmt,

    %% ensure new balances
    assert_balance_at_most(IPubkey, IStartAmt - IAmt - ChannelCreateFee),
    assert_balance_at_most(RPubkey, RStartAmt - RAmt),

    % mine min depth
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                       MinBlocksToMine),

    channel_send_locking_infos(IConnPid, RConnPid, Config),

    channel_send_chan_open_infos(RConnPid, IConnPid, Config),

    ChannelClients = #{initiator => IConnPid,
                       responder => RConnPid},
    ok = ?WS:unregister_test_for_channel_events(IConnPid, [info, get, sign, on_chain_tx]),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [info, get, sign, on_chain_tx]),
    [{channel_clients, ChannelClients},
     {channel_options, ChannelOpts} | Config].


channel_send_conn_open_infos(RConnPid, IConnPid, Config) ->
    {ok, #{<<"event">> := <<"channel_open">>}} = wait_for_channel_event(RConnPid, info, Config),
    {ok, #{<<"event">> := <<"channel_accept">>}} = wait_for_channel_event(IConnPid, info, Config).

channel_send_locking_infos(IConnPid, RConnPid, Config) ->
    {ok, #{<<"event">> := <<"own_funding_locked">>}} = wait_for_channel_event(IConnPid, info, Config),
    {ok, #{<<"event">> := <<"own_funding_locked">>}} = wait_for_channel_event(RConnPid, info, Config),

    {ok, #{<<"event">> := <<"funding_locked">>}} = wait_for_channel_event(IConnPid, info, Config),
    {ok, #{<<"event">> := <<"funding_locked">>}} = wait_for_channel_event(RConnPid, info, Config),
    ok.

channel_send_chan_open_infos(RConnPid, IConnPid, Config) ->
    {ok, #{<<"event">> := <<"open">>}} = wait_for_channel_event(IConnPid, info, Config),
    {ok, #{<<"event">> := <<"open">>}} = wait_for_channel_event(RConnPid, info, Config),
    ok.

channel_participants_balances(IPubkey, RPubkey) ->
    {ok, 200, #{<<"balance">> := BalI}} =
        get_accounts_by_pubkey_sut(aeser_api_encoder:encode(account_pubkey, IPubkey)),
    {ok, 200, #{<<"balance">> := BalR}} =
        get_accounts_by_pubkey_sut(aeser_api_encoder:encode(account_pubkey, RPubkey)),
    {BalI, BalR}.

channel_create(Config, IConnPid, RConnPid) ->
    #{initiator := #{pub_key := IPubkey,
                    priv_key := IPrivkey},
      responder := #{pub_key := RPubkey,
                    priv_key := RPrivkey}} = proplists:get_value(participants, Config),
    %% initiator gets to sign a create_tx
    {IStartAmt, RStartAmt} = channel_participants_balances(IPubkey, RPubkey),

    #{tx := CrTx,
      updates := Updates} = channel_sign_tx(IPubkey, IConnPid, IPrivkey, <<"channels.initiator_sign">>, Config),
    {ok, #{<<"event">> := <<"funding_created">>}} = wait_for_channel_event(RConnPid, info, Config),
    %% responder gets to sign a create_tx
    #{tx := CrTx,
      updates := Updates} = channel_sign_tx(RPubkey, RConnPid, RPrivkey, <<"channels.responder_sign">>, Config),
    {ok, #{<<"event">> := <<"funding_signed">>}} = wait_for_channel_event(IConnPid, info, Config),

    %% both of them receive the same co-signed channel_create_tx
    {ok, #{<<"tx">> := EncodedSignedCrTx}} = wait_for_channel_event(IConnPid, on_chain_tx, Config),
    {ok, #{<<"tx">> := EncodedSignedCrTx}} = wait_for_channel_event(RConnPid, on_chain_tx, Config),

    {ok, SSignedCrTx} = aeser_api_encoder:safe_decode(transaction, EncodedSignedCrTx),
    SignedCrTx = aetx_sign:deserialize_from_binary(SSignedCrTx),
    %% same transaction
    CrTx = aetx_sign:innermost_tx(SignedCrTx),

    {channel_create_tx, Tx} = aetx:specialize_type(CrTx),
    IPubkey = aesc_create_tx:initiator_pubkey(Tx),
    RPubkey = aesc_create_tx:responder_pubkey(Tx),
    ChannelCreateFee = aesc_create_tx:fee(Tx),

    %% ensure the tx is in the mempool
    ok = wait_for_signed_transaction_in_pool(SignedCrTx),

    %% balances hadn't changed yet
    assert_balance(IPubkey, IStartAmt),
    assert_balance(RPubkey, RStartAmt),

    % mine the create_tx
    ok = wait_for_signed_transaction_in_block(SignedCrTx),

    {ok, #{<<"tx">> := EncodedSignedCrTx}} = wait_for_channel_event(IConnPid, on_chain_tx, Config),
    {ok, #{<<"tx">> := EncodedSignedCrTx}} = wait_for_channel_event(RConnPid, on_chain_tx, Config),

    ChannelCreateFee.

sc_ws_update_(Config) ->
    Participants = proplists:get_value(participants, Config),
    Conns = proplists:get_value(channel_clients, Config),
    lists:foldl(
        fun(Sender, Round) ->
            channel_update(Conns, Sender, Participants, 1, Round, Config),
            Round + 1
        end,
        2, % we start from round 2
        [initiator,
         responder,
         responder,
         initiator,
         responder]),
    ok.

channel_conflict(#{initiator := IConnPid, responder :=RConnPid},
               StarterRole,
               #{initiator := #{pub_key := IPubkey,
                                priv_key := IPrivkey},
                 responder := #{pub_key := RPubkey,
                                priv_key := RPrivkey}},
               Amount1, Amount2, Config) ->
    {StarterPid, AcknowledgerPid, StarterPubkey, StarterPrivkey,
     AcknowledgerPubkey, AcknowledgerPrivkey} =
        case StarterRole of
            initiator ->
                {IConnPid, RConnPid, IPubkey, IPrivkey, RPubkey, RPrivkey};
            responder ->
                {RConnPid, IConnPid, RPubkey, RPrivkey, IPubkey, IPrivkey}
        end,
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, conflict]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, conflict]),

    SignUpdate =
        fun TrySignUpdate(ConnPid, Privkey) ->
            case wait_for_channel_event(ConnPid, sign, Config) of
                {ok, <<"update_ack">>, _} -> %% this is not the message we are looking for
                    TrySignUpdate(ConnPid, Privkey);
                {ok, <<"update">>, #{<<"signed_tx">> := EncSignedCreateTx0}} ->
                    {ok, SignedCreateBinTx} =
                        aeser_api_encoder:safe_decode(transaction, EncSignedCreateTx0),
                    STx = aetx_sign:deserialize_from_binary(SignedCreateBinTx),
                    SignedCreateTx = aec_test_utils:co_sign_tx(STx, Privkey),
                    EncSignedCreateTx = aeser_api_encoder:encode(transaction,
                                                  aetx_sign:serialize_to_binary(SignedCreateTx)),
                    ws_send_tagged(ConnPid, <<"channels.update">>,
                                   #{signed_tx => EncSignedCreateTx}, Config)
            end
        end,
    %% sender initiates an update
    ws_send_tagged(StarterPid, <<"channels.update.new">>,
                   #{from => aeser_api_encoder:encode(account_pubkey, StarterPubkey),
                     to => aeser_api_encoder:encode(account_pubkey, AcknowledgerPubkey),
                     amount => Amount1}, Config),

    %% starter signs the new state

    %% acknowledger initiates an update too
    ws_send_tagged(AcknowledgerPid, <<"channels.update.new">>,
                   #{from => aeser_api_encoder:encode(account_pubkey, StarterPubkey),
                     to => aeser_api_encoder:encode(account_pubkey, AcknowledgerPubkey),
                     amount => Amount2}, Config),

    SignUpdate(StarterPid, StarterPrivkey),
    SignUpdate(AcknowledgerPid, AcknowledgerPrivkey),

    {ok, _} = wait_for_channel_event(StarterPid, conflict, Config),
    {ok, _} = wait_for_channel_event(AcknowledgerPid, conflict, Config),

    ok = ?WS:unregister_test_for_channel_events(IConnPid, [sign, conflict]),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [sign, conflict]),

    ok.

channel_update(Conns, Starter, Participants, Amount, Round, Config) ->
    channel_update(Conns, Starter, Participants, Amount, Round,
                   _TestErrors = true, Config).

channel_update(#{initiator := IConnPid, responder :=RConnPid},
               StarterRole,
               #{initiator := #{pub_key := IPubkey,
                                priv_key := IPrivkey},
                 responder := #{pub_key := RPubkey,
                                priv_key := RPrivkey}},
               Amount,_Round,TestErrors,Config) ->
    true = undefined =/= process_info(IConnPid),
    true = undefined =/= process_info(RConnPid),
    {StarterPid, AcknowledgerPid, StarterPubkey, StarterPrivkey,
     AcknowledgerPubkey, AcknowledgerPrivkey} =
        case StarterRole of
            initiator ->
                {IConnPid, RConnPid, IPubkey, IPrivkey,
                                    RPubkey, RPrivkey};
            responder ->
                {RConnPid, IConnPid, RPubkey, RPrivkey,
                                    IPubkey, IPrivkey}
        end,
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, update,
                                                         get, error]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, update,
                                                         error]),

    %% sender initiates an update
    GetBothBalances = fun(ConnPid) ->
                              sc_ws_get_both_balances(
                                ConnPid, StarterPubkey, AcknowledgerPubkey, Config)
                      end,
    {ok, {Ba0, Bb0} = Bal0} = GetBothBalances(IConnPid),
    ct:log("Balances before: ~p", [Bal0]),
    UpdateOpts = #{from => aeser_api_encoder:encode(account_pubkey, StarterPubkey),
                   to => aeser_api_encoder:encode(account_pubkey, AcknowledgerPubkey),
                   amount => Amount},
    if TestErrors ->
            ws_send_tagged(StarterPid, <<"channels.update.new">>,
                           UpdateOpts#{amount => <<"1">>}, Config),
            {ok, #{<<"reason">> := <<"not_a_number">>}} =
                wait_for_channel_event(StarterPid, error, Config),
            ws_send_tagged(StarterPid, <<"channels.update.new">>,
                           UpdateOpts#{from => <<"ABCDEF">>}, Config),
            {ok, #{<<"reason">> := <<"broken_encoding: accounts">>}} =
                wait_for_channel_event(StarterPid, error, Config),
            ws_send_tagged(StarterPid, <<"channels.update.new">>,
                           UpdateOpts#{to => <<"ABCDEF">>}, Config),
            {ok, #{<<"reason">> := <<"broken_encoding: accounts">>}} =
                wait_for_channel_event(StarterPid, error, Config);
       true -> ok
    end,
    ws_send_tagged(StarterPid, <<"channels.update.new">>,
                   UpdateOpts, Config),

    %% starter signs the new state
    #{tx := UnsignedStateTx,
      updates := Updates} = channel_sign_tx(StarterPubkey, StarterPid, StarterPrivkey, <<"channels.update">>, Config),
    ct:log("Unsigned state tx ~p", [UnsignedStateTx]),
    %% verify contents
    {channel_offchain_tx, _OffchainTx} = aetx:specialize_type(UnsignedStateTx),
    [Update] = Updates,
    Expected = aesc_offchain_update:op_transfer(aeser_id:create(account, StarterPubkey),
                                                aeser_id:create(account, AcknowledgerPubkey), Amount),
    ExpectedForClient = aesc_offchain_update:for_client(Expected),
    {ExpectedForClient, _} = {Update, ExpectedForClient},


    %% acknowledger signs the new state
    {ok, #{<<"event">> := <<"update">>}} = wait_for_channel_event(AcknowledgerPid, info, Config),
    #{tx := UnsignedStateTx,
      updates := Updates} = channel_sign_tx(AcknowledgerPubkey, AcknowledgerPid, AcknowledgerPrivkey, <<"channels.update_ack">>, Config),

    {ok, #{<<"state">> := NewState}} = wait_for_channel_event(IConnPid, update, Config),
    {ok, #{<<"state">> := NewState}} = wait_for_channel_event(RConnPid, update, Config),
    {ok, SignedStateTxBin} = aeser_api_encoder:safe_decode(transaction, NewState),
    SignedStateTx = aetx_sign:deserialize_from_binary(SignedStateTxBin),

    %% validate it is co-signed
    {ok, Trees} = rpc(aec_chain, get_top_state, []),
    ok = rpc(aetx_sign, verify, [SignedStateTx, Trees]), % RPC because of DB
    {UnsignedStateTx, _} = % same transaction that was signed
        {aetx_sign:innermost_tx(SignedStateTx), UnsignedStateTx},

    {ok, {Ba1, Bb1} = Bal1} = GetBothBalances(IConnPid),
    ct:log("Balances after: ~p", [Bal1]),
    Ba1 = Ba0 - Amount,
    Bb1 = Bb0 + Amount,

    {ok, #{ trees := StateTrees
          , signed_tx := StateSignedStateTx }} = sc_ws_get_state(IConnPid, Config),
    SignedStateTx = StateSignedStateTx,
    assert_trees_balance(StateTrees, StarterPubkey, Ba1),
    assert_trees_balance(StateTrees, AcknowledgerPubkey, Bb1),

    ok = ?WS:unregister_test_for_channel_events(IConnPid, [sign, info, update,
                                                           get, error]),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [sign, info, update,
                                                           error]),

    ok.


channel_update_fail(#{initiator := IConnPid, responder :=RConnPid},
               StarterRole,
               #{initiator := #{pub_key := IPubkey},
                 responder := #{pub_key := RPubkey}},
               Amount, Config) ->
    {StarterPid, _AcknowledgerPid, StarterPubkey, AcknowledgerPubkey} =
        case StarterRole of
            initiator ->
                {IConnPid, RConnPid, IPubkey, RPubkey};
            responder ->
                {RConnPid, IConnPid, RPubkey, IPubkey}
        end,
    ok = ?WS:register_test_for_channel_event(StarterPid, error),

    %% sender initiates an update
    ws_send_tagged(StarterPid, <<"channels.update.new">>,
                   #{from => aeser_api_encoder:encode(account_pubkey, StarterPubkey),
                     to => aeser_api_encoder:encode(account_pubkey, AcknowledgerPubkey),
                     amount => Amount}, Config),

    {ok, _Payload}= Res = wait_for_channel_event(StarterPid, error, Config),


    ok = ?WS:unregister_test_for_channel_event(StarterPid, error),
    Res.

sc_ws_close_(ConfigList) ->
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, ConfigList),


    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

sc_ws_get_balance(ConnPid, PubKey, Config) ->
    Account = aeser_api_encoder:encode(account_pubkey, PubKey),
    {ok, Res} = query_balances(ConnPid, [Account], Config),
    [#{<<"account">> := Account, <<"balance">> := B}] = Res,
    {ok, B}.

sc_ws_get_both_balances(ConnPid, PubKeyI, PubKeyR, Config) ->
    AccountI = aeser_api_encoder:encode(account_pubkey, PubKeyI),
    AccountR = aeser_api_encoder:encode(account_pubkey, PubKeyR),
    {ok, Res} = query_balances(ConnPid, [AccountI, AccountR], Config),
    [#{<<"account">> := AccountI, <<"balance">> := BI},
     #{<<"account">> := AccountR, <<"balance">> := BR}] = Res,
    {ok, {BI, BR}}.

query_balances(ConnPid, Accounts, Config) ->
    query_balances_(ConnPid, Accounts, sc_ws_protocol(Config)).

query_balances_(ConnPid, Accounts, <<"json-rpc">>) ->
    {ok, ?WS:json_rpc_call(
            ConnPid, #{ <<"method">> => <<"channels.get.balances">>
                      , <<"params">> => #{<<"accounts">> => Accounts} })}.

request_slash(ConnPid) ->
    {ok, ?WS:json_rpc_call(
            ConnPid, #{ <<"method">> => <<"channels.slash">> })}.

sc_ws_get_state(ConnPid, Config) ->
    {ok, Res} = query_state(ConnPid, Config),
    #{ <<"trees">> := EncodedTrees
     , <<"calls">> := EncodedCalls
     , <<"signed_tx">> := EncodedSignedTx
     , <<"half_signed_tx">> := EncodedHalfSignedTx
     } = Res,
    {ok, STrees} = aeser_api_encoder:safe_decode(state_trees, EncodedTrees),
    Trees = aec_trees:deserialize_from_binary_without_backend(STrees),
    {ok, SCalls} = aeser_api_encoder:safe_decode(call_state_tree, EncodedCalls),
    Calls = aect_call_state_tree:from_binary_without_backend(SCalls),
    {ok, #{ trees => Trees
          , calls => Calls
          , signed_tx => decode_signed_tx(EncodedSignedTx)
          , half_signed_tx => decode_signed_tx(EncodedHalfSignedTx)}}.

decode_signed_tx(<<>>) ->
    no_tx;
decode_signed_tx(EncodedSignedTx) ->
    {ok, SSignedTx} = aeser_api_encoder:safe_decode(transaction, EncodedSignedTx),
    aetx_sign:deserialize_from_binary(SSignedTx).

query_state(ConnPid, Config) ->
    query_state_(ConnPid, sc_ws_protocol(Config)).

query_state_(ConnPid, <<"json-rpc">>) ->
    {ok, ?WS:json_rpc_call(
        ConnPid, #{ <<"method">> => <<"channels.get.offchain_state">>
                  , <<"params">> => #{} })}.

sc_ws_get_contract_assert_equal(ConnPid1, ConnPid2, Pubkey, Config) ->
    ContractState = sc_ws_get_contract(ConnPid1, Pubkey, Config),
    ContractState = sc_ws_get_contract(ConnPid2, Pubkey, Config),
    ContractState.

sc_ws_get_contract(ConnPid, Pubkey, Config) ->
    PubkeyE = aeser_api_encoder:encode(contract_pubkey, Pubkey),
    {ok, #{<<"contract">> := ContractE,
           <<"contract_state">> := ContractStateE }} = query_contract(ConnPid, PubkeyE, Config),
    #{ <<"id">>          := IdE
    , <<"owner_id">>     := OwnerIdE
    , <<"vm_version">>   := VMVersion
    , <<"abi_version">>  := ABIVersion
    , <<"active">>       := Active
    , <<"referrer_ids">> := ReferrerIdsE
    , <<"deposit">>      := Deposit
    } = ContractE,
    {ok, Id} = aeser_api_encoder:safe_decode({id_hash, [contract_pubkey]}, IdE),
    {ok, OwnerId} = aeser_api_encoder:safe_decode({id_hash, [account_pubkey]}, OwnerIdE),
    ReferrerIds = [ RId || {ok, RId} <- [aeser_api_encoder:safe_decode({id_hash, [contract_pubkey]}, RId) || RId <- ReferrerIdsE]],
    ContractState = maps:from_list([
        {Key, Val} || {{ok, Key}, {ok, Val}}
        <- [ {aeser_api_encoder:safe_decode(contract_store_key, Key),
              aeser_api_encoder:safe_decode(contract_store_value, Val)} || {Key, Val} <- maps:to_list(ContractStateE) ]
    ]),
    #{ id => Id
     , owner_id => OwnerId
     , vm_version => VMVersion
     , abi_version => ABIVersion
     , active => Active
     , referrer_ids => ReferrerIds
     , deposit => Deposit
     , state => ContractState
    }.

query_contract(ConnPid, Pubkey, Config) ->
    query_contract_(ConnPid, Pubkey, sc_ws_protocol(Config)).

query_contract_(ConnPid, Pubkey, <<"json-rpc">>) ->
    {ok, ?WS:json_rpc_call(
        ConnPid, #{ <<"method">> => <<"channels.get.contract">>
                  , <<"params">> => #{<<"pubkey">> => Pubkey}})}.

sc_ws_close_mutual_(Config, Closer) when Closer =:= initiator
                                 orelse Closer =:= responder ->
    ct:log("ConfigList = ~p", [Config]),
    #{initiator := #{pub_key := IPubkey,
                    priv_key := IPrivkey},
      responder := #{pub_key := RPubkey,
                    priv_key := RPrivkey}} = proplists:get_value(participants,
                                                                 Config),
    {IStartB, RStartB} = channel_participants_balances(IPubkey, RPubkey),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Config),
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, on_chain_tx]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, on_chain_tx]),


    CloseMutual =
        fun(CloserPubkey, CloserConn, CloserPrivkey, OtherPubkey, OtherConn, OtherPrivkey) ->
                ws_send_tagged(CloserConn, <<"channels.shutdown">>, #{}, Config),

                #{tx := ShTx,
                  updates := Updates} = channel_sign_tx(CloserPubkey, CloserConn, CloserPrivkey, <<"channels.shutdown_sign">>, Config),
                #{tx := ShTx,
                  updates := Updates} = channel_sign_tx(OtherPubkey, OtherConn, OtherPrivkey, <<"channels.shutdown_sign_ack">>, Config),
            ShTx
        end,
    ShutdownTx =
        case Closer of
            initiator -> CloseMutual(IPubkey, IConnPid, IPrivkey, RPubkey, RConnPid, RPrivkey);
            responder -> CloseMutual(RPubkey, RConnPid, RPrivkey, IPubkey, IConnPid, IPrivkey)
        end,

    {ok, #{<<"tx">> := EncodedSignedMutualTx}} = wait_for_channel_event(IConnPid, on_chain_tx, Config),
    {ok, #{<<"tx">> := EncodedSignedMutualTx}} = wait_for_channel_event(RConnPid, on_chain_tx, Config),

    {ok, SSignedMutualTx} = aeser_api_encoder:safe_decode(transaction, EncodedSignedMutualTx),
    SignedMutualTx = aetx_sign:deserialize_from_binary(SSignedMutualTx),
    %% same transaction
    ShutdownTx = aetx_sign:tx(SignedMutualTx),

    {channel_close_mutual_tx, MutualTx} = aetx:specialize_type(ShutdownTx),

    ok = wait_for_signed_transaction_in_pool(SignedMutualTx),

    assert_balance(IPubkey, IStartB),
    assert_balance(RPubkey, RStartB),

    ok = wait_for_signed_transaction_in_block(SignedMutualTx),

    IChange = aesc_close_mutual_tx:initiator_amount_final(MutualTx),
    RChange = aesc_close_mutual_tx:responder_amount_final(MutualTx),

    assert_balance(IPubkey, IStartB + IChange),
    assert_balance(RPubkey, RStartB + RChange),

    % ensure tx is not hanging in mempool
    {ok, 200, #{<<"transactions">> := []}} = get_pending_transactions(),
    ok.

sc_ws_close_solo_(Config, Closer) when Closer =:= initiator;
                                       Closer =:= responder ->
    ct:log("ConfigList = ~p", [Config]),
    #{initiator := #{pub_key  := IPubKey,
                     priv_key := IPrivKey},
      responder := #{pub_key  := RPubKey,
                     priv_key := RPrivKey}} = proplists:get_value(participants,
                                                                      Config),
    {_IStartB, _RStartB} = channel_participants_balances(IPubKey, RPubKey),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Config),
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, on_chain_tx]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, on_chain_tx]),

    CloseSolo =
        fun(CloserPubkey, CloserConn, CloserPrivKey) ->
                ws_send_tagged(CloserConn, <<"channels.close_solo">>, #{}, Config),
                #{signed_tx := CSTx} = channel_sign_tx(CloserPubkey, CloserConn, CloserPrivKey,
                                                       <<"channels.close_solo_sign">>, Config),
                CSTx
        end,
    CloseSoloTx = case Closer of
                      initiator -> CloseSolo(IPubKey, IConnPid, IPrivKey);
                      responder -> CloseSolo(RPubKey, RConnPid, RPrivKey)
                  end,

    ok = wait_for_signed_transaction_in_block(CloseSoloTx),

    {ok, #{<<"tx">> := EncodedSignedSoloTx}} = wait_for_channel_event(
                                                 IConnPid, on_chain_tx, Config),
    {ok, #{<<"tx">> := EncodedSignedSoloTx}} = wait_for_channel_event(
                                                 RConnPid, on_chain_tx, Config),
    {ok, SSignedSoloTx} = aeser_api_encoder:safe_decode(transaction, EncodedSignedSoloTx),
    SignedSoloTx = aetx_sign:deserialize_from_binary(SSignedSoloTx),
    SoloTx = aetx_sign:tx(SignedSoloTx),
    {channel_close_solo_tx, _TxI} = aetx:specialize_type(SoloTx),

    ct:log("Close_solo tx verified", []),

    {ok, 200, #{<<"transactions">> := []}} = get_pending_transactions(),

    settle_(Config, Closer),

    ok.

settle_(Config, Closer) when Closer =:= initiator;
                             Closer =:= responder ->
    #{initiator := #{priv_key := IPrivKey, pub_key := IPubKey},
      responder := #{priv_key := RPrivKey, pub_key := RPubKey}} =
          proplists:get_value(participants, Config),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Config),
    {PubKey, ConnPid, PrivKey} = case Closer of
                             initiator -> {RPubKey, RConnPid, RPrivKey};
                             responder -> {IPubKey, IConnPid, IPrivKey}
                         end,
    ws_send_tagged(ConnPid, <<"channels.settle">>, #{}, Config),

    #{signed_tx := SettleTx} = channel_sign_tx(PubKey, ConnPid, PrivKey,
                                               <<"channels.settle_sign">>, Config),

    ok = wait_for_signed_transaction_in_block(SettleTx),

    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 15),

    ok.

sc_ws_leave_(Config) ->
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Config),
    ok = ?WS:register_test_for_channel_events(IConnPid, [leave, info]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [leave, info]),
    ok = ?WS:register_test_for_events(IConnPid, websocket, [closed]),
    ok = ?WS:register_test_for_events(RConnPid, websocket, [closed]),
    %%
    ok = ws_send_tagged(IConnPid, <<"channels.leave">>, #{}, Config),
    %%
    {ok, #{id := IDi, state := StI}} = wait_for_channel_leave_msg(IConnPid, Config),
    {ok, #{id := IDr, state := StR}} = wait_for_channel_leave_msg(RConnPid, Config),
    {IDi, IDr} = {IDr, IDi},
    {StI, StR} = {StR, StI},
    {ok, #{<<"event">> := <<"died">>}} = wait_for_channel_event(IConnPid, info, Config),
    {ok, #{<<"event">> := <<"died">>}} = wait_for_channel_event(RConnPid, info, Config),
    ok = ?WS:wait_for_event(IConnPid, websocket, closed),
    ok = ?WS:wait_for_event(RConnPid, websocket, closed),
    %%
    Options = proplists:get_value(channel_options, Config),
    Port = maps:get(port, Options),
    RPort = Port+1,
    ReestablOptions = #{existing_channel_id => IDi,
                        offchain_tx => StI,
                        port => RPort,
                        protocol => maps:get(protocol, Options)},
    ReestablOptions.


sc_ws_reestablish_(ReestablOptions, Config) ->
    {ok, RrConnPid} = channel_ws_start(responder, ReestablOptions, Config),
    {ok, IrConnPid} = channel_ws_start(initiator, maps:put(
                                                    host, <<"localhost">>,
                                                    ReestablOptions), Config),
    ok = ?WS:register_test_for_channel_events(RrConnPid, [info, update]),
    ok = ?WS:register_test_for_channel_events(IrConnPid, [info, update]),
    {ok, #{<<"event">> := <<"channel_reestablished">>}} =
        wait_for_channel_event(IrConnPid, info, Config),
    {ok, #{<<"event">> := <<"channel_reestablished">>}} =
        wait_for_channel_event(RrConnPid, info, Config),
    {ok, #{<<"event">> := <<"open">>}} =
        wait_for_channel_event(IrConnPid, info, Config),
    {ok, #{<<"event">> := <<"open">>}} =
        wait_for_channel_event(RrConnPid, info, Config),
    {ok, #{<<"state">> := NewState}} = wait_for_channel_event(IrConnPid, update, Config),
    {ok, #{<<"state">> := NewState}} = wait_for_channel_event(RrConnPid, update, Config),
    ChannelClients = #{initiator => IrConnPid,
                       responder => RrConnPid},
    ok = ?WS:unregister_test_for_channel_events(RrConnPid, [info, update]),
    ok = ?WS:unregister_test_for_channel_events(IrConnPid, [info, update]),
    [{channel_clients, ChannelClients} | Config].


sc_ws_deposit_(Config, Origin) when Origin =:= initiator
                            orelse Origin =:= responder ->
    Participants= proplists:get_value(participants, Config),
    Clients = proplists:get_value(channel_clients, Config),
    {SenderRole, AckRole} =
        case Origin of
            initiator -> {initiator, responder};
            responder -> {responder, initiator}
        end,
    #{pub_key := SenderPubkey,
      priv_key:= SenderPrivkey} = maps:get(SenderRole, Participants),
    #{pub_key := AckPubkey,
      priv_key:= AckPrivkey} = maps:get(AckRole, Participants),
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),
    {SStartB, AStartB} = channel_participants_balances(SenderPubkey, AckPubkey),
    ok = ?WS:register_test_for_channel_events(SenderConnPid, [sign, info, on_chain_tx,
                                                              error]),
    ok = ?WS:register_test_for_channel_events(AckConnPid, [sign, info, on_chain_tx,
                                                           error]),
    ws_send_tagged(SenderConnPid, <<"channels.deposit">>, #{amount => <<"2">>}, Config),
    {ok, #{<<"reason">> := <<"not_a_number">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    ws_send_tagged(SenderConnPid, <<"channels.deposit">>, #{amount => 2}, Config),
    #{tx := UnsignedStateTx,
      updates := Updates} = channel_sign_tx(SenderPubkey, SenderConnPid, SenderPrivkey, <<"channels.deposit_tx">>, Config),
    {ok, #{<<"event">> := <<"deposit_created">>}} = wait_for_channel_event(AckConnPid, info, Config),
    #{tx := UnsignedStateTx,
      updates := Updates} = channel_sign_tx(AckPubkey, AckConnPid, AckPrivkey, <<"channels.deposit_ack">>, Config),
    ct:log("Unsigned state tx ~p", [UnsignedStateTx]),
    {ok, #{<<"tx">> := EncodedSignedDepositTx} = E1} = wait_for_channel_event(SenderConnPid, on_chain_tx, Config),
    ok = match_info(E1, #{<<"info">> => <<"deposit_signed">>}),
    {ok, #{<<"tx">> := EncodedSignedDepositTx} = E2} = wait_for_channel_event(AckConnPid, on_chain_tx, Config),
    ok = match_info(E2, #{<<"info">> => <<"deposit_created">>}),

    {ok, SSignedDepositTx} = aeser_api_encoder:safe_decode(transaction,
                                                     EncodedSignedDepositTx),
    SignedDepositTx = aetx_sign:deserialize_from_binary(SSignedDepositTx),
    ok = wait_for_signed_transaction_in_block(SignedDepositTx),

    {ok, #{<<"tx">> := EncodedSignedWTx} = Cc1} = wait_for_channel_event(SenderConnPid, on_chain_tx, Config),
    ok = match_info(Cc1, #{<<"info">> => <<"channel_changed">>}),
    {ok, #{<<"tx">> := EncodedSignedWTx} = Cc2} = wait_for_channel_event(AckConnPid, on_chain_tx, Config),
    ok = match_info(Cc2, #{<<"info">> => <<"channel_changed">>}),

    % assert acknowledger balance have not changed
    {SStartB1, AStartB} = channel_participants_balances(SenderPubkey, AckPubkey),
    % assert sender balance has changed
    Fee = aetx:fee(aetx_sign:tx(SignedDepositTx)),
    {SStartB1, _, _} = {SStartB - 2 - Fee, SStartB1, SStartB},

    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ?DEFAULT_MIN_DEPTH),
    {ok, #{<<"event">> := <<"own_deposit_locked">>}} = wait_for_channel_event(SenderConnPid, info, Config),
    {ok, #{<<"event">> := <<"own_deposit_locked">>}} = wait_for_channel_event(AckConnPid, info, Config),

    {ok, #{<<"event">> := <<"deposit_locked">>}} = wait_for_channel_event(SenderConnPid, info, Config),
    {ok, #{<<"event">> := <<"deposit_locked">>}} = wait_for_channel_event(AckConnPid, info, Config),
    ok = ?WS:unregister_test_for_channel_events(SenderConnPid, [sign, info, on_chain_tx,
                                                                error]),
    ok = ?WS:unregister_test_for_channel_events(AckConnPid, [sign, info, on_chain_tx,
                                                             error]),
    ok.

sc_ws_withdraw_(Config, Origin) when Origin =:= initiator
                              orelse Origin =:= responder ->
    ct:log("withdraw test, Origin == ~p", [Origin]),
    Participants = proplists:get_value(participants, Config),
    Clients = proplists:get_value(channel_clients, Config),
    {SenderRole, AckRole} =
        case Origin of
            initiator -> {initiator, responder};
            responder -> {responder, initiator}
        end,
    #{pub_key := SenderPubkey,
      priv_key:= SenderPrivkey} = maps:get(SenderRole, Participants),
    #{pub_key := AckPubkey,
      priv_key:= AckPrivkey} = maps:get(AckRole, Participants),
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),
    {SStartB, AStartB} = channel_participants_balances(SenderPubkey, AckPubkey),
    ok = ?WS:register_test_for_channel_events(SenderConnPid, [sign, info, on_chain_tx,
                                                              error]),
    ok = ?WS:register_test_for_channel_events(AckConnPid, [sign, info, on_chain_tx,
                                                           error]),
    ws_send_tagged(SenderConnPid, <<"channels.withdraw">>, #{amount => <<"2">>}, Config),
    {ok, #{<<"reason">> := <<"not_a_number">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    ws_send_tagged(SenderConnPid, <<"channels.withdraw">>, #{amount => 2}, Config),
    #{tx := UnsignedStateTx,
      updates := Updates} = channel_sign_tx(SenderPubkey, SenderConnPid, SenderPrivkey, <<"channels.withdraw_tx">>, Config),
    {ok, #{<<"event">> := <<"withdraw_created">>}} = wait_for_channel_event(AckConnPid, info, Config),
    #{tx := UnsignedStateTx,
      updates := Updates} = channel_sign_tx(AckPubkey, AckConnPid, AckPrivkey, <<"channels.withdraw_ack">>, Config),
    ct:log("Unsigned state tx ~p", [UnsignedStateTx]),
    {ok, #{<<"tx">> := EncodedSignedWTx} = E1} = wait_for_channel_event(SenderConnPid, on_chain_tx, Config),
    ok = match_info(E1, #{<<"info">> => <<"withdraw_signed">>}),
    {ok, #{<<"tx">> := EncodedSignedWTx} = E2} = wait_for_channel_event(AckConnPid, on_chain_tx, Config),
    ok = match_info(E2, #{<<"info">> => <<"withdraw_created">>}),

    ct:log("Got on_chain_tx from both"),

    {ok, SSignedWTx} = aeser_api_encoder:safe_decode(transaction, EncodedSignedWTx),
    SignedWTx = aetx_sign:deserialize_from_binary(SSignedWTx),
    ok = wait_for_signed_transaction_in_block(SignedWTx),

    ct:log("Found signed transaction in block"),

    {ok, #{<<"tx">> := EncodedSignedWTx} = Cc1} = wait_for_channel_event(SenderConnPid, on_chain_tx, Config),
    ok = match_info(Cc1, #{<<"info">> => <<"channel_changed">>}),
    {ok, #{<<"tx">> := EncodedSignedWTx} = Cc2} = wait_for_channel_event(AckConnPid, on_chain_tx, Config),
    ok = match_info(Cc2, #{<<"info">> => <<"channel_changed">>}),

    % assert acknowledger balance have not changed
    {SStartB1, AStartB} = channel_participants_balances(SenderPubkey, AckPubkey),
    % assert sender balance has changed
    Fee = aetx:fee(aetx_sign:tx(SignedWTx)),
    {SStartB1, _, _} = {SStartB + 2 - Fee, SStartB1, SStartB},

    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ?DEFAULT_MIN_DEPTH, #{strictly_follow_top => true}),
    {ok, #{<<"event">> := <<"own_withdraw_locked">>}} = wait_for_channel_event(SenderConnPid, info, Config),
    {ok, #{<<"event">> := <<"own_withdraw_locked">>}} = wait_for_channel_event(AckConnPid, info, Config),

    ct:log("own_withdraw_locked from both"),

    {ok, #{<<"event">> := <<"withdraw_locked">>}} = wait_for_channel_event(SenderConnPid, info, Config),
    {ok, #{<<"event">> := <<"withdraw_locked">>}} = wait_for_channel_event(AckConnPid, info, Config),

    ct:log("withdraw_locked from both"),

    ok = ?WS:unregister_test_for_channel_events(SenderConnPid, [sign, info, on_chain_tx,
                                                                error]),
    ok = ?WS:unregister_test_for_channel_events(AckConnPid, [sign, info, on_chain_tx,
                                                            error]),
    ct:log("sequence successful", []),
    ok.

sc_ws_contracts(Config) ->
    lists:foreach(
        fun({Owner, TestName}) ->
            sc_ws_contract_(Config, TestName, Owner)
        end,
        [{Owner, Test} || Owner <- [initiator, responder],
                          Test  <- [identity, counter, spend_test]]),
    ok.

sc_ws_oracle_contract(Config) ->
    [sc_ws_contract_generic_(Role, ContractSource, fun sc_ws_oracle_contract_/9, Config, [])
        || Role <- [initiator, responder],
           ContractSource <- [offchain]], % Oracle contract requires an oracle to be created so we skip onchain check
    ok.

sc_ws_nameservice_contract(Config) ->
    [sc_ws_contract_generic_(Role, ContractSource, fun sc_ws_nameservice_contract_/9, Config,
                            [])
        || Role <- [initiator, responder], ContractSource <- [onchain, offchain]],
    ok.

sc_ws_environment_contract(Config) ->
    [sc_ws_contract_generic_(Role, ContractSource, fun sc_ws_enviroment_contract_/9, Config,
                            [])
        || Role <- [initiator, responder], ContractSource <- [onchain, offchain]],
    ok.

sc_ws_remote_call_contract(Config) ->
    [sc_ws_contract_generic_(Role, ContractSource, fun sc_ws_remote_call_contract_/9, Config,
                            [])
        || Role <- [initiator, responder], ContractSource <- [onchain, offchain]],
    ok.

sc_ws_remote_call_contract_refering_onchain_data(Config) ->
    [sc_ws_contract_generic_(Role, ContractSource, fun sc_ws_remote_call_contract_refering_onchain_data_/9, Config,
                            [])
        || Role <- [initiator, responder], ContractSource <- [onchain, offchain]],
    ok.

random_unused_name() ->
    random_unused_name(_Attempts = 10).

random_unused_name(Attempts) when Attempts < 1->
    {error, exhausted};
random_unused_name(Attempts) ->
    Size = 10,
    RandStr = base58:binary_to_base58(crypto:strong_rand_bytes(Size)),
    NameL = RandStr ++ ".test",
    Name = list_to_binary(NameL),
    case get_names_entry_by_name_sut(Name) of
        {ok, 404, _Error} -> Name; % name not used yet
        _ -> random_unused_name(Attempts - 1)
    end.

sc_ws_contract_generic_(Origin, ContractSource, Fun, Config, Opts) ->
    %% get the infrastructure for users going
    Participants = proplists:get_value(participants, Config),
    Clients = proplists:get_value(channel_clients, Config),
    {SenderRole, AckRole} =
        case Origin of
            initiator -> {initiator, responder};
            responder -> {responder, initiator}
        end,
    #{pub_key := SenderPubkey,
      priv_key:= SenderPrivkey} = maps:get(SenderRole, Participants),
    #{pub_key := AckPubkey,
      priv_key:= AckPrivkey} = maps:get(AckRole, Participants),
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),
    ok = ?WS:register_test_for_channel_events(SenderConnPid, [sign, info, get, error]),
    ok = ?WS:register_test_for_channel_events(AckConnPid, [sign, info, get, error]),
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),
    %% helper lambda for update
    GetVolley =
        fun(Actor) ->
            case Actor =:= Origin of
                true ->
                    {fun() -> update_volley_(SenderPubkey, SenderConnPid, SenderPrivkey,
                                             AckPubkey, AckConnPid, AckPrivkey, Config) end,
                     SenderConnPid, SenderPubkey};
                false ->
                    {fun() -> update_volley_(AckPubkey, AckConnPid, AckPrivkey,
                                             SenderPubkey, SenderConnPid, SenderPrivkey, Config) end,
                     AckConnPid, AckPubkey}
            end
        end,

    GetPubkeys =
        fun(Role) ->
            case Origin =:= Role of
                true  -> {SenderPubkey, AckPubkey};
                false -> {AckPubkey, SenderPubkey}
            end
        end,

    CreateContract =
        case ContractSource of
            offchain ->
                fun(Owner, EncodedCode, EncodedInitData, Deposit) ->
                    {CreateVolley, OwnerConnPid, OwnerPubKey} = GetVolley(Owner),
                    NewContractOpts =
                        #{vm_version  => aect_test_utils:vm_version(),
                          abi_version => aect_test_utils:abi_version(),
                          deposit     => Deposit,
                          code        => EncodedCode,
                          call_data   => EncodedInitData},
                    % incorrect calls
                    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract">>,
                        NewContractOpts#{deposit => <<"1">>}, Config),
                    {ok, #{<<"reason">> := <<"not_a_number">>}} =
                        wait_for_channel_event(OwnerConnPid, error, Config),

                    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract">>,
                        NewContractOpts#{vm_version => <<"1">>}, Config),
                    {ok, #{<<"reason">> := <<"not_a_number">>}} =
                        wait_for_channel_event(OwnerConnPid, error, Config),

                    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract">>,
                        NewContractOpts#{abi_version => <<"1">>}, Config),
                    {ok, #{<<"reason">> := <<"not_a_number">>}} =
                        wait_for_channel_event(OwnerConnPid, error, Config),
                    % correct call
                    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract">>,
                        NewContractOpts, Config),

                    #{tx := UnsignedStateTx, updates := _Updates} = CreateVolley(),
                    contract_id_from_create_update(OwnerPubKey, UnsignedStateTx)
                end;
            onchain ->
                fun(Owner, EncodedCode, EncodedInitData, Deposit) ->
                    EncodedOnChainPubkey = post_contract_onchain(EncodedCode, EncodedInitData),

                    {CreateVolley, OwnerConnPid, OwnerPubKey} = GetVolley(Owner),
                    NewContractOpts =
                        #{deposit     => Deposit,
                          contract    => EncodedOnChainPubkey,
                          call_data   => EncodedInitData},
                    % incorrect call
                    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract_from_onchain">>,
                        NewContractOpts#{deposit => <<"1">>}, Config),
                    {ok, #{<<"reason">> := <<"not_a_number">>}} =
                        wait_for_channel_event(OwnerConnPid, error, Config),
                    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract_from_onchain">>,
                        NewContractOpts#{contract => <<"ABCDEF">>}, Config),
                    {ok, #{<<"reason">> := <<"broken_encoding: contracts">>}} =
                        wait_for_channel_event(OwnerConnPid, error, Config),
                    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract_from_onchain">>,
                        NewContractOpts#{call_data => <<"ABCDEF">>}, Config),
                    {ok, #{<<"reason">> := <<"broken_encoding: bytearray">>}} =
                        wait_for_channel_event(OwnerConnPid, error, Config),

                    % correct call
                    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract_from_onchain">>,
                        NewContractOpts, Config),

                    #{tx := UnsignedStateTx, updates := _Updates} = CreateVolley(),
                    contract_id_from_create_update(OwnerPubKey, UnsignedStateTx)
                end
        end,


    Actors = [{R, GetPubkeys(R)} || R <- [initiator, responder]],
    [Fun(Owner, GetVolley, CreateContract, SenderConnPid,
         AckConnPid, OwnerPubkey, OtherPubkey, Opts, Config)
        || {Owner, {OwnerPubkey, OtherPubkey}} <- Actors],
    ok = ?WS:unregister_test_for_channel_events(SenderConnPid, [sign, info, get, error]),
    ok = ?WS:unregister_test_for_channel_events(AckConnPid, [sign, info, get, error]),
    ok.

post_contract_onchain(EncodedCode, EncodedInitCallData) ->
    Pubkey = get_pubkey(),

    ValidEncoded = #{ owner_id    => Pubkey,
                      code        => EncodedCode,
                      vm_version  => aect_test_utils:vm_version(),
                      abi_version => aect_test_utils:abi_version(),
                      deposit     => 0,
                      amount      => 0,
                      gas         => 600,
                      gas_price   => aec_test_utils:min_gas_price(),
                      fee         => 400000 * aec_test_utils:min_gas_price(),
                      call_data   => EncodedInitCallData},

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
                <<"contract_id">> := EncodedContractPubKey}} = get_contract_create(ValidEncoded),
    %% {ok, ContractPubKey} = aeser_api_encoder:safe_decode(contract_pubkey, EncodedContractPubKey),
    ContractCreateTxHash = sign_and_post_tx(EncodedUnsignedContractCreateTx),

    ?assertMatch({ok, 200, _}, get_transactions_by_hash_sut(ContractCreateTxHash)),
    ?assertEqual({ok, 404, #{<<"reason">> => <<"Tx not mined">>}}, get_contract_call_object(ContractCreateTxHash)),

    % mine
    ok = wait_for_tx_hash_on_chain(ContractCreateTxHash),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    ?assertMatch({ok, 200, _}, get_transactions_by_hash_sut(ContractCreateTxHash)),
    ?assertMatch({ok, 200, _}, get_contract_call_object(ContractCreateTxHash)),
    ?assertMatch({ok, 200, #{<<"id">>          := EncodedContractPubKey,
        <<"owner_id">>    := Pubkey,
        <<"active">>      := true,
        <<"deposit">>     := 0,
        <<"referrer_ids">> := []}},
        get_contract_sut(EncodedContractPubKey)),
    EncodedContractPubKey.

sc_ws_oracle_contract_(Owner, GetVolley, CreateContract, ConnPid1, ConnPid2,
                       OwnerPubkey, OtherPubkey, _Opts, Config) ->
    %% Register an oracle. It will be used in an off-chain contract
    %% Oracle ask itself a question and answers it
    {OraclePubkey, OraclePrivkey} = ?CAROL,
    ok = initialize_account(2000000 * aec_test_utils:min_gas_price(), ?CAROL),

    SophiaStringType =
        case aect_test_utils:backend() of
            aevm -> aeb_heap:to_binary(string, 0);
            fate -> iolist_to_binary(aeb_fate_encoding:serialize_type(string))
        end,
    SophiaString =
        fun(S0) ->
            S = list_to_binary(tl(lists:droplast(S0))),
            case aect_test_utils:backend() of
                aevm -> aeb_heap:to_binary(S, 0);
                fate -> aeb_fate_encoding:serialize(S)
            end
        end,
    QueryFee = 3,
    OracleTTL = 20,
    QueryTTL = 5,
    ResponseTTL = 5,
    Question = "\"Fill me in with something reasonable\"",
    register_oracle(OraclePubkey, OraclePrivkey,
                    #{query_format    => SophiaStringType,
                      response_format => SophiaStringType,
                      query_fee       => QueryFee,
                      query_ttl       => QueryTTL,
                      abi_version     => aect_test_utils:abi_version(),
                      oracle_ttl      => {delta, OracleTTL}
                     }),
    OracleQuerySequence =
        fun(Q0, R0) ->
            Q = SophiaString(Q0),
            R = SophiaString(R0),
            QueryId = query_oracle(OraclePubkey, OraclePrivkey, %oracle asks oracle
                                   OraclePubkey,
                                   #{query        => Q,
                                     query_ttl    => {delta, QueryTTL - 1},
                                     response_ttl => {delta, ResponseTTL}}),
            respond_oracle(OraclePubkey, OraclePrivkey, QueryId,
                           R, #{response_ttl => {delta, ResponseTTL}}),
            QueryId
        end,

    EncodedCode = contract_byte_code("channel_on_chain_contract_oracle"),
    {ok, EncodedInitData} = encode_call_data(channel_on_chain_contract_oracle,
                              "init", [aeser_api_encoder:encode(oracle_pubkey, OraclePubkey), Question]),

    ContractPubKey = CreateContract(Owner, EncodedCode, EncodedInitData, 10),

    ContractId = aeser_id:create(contract, ContractPubKey),
    OwnerId = aeser_id:create(account, OwnerPubkey),
    ExpectedABIVersion = aect_test_utils:abi_version(),
    #{ active := true
     , abi_version := ExpectedABIVersion
     , deposit := 10
     , id := ContractId
     , owner_id := OwnerId
     , referrer_ids := []
     } = sc_ws_get_contract_assert_equal(ConnPid1, ConnPid2, ContractPubKey, Config),

    CallContract =
        fun(Who, Fun, Args, ReturnType, Result) ->
            {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
            R0 = dry_call_a_contract(Fun, Args, ContractPubKey,
                                     channel_on_chain_contract_oracle, UpdaterConnPid, Config),
            #{tx := Tx, updates := Updates} =
                call_a_contract(Fun, Args, ContractPubKey, channel_on_chain_contract_oracle,
                                UpdaterConnPid, UpdateVolley, Config),
            R =
                ws_get_decoded_result(ConnPid1, ConnPid2, channel_on_chain_contract_oracle,
                                      Fun, Updates, Tx, Config),
            {R, R} = {R0, R},
            {R, R} = {Result, R}

        end,
    [CallContract(Who, "place_bet", [Bet],
                  <<"string">>, <<"ok">>)
        || {Who, Bet} <- [{initiator, "\"I win\""},
                          {responder, "\"no, I win\""}]],
    #{ active := true
     , abi_version := ExpectedABIVersion
     , deposit := 10
     , id := ContractId
     , owner_id := OwnerId
     , referrer_ids := []
     , state := State2
     } = sc_ws_get_contract_assert_equal(ConnPid1, ConnPid2, ContractPubKey, Config),
    %% initiator places a bet and then nobody can overwrite it
    ParkedAnswer = "\"I claim this\"",
    CallContract(initiator, "place_bet", [ParkedAnswer],
                  <<"string">>, <<"ok">>),
    [CallContract(Who, "place_bet", [ParkedAnswer],
                  <<"string">>, <<"bet_already_taken">>)
        || Who <- [initiator, responder]],

    %% place some oracle query id with a different question
    ErrQueryId = OracleQuerySequence("\"other question\"", "\"some answer\""),
    [CallContract(Who, "resolve", [aeser_api_encoder:encode(oracle_query_id, ErrQueryId)],
                  <<"string">>, <<"different question">>)
        || Who <- [initiator, responder]],
    #{ active := true
     , abi_version := ExpectedABIVersion
     , deposit := 10
     , id := ContractId
     , owner_id := OwnerId
     , referrer_ids := []
     , state := State3
     } = sc_ws_get_contract_assert_equal(ConnPid1, ConnPid2, ContractPubKey, Config),
    Answer = "\"other reasonable thingy\"",
    CorrectQueryId = OracleQuerySequence(Question, Answer),
    EncodedQueryId = aeser_api_encoder:encode(oracle_query_id, CorrectQueryId),

    {ok, {OwnerBal0, OtherBal0}} = sc_ws_get_both_balances(ConnPid1,
                                                           OwnerPubkey,
                                                           OtherPubkey,
                                                           Config),
    CallContract(Owner, "resolve", [EncodedQueryId],
                 <<"string">>, <<"no winning bet">>),
    sc_ws_get_contract_assert_equal(ConnPid1, ConnPid2, ContractPubKey, Config),
    % no changes in balances
    {ok, {OwnerBal0, OtherBal0}} = sc_ws_get_both_balances(ConnPid1,
                                                           OwnerPubkey,
                                                           OtherPubkey,
                                                           Config),

    % owner posts the correct
    CallContract(Owner, "place_bet", [Answer], <<"string">>, <<"ok">>),
    #{ active := true
     , abi_version := ExpectedABIVersion
     , deposit := 10
     , id := ContractId
     , owner_id := OwnerId
     , referrer_ids := []
     , state := State4
    } = sc_ws_get_contract_assert_equal(ConnPid1, ConnPid2, ContractPubKey, Config),

    {ok, {OwnerBal, OtherBal0}} = sc_ws_get_both_balances(ConnPid1,
                                                          OwnerPubkey,
                                                          OtherPubkey,
                                                          Config),
    {ok, ContractBalance} = sc_ws_get_balance(ConnPid1, ContractPubKey, Config),
    CallContract(Owner, "resolve", [EncodedQueryId], <<"string">>, <<"ok">>),
    sc_ws_get_contract_assert_equal(ConnPid1, ConnPid2, ContractPubKey, Config),

    % contract balance is 0 now
    {ok, 0} = sc_ws_get_balance(ConnPid1, ContractPubKey, Config),
    {ok, 0} = sc_ws_get_balance(ConnPid2, ContractPubKey, Config),

    % contract owner balance is updated
    {ok, {OwnerBal1, OtherBal0}} = sc_ws_get_both_balances(ConnPid1,
                                                           OwnerPubkey,
                                                           OtherPubkey,
                                                           Config),
    {ok, {OwnerBal1, OtherBal0}} = sc_ws_get_both_balances(ConnPid2,
                                                           OwnerPubkey,
                                                           OtherPubkey,
                                                           Config),
    {OwnerBal1, _} = {OwnerBal + ContractBalance, OwnerBal1},
    ok.

sc_ws_nameservice_contract_(Owner, GetVolley, CreateContract, ConnPid1, ConnPid2,
                            _OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    Name = random_unused_name(),
    %% Register an oracle. It will be used in an off-chain contract
    %% Oracle ask itself a question and answers it
    {NamePubkey, NamePrivkey} = ?CAROL,
    ok = initialize_account(2000000 * aec_test_utils:min_gas_price(), ?CAROL),

    EncodedCode = contract_byte_code("channel_on_chain_contract_name_resolution"),
    {ok, EncodedInitData} = encode_call_data(channel_on_chain_contract_name_resolution,
                              "init", []),

    ContractPubKey = CreateContract(Owner, EncodedCode, EncodedInitData, 10),

    ContractCanNameResolve =
        fun(Who, Name0, Key0, Result) ->
            {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
            AddQuotes = fun(B) when is_binary(B) -> <<"\"", B/binary, "\"">> end,
            QName = AddQuotes(Name0),
            QKey = AddQuotes(Key0),
            Args = [QName, QKey],
            FunctionName = "can_resolve",
            R0 = dry_call_a_contract(FunctionName, Args, ContractPubKey,
                                     channel_on_chain_contract_name_resolution,
                                     UpdaterConnPid, Config),

            #{tx := Tx, updates := Updates} =
                call_a_contract(FunctionName, Args, ContractPubKey,
                                channel_on_chain_contract_name_resolution,
                                UpdaterConnPid, UpdateVolley, Config),
            R = ws_get_decoded_result(ConnPid1, ConnPid2,
                                      channel_on_chain_contract_name_resolution,
                                      FunctionName, Updates, Tx, Config),
            {R, R} = {R0, R},
            {R, R} = {Result, R}

        end,
    Test =
        fun(Name1, Key, Result) ->
            [ContractCanNameResolve(Who, Name1, Key, Result)
                || Who <- [initiator, responder]]
        end,

    Test(Name, <<"oracle">>, false),
    register_name(NamePubkey, NamePrivkey, Name,
                  [{<<"account_pubkey">>, aeser_id:create(account, <<1:256>>)},
                   {<<"oracle">>, aeser_id:create(oracle, <<2:256>>)},
                   {<<"unexpected_key">>, aeser_id:create(account, <<3:256>>)}]),
    Test(Name, <<"account_pubkey">>, true),
    Test(Name, <<"oracle">>, true),
    Test(Name, <<"unexpected_key">>, true),
    Test(Name, <<"missing_key">>, false),
    ok.

sc_ws_enviroment_contract_(Owner, GetVolley, CreateContract, ConnPid1, ConnPid2,
                           _OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    EncodedCode = contract_byte_code("channel_env"),
    {ok, EncodedInitData} = encode_call_data(channel_env, "init", []),

    ContractPubKey = CreateContract(Owner, EncodedCode, EncodedInitData, 10),

    ContractCall =
        fun(Who, Fun, Result) ->
            {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
            Args = [],
            R0 = dry_call_a_contract(Fun, Args, ContractPubKey,
                                     channel_env, UpdaterConnPid, Config),
            #{tx := Tx, updates := Updates} =
                call_a_contract(Fun, Args, ContractPubKey, channel_env,
                                UpdaterConnPid, UpdateVolley, Config),
            R = ws_get_decoded_result(ConnPid1, ConnPid2, channel_env,
                                      Fun, Updates, Tx, Config),
            {R, R} = {R0, R},
            case is_function(Result) of
                true -> true = Result(R);
                false ->
                    {R, R} = {Result, R}
            end
        end,
    Test =
        fun(Fun, Result) ->
            [ContractCall(Who, Fun, Result)
                || Who <- [initiator, responder]]
        end,
    {ok, 200, Block} = get_key_blocks_current_sut(),
    #{<<"height">> := BlockHeight,
      <<"beneficiary">> := EncBeneficiary,
      <<"time">> := Time
     } = Block,

    Test(<<"block_height">>, BlockHeight),
    Test(<<"coinbase">>, EncBeneficiary),
    Test(<<"timestamp">>, fun(T) -> T > Time end),
    ok.

sc_ws_remote_call_contract_(Owner, GetVolley, CreateContract, ConnPid1, ConnPid2,
                            _OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    %% create identity contract off-chain
    CreateIdentityContract =
        fun(Name) ->
            EncodedCode = contract_byte_code(Name),
            {ok, EncodedInitData} = encode_call_data(Name, "init", []),
            CreateContract(Owner, EncodedCode, EncodedInitData, 10)
        end,
    IdentityCPubKey   = CreateIdentityContract(identity),
    RemoteCallCPubKey = CreateIdentityContract(remote_call),

    ContractCall =
        fun(Who, ContractPubKey, Contract, Fun, Args, Result, Amount) ->
                {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
                R0 = dry_call_a_contract(Fun, Args, ContractPubKey, Contract,
                                         UpdaterConnPid, Amount, Config),
                #{tx := Tx, updates := Updates} =
                    call_a_contract(Fun, Args, ContractPubKey, Contract,
                                    UpdaterConnPid, UpdateVolley, Amount, Config),
                R = ws_get_decoded_result(ConnPid1, ConnPid2, Contract,
                                          Fun, Updates, Tx, Config),
                {R, R} = {R0, R},
                {R, R} = {Result, R}
        end,
    CallIdentity =
        fun(Who, Val) ->
            ValB = integer_to_list(Val),
            ContractCall(Who, IdentityCPubKey, identity, <<"main">>,
                         [ValB], Val, _Amount = 0)
        end,
    EncIdPubkey = aeser_api_encoder:encode(contract_pubkey, IdentityCPubKey),
    CallRemoteContract =
        fun(Who, Val) ->
            ValB = integer_to_list(Val),
            ContractCall(Who, RemoteCallCPubKey, remote_call, <<"call">>,
                         [EncIdPubkey, ValB], Val,
                         % beacuse of hardcoded value=10 in the
                         % remote_call.aes -> amount in the call must be > 10
                         _Amount = 20)
        end,
    Test =
        fun(Fun, Val) ->
            [Fun(Who, Val)
                || Who <- [initiator, responder]]
        end,

    % actual tests
    Test(CallIdentity, 10),
    Test(CallIdentity, 11),
    Test(CallRemoteContract, 42),
    Test(CallRemoteContract, 43),
    Test(CallIdentity, 12),
    ok.

sc_ws_remote_call_contract_refering_onchain_data_(Owner, GetVolley, CreateContract,
                                                  ConnPid1, ConnPid2, _OwnerPubkey,
                                                  _OtherPubkey, _Opts, Config) ->
    %% create identity contract off-chain
    CreateIdentityContract =
        fun(Name) ->
            EncodedCode = contract_byte_code(Name),
            {ok, EncodedInitData} = encode_call_data(Name, "init", []),
            CreateContract(Owner, EncodedCode, EncodedInitData, 10)
          end,
    ResolverCPubKey   = CreateIdentityContract(channel_on_chain_contract_name_resolution),
    RemoteCallCPubKey = CreateIdentityContract(channel_remote_on_chain_contract_name_resolution),

    ContractCall =
        fun(Who, ContractPubKey, Contract, Fun, Args, Result, Amount) ->
                {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
                R0 = dry_call_a_contract(Fun, Args, ContractPubKey, Contract,
                                         UpdaterConnPid, Amount, Config),
                #{tx := Tx, updates := Updates} =
                    call_a_contract(Fun, Args, ContractPubKey, Contract,
                                    UpdaterConnPid, UpdateVolley, Amount, Config),
                R = ws_get_decoded_result(ConnPid1, ConnPid2, Contract, Fun,
                                          Updates, Tx, Config),
                {R, R} = {R0, R},
                {R, R} = {Result, R}
        end,
    CallResolve =
        fun(Who, Name, Key, IsResolvable) ->
            AddQuotes = fun(B) when is_binary(B) -> <<"\"", B/binary, "\"">> end,
            QName = AddQuotes(Name),
            QKey = AddQuotes(Key),
            Args = [QName, QKey],
            ContractCall(Who, ResolverCPubKey, channel_on_chain_contract_name_resolution,
                         <<"can_resolve">>, Args, IsResolvable, _Amount = 0)
        end,
    EncResPubkey = aeser_api_encoder:encode(contract_pubkey, ResolverCPubKey),
    CallRemoteContract =
        fun(Who, Name, Key, IsResolvable) ->
            AddQuotes = fun(B) when is_binary(B) -> <<"\"", B/binary, "\"">> end,
            QName = AddQuotes(Name),
            QKey = AddQuotes(Key),
            Args = [EncResPubkey, QName, QKey],
            ContractCall(Who, RemoteCallCPubKey,
                         channel_remote_on_chain_contract_name_resolution,
                         <<"remote_resolve">>, Args, IsResolvable,
                         % beacuse of hardcoded value=10 in the
                         % remote_call.aes -> amount in the call must be > 10
                         _Amount = 20)
        end,
    Test =
        fun(Fun, N, K, Res) ->
            [Fun(Who, N, K, Res)
                || Who <- [initiator, responder]]
        end,

    % actual tests
    % we have two contracts: c
    % * channel_on_chain_contract_name_resolution.aes that has
    %     `can_resolve(Name, Key)` function. It resolves on-chain names
    % * channel_remote_on_chain_contract_name_res.aes that has
    %     `remote_resolve(Contract, Name, Key)` function that makes a remote
    %     call to the first contract and uses it to resolve the name on-chain
    % both functions shall return the same result
    Name = random_unused_name(),

    % name is not present on-chain, both contracts shall return false:
    Test(CallResolve, Name, <<"account_pubkey">>, false),
    Test(CallRemoteContract, Name, <<"account_pubkey">>, false),

    % registering the name on-chain
    {NamePubkey, NamePrivkey} = ?CAROL,
    ok = initialize_account(2000000 * aec_test_utils:min_gas_price(), ?CAROL),
    register_name(NamePubkey, NamePrivkey, Name,
                  [{<<"account_pubkey">>, aeser_id:create(account, <<1:256>>)}]),

    % now the name is on-chain, both must return true:
    Test(CallResolve, Name, <<"account_pubkey">>, true),
    case aect_test_utils:backend() of
        aevm ->
            Test(CallRemoteContract, Name, <<"account_pubkey">>, false); % BUG
        fate ->
            Test(CallRemoteContract, Name, <<"account_pubkey">>, true)
    end,
    ok.

register_oracle(OraclePubkey, OraclePrivkey, Opts) ->
    case rpc(aec_chain, get_oracle, [OraclePubkey]) of
        {error, not_found} -> pass;
        {ok, Oracle} ->
            Height = current_height(),
            TTL = aeo_oracles:ttl(Oracle), % absolute TTL
            ExpBlocksCnt = TTL - Height,
            ct:log("Already an oracle, mining ~p blocks so it expires",
                   [ExpBlocksCnt]),
            Node = aecore_suite_utils:node_name(?NODE),
            aecore_suite_utils:mine_key_blocks(Node, ExpBlocksCnt + 1)
    end,
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [OraclePubkey]),
    Tx = aeo_test_utils:register_tx(OraclePubkey, Opts#{nonce => Nonce}, #{}),
    sign_post_mine(Tx, OraclePrivkey),
    OracleId = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {ok, 200, _Resp} = get_oracles_by_pubkey_sut(OracleId),
    ok.

query_oracle(FromPubkey, FromPrivkey, OraclePubkey, Opts) ->
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [FromPubkey]),
    Tx = aeo_test_utils:query_tx(FromPubkey, aeser_id:create(oracle, OraclePubkey),
                                 Opts#{nonce => Nonce}, #{}),
    sign_post_mine(Tx, FromPrivkey),
    {aeo_query_tx, QueryTx} = aetx:specialize_callback(Tx),
    aeo_query_tx:query_id(QueryTx).

respond_oracle(OraclePubkey, OraclePrivkey, QueryId, Response, Opts) ->
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [OraclePubkey]),
    Tx = aeo_test_utils:response_tx(OraclePubkey, QueryId,
                                    Response, Opts#{nonce => Nonce}, #{}),
    sign_post_mine(Tx, OraclePrivkey),
    ok.

sign_post_mine(Tx, Privkey) ->
    SignedTx = aec_test_utils:sign_tx(Tx, Privkey),
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    EncodedSerializedSignedTx =
        aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedTx)),
    ok = post_tx(TxHash, EncodedSerializedSignedTx),
    ok = wait_for_tx_hash_on_chain(TxHash).

register_name(Owner, OwnerPrivKey, Name, Pointers) ->
    Salt = rand:uniform(10000),
    preclaim_name(Owner, OwnerPrivKey, Name, Salt),
    claim_name(Owner, OwnerPrivKey, Name, Salt),
    update_pointers(Owner, OwnerPrivKey, Name, Pointers),
    ok.

preclaim_name(Owner, OwnerPrivKey, Name, Salt) ->
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [Owner]),
    CHash = aens_hash:commitment_hash(NameAscii, Salt),
    TxSpec = aens_test_utils:preclaim_tx_spec(Owner, CHash, #{nonce => Nonce}, #{}),
    {ok, Tx} = aens_preclaim_tx:new(TxSpec),
    sign_post_mine(Tx, OwnerPrivKey),
    ok.

claim_name(Owner, OwnerPrivKey, Name, Salt) ->
    Delta = aec_governance:name_claim_preclaim_delta(),
    Node = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:mine_key_blocks(Node, Delta),
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [Owner]),
    TxSpec = aens_test_utils:claim_tx_spec(Owner, Name, Salt,  #{nonce => Nonce},#{}),
    {ok, Tx} = aens_claim_tx:new(TxSpec),
    sign_post_mine(Tx, OwnerPrivKey),
    ok.

update_pointers(Owner, OwnerPrivKey, Name, Pointers0) ->
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [Owner]),
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    NHash = aens_hash:name_hash(NameAscii),
    Pointers =
        lists:map(
            fun({PointerName, Value}) ->
                aens_pointer:new(PointerName, Value)
            end,
            Pointers0),
    NameTTL  = 40000,
    TxSpec = aens_test_utils:update_tx_spec(
                Owner, NHash, #{pointers => Pointers,
                                name_ttl => NameTTL,
                                nonce => Nonce}, #{}),
    {ok, Tx} = aens_update_tx:new(TxSpec),
    sign_post_mine(Tx, OwnerPrivKey),
    ok.

initialize_account(Amount, KeyPair) ->
    initialize_account(Amount, KeyPair, true).

initialize_account(Amount, {Pubkey, _Privkey}, Check) ->
    Fee = ?SPEND_FEE,
    Node = aecore_suite_utils:node_name(?NODE),

    {ok, 200, #{<<"tx">> := SpendTx}} =
        post_spend_tx(aeser_api_encoder:encode(account_pubkey, Pubkey), Amount, Fee),
    TxHash = sign_and_post_tx(SpendTx),
    if Check ->
        aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [TxHash], ?MAX_MINED_BLOCKS),
        assert_balance_at_least(Pubkey, Amount),
        ok;
       true ->
        TxHash
    end.

update_volley_(FirstPubkey, FirstConnPid, FirstPrivkey, SecondPubkey, SecondConnPid, SecondPrivkey, Config) ->
    #{tx := UnsignedStateTx,
      updates := Updates} = channel_sign_tx(FirstPubkey, FirstConnPid, FirstPrivkey, <<"channels.update">>, Config),

    % acknowledger signs update_ack
    {ok, #{<<"event">> := <<"update">>}} = wait_for_channel_event(SecondConnPid, info, Config),
    #{tx := UnsignedStateTx,
      updates := Updates} = channel_sign_tx(SecondPubkey, SecondConnPid, SecondPrivkey,
                                            <<"channels.update_ack">>, Config).

sc_ws_contract_(Config, TestName, Owner) ->
    Participants = proplists:get_value(participants, Config),
    Clients = proplists:get_value(channel_clients, Config),
    {SenderRole, AckRole} =
        case Owner of
            initiator -> {initiator, responder};
            responder -> {responder, initiator}
        end,
    #{pub_key := SenderPubkey,
      priv_key:= SenderPrivkey} = maps:get(SenderRole, Participants),
    #{pub_key := AckPubkey,
      priv_key:= AckPrivkey} = maps:get(AckRole, Participants),
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),

    ok = ?WS:register_test_for_channel_events(SenderConnPid, [sign, info, get, error]),
    ok = ?WS:register_test_for_channel_events(AckConnPid, [sign, info, get, error]),

    %% helper lambda for update
    UpdateVolley =
        fun() ->
            update_volley_(SenderPubkey, SenderConnPid, SenderPrivkey,
                           AckPubkey, AckConnPid, AckPrivkey, Config)
        end,
    UpdateVolleyReverse =
        fun() ->
            update_volley_(AckPubkey, AckConnPid, AckPrivkey,
                           SenderPubkey, SenderConnPid, SenderPrivkey, Config)
        end,

    % trigger new contract
    {UnsignedStateTx, _Updates, Code} = create_contract_(TestName, SenderConnPid, UpdateVolley, Config),

    ContractPubKey = contract_id_from_create_update(SenderPubkey,
                                                    UnsignedStateTx),

    %% helper lambdas for pruning and call not found
    PruneCalls =
        fun(ConnPid) ->
            ok = ?WS:register_test_for_channel_events(ConnPid, [calls_pruned]),
            ws_send_tagged(ConnPid, <<"channels.clean_contract_calls">>, #{}, Config),
            {ok, _} = wait_for_channel_event(ConnPid, calls_pruned, Config),
            ok = ?WS:unregister_test_for_channel_events(ConnPid, [calls_pruned])
        end,
    CallMissingCall =
        fun(Tx, [U], ConnPid) ->
            ws_send_tagged(ConnPid, <<"channels.get.contract_call">>,
                           ws_get_call_params(U, Tx), Config),
            {ok, #{<<"reason">> := <<"call_not_found">>}} = wait_for_channel_event(ConnPid, error, Config),
            ok
        end,

    % trigger call contract
    % owner can call a contract
    {Fun, SomeUnsignedStateTx, Updates} =
        contract_calls_(TestName, ContractPubKey, SenderConnPid, UpdateVolley,
                        AckConnPid, SenderPubkey, AckPubkey, Config),
    _ = ws_get_decoded_result(SenderConnPid, AckConnPid, TestName, Fun,
                              Updates, SomeUnsignedStateTx, Config),
    ok = PruneCalls(SenderConnPid),
    ok = CallMissingCall(SomeUnsignedStateTx, Updates, SenderConnPid),
    % state is still usable

    % acknowledger can call a contract
    contract_calls_(TestName, ContractPubKey, AckConnPid, UpdateVolleyReverse,
                    SenderConnPid, AckPubkey, SenderPubkey, Config),

    GetPoI =
        fun(ConnPid) ->
                Scope = #{contracts   => [aeser_api_encoder:encode(contract_pubkey, ContractPubKey)],
                          accounts    => [aeser_api_encoder:encode(account_pubkey, SenderPubkey),
                                          aeser_api_encoder:encode(account_pubkey, AckPubkey)]
                         },
                {ok, #{poi := P}} = sc_ws_get_poi_(ConnPid, Scope, Config),
                P
        end,

    GetMissingPoI =
        fun(ConnPid, Accs, Cts) ->
            ws_send_tagged(ConnPid, <<"channels.get.poi">>,
                            #{contracts   => [aeser_api_encoder:encode(contract_pubkey, C) || C <- Cts],
                              accounts    => [aeser_api_encoder:encode(account_pubkey, Acc) || Acc <- Accs]
                            }, Config),

                    {ok, #{<<"reason">> := R}} = wait_for_channel_event(ConnPid, error, Config),
                    R
                end,

    EncodedPoI = GetPoI(SenderConnPid),
    EncodedPoI = GetPoI(AckConnPid),

    NegativePoiTests =
        fun(ConnPid) ->
            <<"broken_encoding: accounts">> = GetMissingPoI(ConnPid, [<<123456789>>], []),
            <<"broken_encoding: contracts">> = GetMissingPoI(ConnPid, [], [<<123456789>>]),
            <<"broken_encoding: accounts, contracts">> = GetMissingPoI(ConnPid, [<<123456789>>], [<<123456789>>]),
            AccountByteSize = aeser_api_encoder:byte_size_for_type(account_pubkey),
            FakeAccountId = <<42:AccountByteSize/unit:8>>,
            <<"not_found">> = GetMissingPoI(ConnPid, [FakeAccountId], []),
            ContractByteSize = aeser_api_encoder:byte_size_for_type(contract_pubkey),
            FakeContractId = <<42:ContractByteSize/unit:8>>,
            <<"not_found">> = GetMissingPoI(ConnPid, [], [FakeContractId])
        end,

    NegativePoiTests(SenderConnPid),
    NegativePoiTests(AckConnPid),

    {ok, PoIBin} = aeser_api_encoder:safe_decode(poi, EncodedPoI),
    PoI = aec_trees:deserialize_poi(PoIBin),
    {ok, _SenderAcc} = aec_trees:lookup_poi(accounts, SenderPubkey, PoI),
    {ok, _AckAcc} = aec_trees:lookup_poi(accounts, AckPubkey, PoI),
    {ok, _ContractAcc} = aec_trees:lookup_poi(accounts, ContractPubKey, PoI),
    {ok, _ContractObj} = aec_trees:lookup_poi(contracts, ContractPubKey, PoI),

    ok = ?WS:unregister_test_for_channel_events(SenderConnPid, [sign, info, get, error]),
    ok = ?WS:unregister_test_for_channel_events(AckConnPid, [sign, info, get, error]),
    ok.

sc_ws_get_poi_(ConnPid, Scope, Config) ->
    ws_send_tagged(ConnPid, <<"channels.get.poi">>, Scope, Config),
    {ok, <<"poi">>, #{ channel_id := ChId
                     , data := #{<<"poi">> := Poi}}} =
        wait_for_channel_event_full(ConnPid, get, Config),
    {ok, #{ channel_id => ChId
          , poi => Poi}}.

contract_id_from_create_update(Owner, OffchainTx) ->
    {CB, Tx} = aetx:specialize_callback(OffchainTx),
    Round = CB:round(Tx),
    aect_contracts:compute_contract_pubkey(Owner, Round).


create_contract_(TestName, SenderConnPid, UpdateVolley, Config) ->
    EncodedCode = contract_byte_code(TestName),
    InitArgument = contract_create_init_arg(TestName),
    {ok, EncodedInitData} = encode_call_data(TestName, "init", InitArgument),

    ws_send_tagged(SenderConnPid, <<"channels.update.new_contract">>,
                   #{vm_version  => aect_test_utils:vm_version(),
                     abi_version => aect_test_utils:abi_version(),
                     deposit     => 10,
                     code        => EncodedCode,
                     call_data   => EncodedInitData}, Config),
    #{tx := UnsignedStateTx, updates := Updates} = UpdateVolley(),
    {UnsignedStateTx, Updates, contract_code(TestName)}.

contract_calls_(identity = TestName, ContractPubKey, SenderConnPid, UpdateVolley,
                AckConnPid, _ , _, Config) ->
    FunctionName = "main",
    Args = ["42"],
    ExpectedResult = 42,
    #{tx := UnsignedStateTx, updates := Updates} =
        call_a_contract(FunctionName, Args, ContractPubKey, TestName, SenderConnPid,
                    UpdateVolley, Config),
    ExpectedResult = dry_call_a_contract(FunctionName, Args, ContractPubKey,
                                         TestName, SenderConnPid, Config),
    DecodedCallResult = ws_get_decoded_result(SenderConnPid, AckConnPid,
                                              TestName, FunctionName,
                                              Updates, UnsignedStateTx, Config),
    {ExpectedResult, _} = {DecodedCallResult, ExpectedResult},
    {FunctionName, UnsignedStateTx, Updates};
contract_calls_(counter = TestName, ContractPubKey, SenderConnPid, UpdateVolley,
                AckConnPid, _ , _, Config) ->
    #{tx := UnsignedStateTx0, updates := Updates0} =
        call_a_contract("get", [], ContractPubKey, TestName, SenderConnPid,
                        UpdateVolley, Config),
    GetDecodedResult =
        fun(Tx, U) ->
            ws_get_decoded_result(SenderConnPid, AckConnPid, TestName,
                                  "get", U, Tx, Config)
        end,

    ExpectedInitResult = dry_call_a_contract("get", [], ContractPubKey,
                                             TestName, SenderConnPid, Config),
    InitResult = GetDecodedResult(UnsignedStateTx0, Updates0),
    {InitResult, InitResult} = {ExpectedInitResult, InitResult},
    call_a_contract("tick", [], ContractPubKey, TestName,
                    SenderConnPid, UpdateVolley, Config),

    #{tx := UnsignedStateTx1, updates := Updates1} =
        call_a_contract("get", [], ContractPubKey, TestName,
                        SenderConnPid, UpdateVolley, Config),

    #{tx := UnsignedStateTx2, updates := Updates2} =
        call_a_contract("get", [], ContractPubKey, TestName,
                        SenderConnPid, UpdateVolley, Config),

    ExpectedResult = InitResult + 1,
    DecodedCallResult = GetDecodedResult(UnsignedStateTx1, Updates1),
    DecodedCallResult = GetDecodedResult(UnsignedStateTx2, Updates2),
    {ExpectedResult, _} = {DecodedCallResult, ExpectedResult},
    {"get", UnsignedStateTx0, Updates0};
contract_calls_(spend_test = TestName, ContractPubKey, SenderConnPid, UpdateVolley,
                AckConnPid, SenderPubkey, AckPubkey, Config) ->
    GetBalance =
        fun(Args) ->
            FunName =
                case Args of
                    [] -> <<"get_balance">>;
                    _ -> <<"get_balance_of">>
                end,
                #{tx := UsStateTx, updates := Updates} =
                call_a_contract(FunName, Args, ContractPubKey, TestName,
                                SenderConnPid, UpdateVolley, Config),
            ws_get_decoded_result(SenderConnPid, AckConnPid, TestName, FunName,
                                  Updates, UsStateTx, Config)
        end,
    ContractBalance0 = GetBalance([]),

    SenderB0 = GetBalance(format_args(SenderPubkey)),
    AckB0 = GetBalance(format_args(AckPubkey)),

    SpendFun =
        fun(To, Amt) ->
            SpendArgs = format_args([To, Amt]),
            #{tx := _SpendStateTx, updates := _Updates2} =
                call_a_contract("spend", SpendArgs, ContractPubKey, TestName,
                                SenderConnPid, UpdateVolley, Config)
        end,

    SpendAmt = 3,
    SpendFun(SenderPubkey, SpendAmt),
    ContractBalance = GetBalance([]),
    {ContractBalance, _} = {ContractBalance0 - SpendAmt, ContractBalance0},
    SenderB = GetBalance(format_args(SenderPubkey)),
    AckB0 = GetBalance(format_args(AckPubkey)),
    SenderB = SenderB0 + SpendAmt,

    SpendAmt2 = 2,
    #{tx := UnsignedStateTx, updates := Updates} = SpendFun(AckPubkey, SpendAmt2),
    ContractBalance1 = GetBalance([]),
    {ContractBalance1, _} = {ContractBalance - SpendAmt2, ContractBalance1},
    SenderB = GetBalance(format_args(SenderPubkey)),
    AckB = GetBalance(format_args(AckPubkey)),
    AckB = AckB0 + SpendAmt2,
    {"spend", UnsignedStateTx, Updates}.

call_a_contract(Function, Argument, ContractPubKey, Contract,
                SenderConnPid, UpdateVolley, Config) ->
    call_a_contract(Function, Argument, ContractPubKey, Contract,
                    SenderConnPid, UpdateVolley, 0, Config).
call_a_contract(Function, Argument, ContractPubKey, Contract, SenderConnPid,
                UpdateVolley, Amount, Config) ->
    {ok, EncodedMainData} = encode_call_data(Contract, Function, Argument),
    CallOpts =
        #{contract    => aeser_api_encoder:encode(contract_pubkey, ContractPubKey),
          abi_version => aect_test_utils:abi_version(),
          amount      => Amount,
          call_data   => EncodedMainData},
    % invalid call
    ws_send_tagged(SenderConnPid, <<"channels.update.call_contract">>,
                   CallOpts#{amount => <<"1">>}, Config),
    {ok, #{<<"reason">> := <<"not_a_number">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    ws_send_tagged(SenderConnPid, <<"channels.update.call_contract">>,
                   CallOpts#{abi_version => <<"1">>}, Config),
    {ok, #{<<"reason">> := <<"not_a_number">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    ws_send_tagged(SenderConnPid, <<"channels.update.call_contract">>,
        CallOpts#{call_data => <<"ABCDEFG">>}, Config),
    {ok, #{<<"reason">> := <<"broken_encoding: bytearray">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    ws_send_tagged(SenderConnPid, <<"channels.update.call_contract">>,
        CallOpts#{contract => <<"ABCDEFG">>}, Config),
    {ok, #{<<"reason">> := <<"broken_encoding: contracts">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    % correct call
    ws_send_tagged(SenderConnPid, <<"channels.update.call_contract">>,
                   CallOpts, Config),
    #{tx := _UnsignedStateTx, updates := _Updates} = UpdateVolley().

dry_call_a_contract(Function, Argument, CPubKey, Contract, SenderConnPid, Config) ->
    dry_call_a_contract(Function, Argument, CPubKey, Contract, SenderConnPid, 0, Config).

dry_call_a_contract(Function, Argument, ContractPubKey, Contract, SenderConnPid, Amount, Config) ->
    {ok, EncodedMainData} = encode_call_data(Contract, Function, Argument),
    ok = ?WS:register_test_for_channel_event(SenderConnPid, dry_run),
    CallOpts =
        #{contract    => aeser_api_encoder:encode(contract_pubkey, ContractPubKey),
          abi_version => aect_test_utils:abi_version(),
          amount      => Amount,
          call_data   => EncodedMainData},
    % invalid call
    ws_send_tagged(SenderConnPid, <<"channels.dry_run.call_contract">>,
                   CallOpts#{amount => <<"1">>}, Config),
    {ok, #{<<"reason">> := <<"not_a_number">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    ws_send_tagged(SenderConnPid, <<"channels.dry_run.call_contract">>,
                   CallOpts#{abi_version => <<"1">>}, Config),
    {ok, #{<<"reason">> := <<"not_a_number">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    ws_send_tagged(SenderConnPid, <<"channels.dry_run.call_contract">>,
                   CallOpts, Config),
    ws_send_tagged(SenderConnPid, <<"channels.dry_run.call_contract">>,
                   CallOpts#{call_data => <<"ABCDEFG">>}, Config),
    {ok, #{<<"reason">> := <<"broken_encoding: bytearray">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    ws_send_tagged(SenderConnPid, <<"channels.dry_run.call_contract">>,
                   CallOpts#{contract => <<"ABCDEFG">>}, Config),
    {ok, #{<<"reason">> := <<"broken_encoding: contracts">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    {ok, <<"call_contract">>, CallRes} = wait_for_channel_event(SenderConnPid, dry_run, Config),
    ok = ?WS:unregister_test_for_channel_event(SenderConnPid, dry_run),
    #{<<"caller_id">>         := _CallerId,
      <<"caller_nonce">>      := CallRound,
      <<"contract_id">>       := _ContractId,
      <<"gas_price">>         := _,
      <<"gas_used">>          := _,
      <<"height">>            := CallRound,
      <<"return_type">>       := <<"ok">>,
      <<"return_value">>      := ReturnValue} = CallRes,
    decode_call_result(Contract, Function, ok, ReturnValue).

encode_call_data(Name, Fun, Args) when is_atom(Name) ->
    encode_call_data(contract_code(Name), Fun, Args);
encode_call_data(Src, Fun, Args) ->
    {ok, CallData} = aect_test_utils:encode_call_data(aect_test_utils:sophia_version(), Src, Fun, Args),
    {ok, aeser_api_encoder:encode(contract_bytearray, CallData)}.

contract_code(ContractName) ->
    {ok, BinSrc} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), ContractName),
    BinSrc.

contract_byte_code(ContractName) ->
    {ok, BinCode} = aect_test_utils:compile_contract(aect_test_utils:sophia_version(), ContractName),
    aeser_api_encoder:encode(contract_bytearray, BinCode).

get_contract_bytecode(ContractName) ->
    {ok, contract_byte_code(ContractName)}.

contract_return_type(_) ->
    <<"int">>.

contract_create_init_arg(identity) ->
    [];
contract_create_init_arg(counter) ->
    ["21"];
contract_create_init_arg(spend_test) ->
    [].

contract_result_parse(TestName, {ok, Data}) ->
    contract_result_parse(TestName, Data);
contract_result_parse(_TestName, Data) ->
    #{<<"type">> := <<"word">>, <<"value">> := DecodedCallResult} = Data,
    DecodedCallResult.

wait_for_signed_transaction_in_pool(SignedTx) ->
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    WaitForTx =
        fun Try(0) -> no_transaction;
            Try(Attempts) ->
                case tx_in_mempool(TxHash) of
                    true  -> ok;
                    false ->
                        timer:sleep(10),
                        Try(Attempts - 1)
                end
            end,
    ok = WaitForTx(30). % 30 attempts * 10ms

wait_for_signed_transaction_in_block(SignedTx) ->
    Node = aecore_suite_utils:node_name(?NODE),
    TxHash = aeser_api_encoder:encode(tx_hash, Hash = aetx_sign:hash(SignedTx)),
    case rpc:call(Node, aec_chain, find_tx_location, [Hash]) of
        BHash when is_binary(BHash) ->
            case rpc:call(Node, aec_chain, hash_is_in_main_chain, [BHash]) of
                true ->
                    ct:log("Tx is already on chain (BHash = ~p)", [BHash]),
                    ok;
                false ->
                    ct:log("Tx found in a block off-chain (BHash = ~p)", [BHash]),
                    wait_for_tx_hash_on_chain(TxHash)
            end;
        _Other ->
            ct:log("Current Tx location: ~p", [_Other]),
            wait_for_tx_hash_on_chain(TxHash)
    end.

wait_for_tx_hash_on_chain(TxHash) ->
    Node = aecore_suite_utils:node_name(?NODE),
    case rpc:call(Node, aec_chain, find_tx_location, [TxHash]) of
        BlockHash when is_binary(BlockHash) ->
            ct:log("TxHash is already on chain (~p)", [TxHash]),
            ok;
        _ ->
            Rate = aecore_suite_utils:expected_mine_rate(),
            Opts = #{strictly_follow_top => true},
            case aecore_suite_utils:mine_blocks_until_txs_on_chain(
                   aecore_suite_utils:node_name(?NODE), [TxHash], Rate, ?MAX_MINED_BLOCKS, Opts) of
                {ok, _Blocks} -> ok;
                {error, _Reason} -> did_not_mine
            end
    end.

sc_ws_timeout_open(Config) ->
    with_trace(fun sc_ws_timeout_open_/1, Config, "sc_ws_timeout_open").

sc_ws_timeout_open_(Config) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),

    IAmt = 8,
    RAmt = 4,

    ChannelOpts = channel_options(IPubkey, RPubkey, IAmt, RAmt,
                                  #{timeout_accept => 500}, Config),
    {ok, IConnPid} = channel_ws_start(initiator, maps:put(host, <<"localhost">>, ChannelOpts), Config),
    ok = ?WS:register_test_for_channel_event(IConnPid, info),
    ok = wait_for_channel_event(<<"died">>, IConnPid, info, Config),
    ok = ?WS:unregister_test_for_channel_event(IConnPid, info),
    ok.

sc_ws_attach_initiator(Config) ->
    #{initiator := #{pub_key  := Pubkey,
                     priv_key := Privkey}} = proplists:get_value(participants, Config),
    attach({Pubkey, Privkey}, "simple_auth", "authorize", [?SIMPLE_AUTH_GA_SECRET]),
    ok.

sc_ws_attach_responder(Config) ->
    #{responder := #{pub_key  := Pubkey,
                     priv_key := Privkey}} = proplists:get_value(participants, Config),
    attach({Pubkey, Privkey}, "simple_auth", "authorize", [?SIMPLE_AUTH_GA_SECRET]),
    ok.

sc_ws_min_depth_not_reached_timeout(Config) ->
    with_trace(fun sc_ws_min_depth_not_reached_timeout_/1, Config,
               "sc_ws_min_depth_not_reached_timeout").

sc_ws_min_depth_not_reached_timeout_(Config) ->
    S = ?SLOGAN,
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),

    IAmt = 70000 * aec_test_utils:min_gas_price(),
    RAmt = 40000 * aec_test_utils:min_gas_price(),
    %% Set the `timeout_funding_lock` to something short enough that the test case completes in
    %% reasonable time, but not so short that the fsms time out before the `channel_create` has
    %% time to collect the `on_chain_tx` notifications for the `create_tx`. Also, not so long
    %% that the 12 second default timeout in `wait_for_channel_event/4` triggers.
    ChannelOpts = channel_options(IPubkey, RPubkey, IAmt, RAmt,
                                  #{timeout_funding_lock => 3000,
                                    slogan => S}, Config),
    {ok, IConnPid} = channel_ws_start(initiator,
                                           maps:put(host, <<"localhost">>, ChannelOpts), Config),
    ok = ?WS:register_test_for_channel_events(IConnPid, [info, get, sign, on_chain_tx]),

    {ok, RConnPid} = channel_ws_start(responder, ChannelOpts, Config),

    ok = ?WS:register_test_for_channel_events(RConnPid, [info, get, sign, on_chain_tx]),

    channel_send_conn_open_infos(RConnPid, IConnPid, Config),

    channel_create(Config, IConnPid, RConnPid),

    % mine min depth - 1
    %% (but actually -2 since min_depth often adds one for extra measure)
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                       ?DEFAULT_MIN_DEPTH - 2),

    ok = wait_for_channel_event(<<"died">>, IConnPid, info, Config),
    ok = wait_for_channel_event(<<"died">>, RConnPid, info, Config),
    ok.


sc_ws_min_depth_is_modifiable(Config0) ->
    Config = sc_ws_open_(Config0, #{minimum_depth => ?DEFAULT_MIN_DEPTH},
                         ?DEFAULT_MIN_DEPTH),
    ok = sc_ws_update_(Config),
    ok = sc_ws_close_(Config).

sc_ws_basic_open_close(Config0) ->
    Config = sc_ws_open_(Config0),
    ok = sc_ws_update_(Config),
    ok = sc_ws_close_(Config).

sc_ws_basic_open_close_server(Config0) ->
    Config = sc_ws_open_([server_mode|Config0]),
    ok = sc_ws_update_(Config),
    ok = sc_ws_close_(Config).

sc_ws_failed_update(Config) ->
    ChannelClients = proplists:get_value(channel_clients, Config),
    Participants = proplists:get_value(participants, Config),
    lists:foreach(
        fun(Sender) ->
            LogPid = maps:get(Sender, ChannelClients),
            ?WS:log(LogPid, info, "Failing update, insufficient balance"),
            {ok, #{<<"reason">> := <<"insufficient_balance">>,
                  <<"request">> := _Request0}} = channel_update_fail(
                                                   ChannelClients, Sender,
                                                   Participants,
                                                   10000000 * aec_test_utils:min_gas_price(), % try sending too much
                                                   Config),
            ?WS:log(LogPid, info, "Failing update, negative amount"),
            {ok, #{<<"reason">> := <<"negative_amount">>,
                  <<"request">> := _Request1}} = channel_update_fail(
                                                   ChannelClients, Sender,
                                                   Participants, -1, Config),
            ?WS:log(LogPid, info, "Failing update, invalid pubkeys"),
            {ok, #{<<"reason">> := <<"invalid_pubkeys">>,
                  <<"request">> := _Request2}} =
                channel_update_fail(ChannelClients, Sender,
                                    #{initiator => #{pub_key => <<42:32/unit:8>>},
                                      responder => #{pub_key => <<43:32/unit:8>>}},
                                    1, Config),
            ok
        end,
        [initiator, responder]),
    ok.

sc_ws_generic_messages(Config) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),
    #{initiator := IConnPid, responder :=RConnPid}
        = proplists:get_value(channel_clients, Config),
    lists:foreach(
        fun({Sender, Msg}) ->
            {SenderPubkey, ReceiverPubkey, SenderPid, ReceiverPid} =
                case Sender of
                    initiator ->
                        {IPubkey, RPubkey, IConnPid, RConnPid};
                    responder ->
                        {RPubkey, IPubkey, RConnPid, IConnPid}
                end,
                SenderEncodedK = aeser_api_encoder:encode(account_pubkey, SenderPubkey),
                ReceiverEncodedK = aeser_api_encoder:encode(account_pubkey, ReceiverPubkey),
                ok = ?WS:register_test_for_channel_event(ReceiverPid, message),

                ws_send_tagged(SenderPid, <<"channels.message">>,
                        #{<<"to">> => ReceiverEncodedK,
                          <<"info">> => Msg}, Config),

                {ok, #{<<"message">> := #{<<"from">> := SenderEncodedK,
                                          <<"to">> := ReceiverEncodedK,
                                          <<"info">> := Msg}}}
                    = wait_for_channel_event(ReceiverPid, message, Config),
                ok = ?WS:unregister_test_for_channel_event(ReceiverPid, message)
        end,
      [ {initiator, <<"hejsan">>}                   %% initiator can send
      , {responder, <<"svejsan">>}                  %% responder can send
      , {initiator, <<"first message in a row">>}   %% initiator can send two messages in a row
      , {initiator, <<"second message in a row">>}
      , {responder, <<"some message">>}             %% responder can send two messages in a row
      , {responder, <<"other message">>}
      ]),
    ok.

sc_ws_update_conflict(Config) ->
    Participants = proplists:get_value(participants, Config),
    ChannelClients = proplists:get_value(channel_clients, Config),
    lists:foreach(
        fun(FirstSender) ->
                channel_conflict(ChannelClients, FirstSender, Participants, 1, 2,
                                 Config)
        end,
        [initiator,
         responder]),
    ok.

sc_ws_close_mutual(Config) ->
    with_trace(fun sc_ws_close_mutual_/1, Config, "sc_ws_close_mutual").

sc_ws_close_mutual_(Config0) ->
    lists:foreach(
        fun(WhoCloses) ->
            Config = sc_ws_open_(Config0),
            sc_ws_close_mutual_(Config, WhoCloses)
        end,
        [initiator,
         responder]).

sc_ws_close_solo(Config) ->
    with_trace(fun sc_ws_close_solo_/1, Config, "sc_ws_close_solo").

sc_ws_close_solo_(Config0) ->
    lists:foreach(
      fun(WhoCloses) ->
              S = ?SLOGAN(WhoCloses),
              Config = sc_ws_open_(Config0, #{slogan => S}),
              sc_ws_close_solo_(Config, WhoCloses)
      end, [initiator, responder]).

sc_ws_slash(Config) ->
    with_trace(fun sc_ws_slash_/1, Config, "sc_ws_slash").

sc_ws_slash_(Config0) ->
    lists:foreach(
      fun({WhoCloses, WhoSlashes, WhoSettles}) ->
              S = ?SLOGAN([WhoCloses, ",", WhoSlashes]),
              Config = sc_ws_open_(Config0, #{slogan => S}),
              sc_ws_slash_(Config, WhoCloses, WhoSlashes, WhoSettles)
      end, [{A,B, C} || A <- [initiator],
                        B <- [initiator, responder],
                        C <- [initiator]]).

sc_ws_slash_(Config, WhoCloses, WhoSlashes, WhoSettles) ->
    ct:log("WhoCloses  = ~p~n"
           "WhoSlashes = ~p~n"
           "WhoSettles = ~p~n", [WhoCloses, WhoSlashes, WhoSettles]),
    true = lists:member(WhoCloses, ?ROLES),
    true = lists:member(WhoSlashes, ?ROLES),
    ct:log("ConfigList = ~p", [Config]),
    #{initiator := #{pub_key  := IPubKey},
      responder := #{pub_key  := RPubKey}} = Participants =
        proplists:get_value(participants, Config),
    #{initiator := IConnPid,
      responder := RConnPid} = Conns =
        proplists:get_value(channel_clients, Config),
    MyEvents = [sign, info, get, on_chain_tx],
    Pids = [IConnPid, RConnPid],
    ok = register_channel_events(MyEvents, Pids),
    %% Fetch ChId and POI for initial state
    PoiScope = #{accounts => [aeser_api_encoder:encode(account_pubkey, Acc)
                              || Acc <- [IPubKey, RPubKey]]},
    {ok, #{ channel_id := ChIdEnc
          , poi := PoiEnc}} = sc_ws_get_poi_(IConnPid, PoiScope, Config),
    {channel, ChId} = aeser_api_encoder:decode(ChIdEnc),
    {poi, PoiSer} = aeser_api_encoder:decode(PoiEnc),
    Poi = aec_trees:deserialize_poi(PoiSer),
    %% create a new offchain state

    avoid_double_reg(
      MyEvents, Pids,
      fun() ->
              channel_update(Conns, initiator, Participants, 1, 2,
                             _TestErrors = false, Config)
      end),
    sc_ws_cheating_close_solo_(Config, ChId, Poi, WhoCloses),
    %%
    %% Both sides detect slash potential
    %%
    SlasherPid = maps:get(WhoSlashes, Conns),
    SlasherOtherPid = maps:get(other(WhoSlashes), Conns),
    {ok, #{ <<"info">> := <<"can_slash">>
          , <<"type">> := <<"channel_offchain_tx">> }}
        = wait_for_channel_event(SlasherPid, on_chain_tx, Config),
    {ok, #{ <<"info">> := <<"can_slash">>
          , <<"type">> := <<"channel_offchain_tx">> }}
        = wait_for_channel_event(SlasherOtherPid, on_chain_tx, Config),
    {ok, #{<<"event">> := <<"closing">>}}
        = wait_for_channel_event(SlasherPid, info, Config),
    {ok, #{<<"event">> := <<"closing">>}}
        = wait_for_channel_event(SlasherOtherPid, info, Config),
    %%
    %% WhoSlashes initiates a slash
    %%
    {ok, <<"ok">>} = request_slash(SlasherPid),
    {ok, SignedSlashTx} = sign_slash_tx(SlasherPid, WhoSlashes, Config),
    ct:log("SignedSlashTx = ~p", [SignedSlashTx]),
    SlashTxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedSlashTx)),
    ct:log("SlashTxHash = ~p", [SlashTxHash]),
    ok = wait_for_tx_hash_on_chain(SlashTxHash),
    %%
    %% Both sides detect slash tx
    %%
    {ok, #{ <<"info">> := <<"solo_closing">>
          , <<"type">> := <<"channel_slash_tx">> }}
        = wait_for_channel_event(SlasherPid, on_chain_tx, Config),
    {ok, #{ <<"info">> := <<"solo_closing">>
          , <<"type">> := <<"channel_slash_tx">> }}
        = wait_for_channel_event(SlasherOtherPid, on_chain_tx, Config),

    settle_(Config, WhoSettles),
    ok.

other(initiator) -> responder;
other(responder) -> initiator.

sign_slash_tx(ConnPid, Who, Config) ->
    Participants = proplists:get_value(participants, Config),
    #{priv_key := PrivKey} = maps:get(Who, Participants),
    sign_tx(ConnPid, <<"slash_tx">>, <<"channels.slash_sign">>, PrivKey, Config).

sign_tx(ConnPid, Tag, ReplyMethod, PrivKey, Config) ->
    {ok, Tag, #{<<"signed_tx">> := EncSTx}} =
        wait_for_channel_event(ConnPid, sign, Config),
    {ok, BinSTx} = aeser_api_encoder:safe_decode(transaction, EncSTx),
    STx = aetx_sign:deserialize_from_binary(BinSTx),
    SignedTx = aec_test_utils:co_sign_tx(STx, PrivKey),
    EncSignedTx = aeser_api_encoder:encode(
                    transaction,
                    aetx_sign:serialize_to_binary(SignedTx)),
    ws_send_tagged(ConnPid, ReplyMethod, #{signed_tx => EncSignedTx}, Config),
    {ok, SignedTx}.

register_channel_events(Events, Pids) ->
    [ok = ?WS:register_test_for_channel_events(Pid, Events)
     || Pid <- Pids],
    ok.

unregister_channel_events(Events, Pids) ->
    [ok = ?WS:unregister_test_for_channel_events(Pid, Events)
     || Pid <- Pids],
    ok.

avoid_double_reg(Events, Pids, F) ->
    ok = unregister_channel_events(Events, Pids),
    Res = F(),
    ok = register_channel_events(Events, Pids),
    Res.

sc_ws_cheating_close_solo_(Config, ChId, Poi, WhoCloses) ->
    %% Create a close_solo based on the create_tx
    Participants = proplists:get_value(participants, Config),
    Keys = maps:get(WhoCloses, Participants),
    #{pub_key := PubKey, priv_key := PrivKey} = Keys,
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [PubKey]),
    TTL = current_height() + 10,
    Fee = 30000 * aec_test_utils:min_gas_price(),
    ct:log("Will try to create close_solo_tx~n"
           "ChId = ~p~n"
           "PubKey = ~p~n", [ChId, PubKey]),
    TxOpts = #{ channel_id => aeser_id:create(channel, ChId)
              , from_id    => aeser_id:create(account, PubKey)
              , payload    => <<>>
              , poi        => Poi
              , ttl        => TTL
              , fee        => Fee
              , nonce      => Nonce
              },
    ct:log("close_solo_tx: ~p", [TxOpts]),
    {ok, Tx} = aesc_close_solo_tx:new(TxOpts),
    SignedTx = aec_test_utils:sign_tx(Tx, [PrivKey]),
    ok = rpc(dev1, aec_tx_pool, push, [SignedTx]),
    wait_for_signed_transaction_in_block(SignedTx),
    ok.

sc_ws_leave_reestablish(Config0) ->
    Config = sc_ws_open_(Config0),
    ReestablOptions = sc_ws_leave_(Config),
    Config1 = sc_ws_reestablish_(ReestablOptions, Config),
    ok = sc_ws_update_(Config1),
    ok = sc_ws_close_(Config1).

sc_ws_ping_pong(Config) ->
    #{initiator := IConnPid, responder :=RConnPid} =
        proplists:get_value(channel_clients, Config),
    PingPong =
        fun(ConnPid) ->
            ok = ?WS:register_test_for_channel_events(ConnPid, [system]),
            ok = ws_send_tagged(ConnPid, <<"channels.system">>,
                                #{<<"action">> => <<"ping">>}, Config),
            {ok, <<"pong">>, #{}} =
                wait_for_channel_event(ConnPid, system, Config),
            ok = ?WS:unregister_test_for_channel_events(ConnPid, [system]),
            ok
        end,
    PingPong(IConnPid),
    PingPong(RConnPid),
    ok.

sc_ws_deposit(Config) ->
    lists:foreach(
        fun(Depositor) ->
            sc_ws_deposit_(Config, Depositor),
            ok
        end,
        [initiator, responder]).

sc_ws_withdraw(Config) ->
    lists:foreach(
        fun(Depositor) ->
            sc_ws_withdraw_(Config, Depositor),
            ok
        end,
        [initiator, responder]).


%% channel_options(IPubkey, RPubkey, IAmt, RAmt) ->
%%     channel_options(IPubkey, RPubkey, IAmt, RAmt, #{}).

channel_options(IPubkey, RPubkey, IAmt, RAmt, Other, Config) ->
    maps:merge(#{ port => ?config(ws_port, Config),
                  initiator_id => aeser_api_encoder:encode(account_pubkey, IPubkey),
                  responder_id => aeser_api_encoder:encode(account_pubkey, RPubkey),
                  lock_period => 10,
                  push_amount => 1,
                  initiator_amount => IAmt,
                  responder_amount => RAmt,
                  channel_reserve => 2,
                  protocol => sc_ws_protocol(Config)
                }, Other).

current_height() ->
    case rpc(aec_chain, top_header, []) of
        undefined -> 1;
        Header -> aec_headers:height(Header) + 1
    end.

format_args(Xs) when is_list(Xs) -> lists:map(fun format_arg/1, Xs);
format_args(X)                   -> [format_arg(X)].

format_arg(B) when is_binary(B)  -> aeser_api_encoder:encode(account_pubkey, B);
format_arg(I) when is_integer(I) -> integer_to_list(I).

%% ============================================================
%% HTTP Requests
%% ============================================================

get_contract_create(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/create", Data).

get_contract_call_object(TxHash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/"++binary_to_list(TxHash)++"/info", []).

get_pending_transactions() ->
    Host = internal_address(),
    http_request(Host, get, "debug/transactions/pending", []).

post_spend_tx(RecipientId, Amount, Fee) ->
    {_, Sender} = aecore_suite_utils:sign_keys(?NODE),
    SenderId = aeser_api_encoder:encode(account_pubkey, Sender),
    post_spend_tx(SenderId, RecipientId, Amount, Fee, <<"foo">>).

post_spend_tx(SenderId, RecipientId, Amount, Fee, Payload) ->
    Host = internal_address(),
    http_request(Host, post, "debug/transactions/spend",
                 #{sender_id => SenderId,
                   recipient_id => RecipientId,
                   amount => Amount,
                   fee => Fee,
                   payload => Payload}).

get_pubkey() ->
    {_, Pubkey} = aecore_suite_utils:sign_keys(?NODE),
    aeser_api_encoder:encode(account_pubkey, Pubkey).

%% ============================================================
%% private functions
%% ============================================================
channel_ws_host_and_port() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"websocket">>, <<"channel">>, <<"port">>],
                aehttp, [channel, websocket, port], 8045]),
    {"localhost", Port}.

channel_ws_start(Role, Opts, Config) ->
    LogFile = docs_log_file(Config),
    Opts1 = Opts#{ {int,logfile} => LogFile },
    {Host, Port} = channel_ws_host_and_port(),
    ?WS:start_channel(Host, Port, Role, Opts1).

docs_log_file(Config) ->
    %% TCLogBase = atom_to_list(?config(tc_name, Config)),
    TCLogBase = log_basename(Config),
    {aehttp_sc_SUITE, TestName} = proplists:get_value(current, ct:get_status()),
    MsgLogFile = filename:join([?config(priv_dir, Config), TCLogBase,
                               atom_to_list(TestName)++ ".md"]),
    ct:log("MsgLogFile = ~p", [MsgLogFile]),
    MsgLogFile.

log_basename(Config) ->
    Protocol = binary_to_list(?config(sc_ws_protocol, Config)),
    GOpts = ?config(tc_group_properties, Config),
    GName = ?config(name, GOpts),
    SubDir =
        case GName =:= continuous_sc_ws of
            true -> filename:join(Protocol, "continuous");
            false -> Protocol
        end,
    filename:join("channel_docs", SubDir).

sign_and_post_tx(EncodedUnsignedTx) ->
    sign_and_post_tx(EncodedUnsignedTx, on_node).

sign_and_post_tx(EncodedUnsignedTx, PrivKey) ->
    %% Check that we get the correct hash
    {ok, 200, #{<<"tx_hash">> := TxHash}} = sign_and_post_tx_(EncodedUnsignedTx, PrivKey),
    %% Check tx is in mempool.
    Fun = fun() ->
                  tx_in_mempool(TxHash)
          end,
    {ok, true} = aec_test_utils:wait_for_it_or_timeout(Fun, true, 5000),
    TxHash.

sign_and_post_tx_(EncodedUnsignedTx, PrivKey) ->
    {ok, SerializedUnsignedTx} = aeser_api_encoder:safe_decode(transaction, EncodedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    {ok, SignedTx} =
        case PrivKey =:= on_node of
            true  -> aecore_suite_utils:sign_on_node(?NODE, UnsignedTx);
            false -> {ok, aec_test_utils:sign_tx(UnsignedTx, PrivKey)}
        end,
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    post_transactions_sut(aeser_api_encoder:encode(transaction, SerializedTx)).

tx_in_mempool(TxHash) ->
    case get_transactions_by_hash_sut(TxHash) of
        {ok, 200, #{<<"block_hash">> := <<"none">>}} -> true;
        {ok, 200, #{<<"block_hash">> := Other}} ->
            ct:log("Tx not in mempool, but in chain: ~p", [Other]),
            false;
        {ok, 404, _} -> false
    end.

tx_in_chain(TxHash) ->
    case get_transactions_by_hash_sut(TxHash) of
        {ok, 200, #{<<"block_hash">> := <<"none">>}} ->
            ct:log("Tx not mined, but in mempool"),
            false;
        {ok, 200, #{<<"block_hash">> := _}} -> true;
        {ok, 404, _} -> false
    end.

ws_get_decoded_result(ConnPid1, ConnPid2, Contract, Function, [Update], UnsignedTx, Config) ->
    %% helper lambda for decoded result
    GetCallResult =
        fun(ConnPid) ->
            GetParams = ws_get_call_params(Update, UnsignedTx),
            ws_send_tagged(ConnPid, <<"channels.get.contract_call">>,
                           GetParams#{round => <<"2">>}, Config),
            {ok, #{<<"reason">> := <<"not_a_number">>}} =
                wait_for_channel_event(ConnPid, error, Config),
            ws_send_tagged(ConnPid, <<"channels.get.contract_call">>,
                GetParams#{caller => <<"ABCEDFG">>}, Config),
            {ok, #{<<"reason">> := <<"broken_encoding: accounts">>}} =
                wait_for_channel_event(ConnPid, error, Config),
            ws_send_tagged(ConnPid, <<"channels.get.contract_call">>,
                GetParams#{contract => <<"ABCDEFG">>}, Config),
            {ok, #{<<"reason">> := <<"broken_encoding: contracts">>}} =
                wait_for_channel_event(ConnPid, error, Config),
            ws_send_tagged(ConnPid, <<"channels.get.contract_call">>,
                           GetParams, Config),
            {ok, <<"contract_call">>, Res} = wait_for_channel_event(ConnPid, get, Config),
            Res
        end,
    CallRes = GetCallResult(ConnPid1),
    CallRes = GetCallResult(ConnPid2),
    #{<<"caller_id">>         := _CallerId,
      <<"caller_nonce">>      := CallRound,
      <<"contract_id">>       := _ContractId,
      <<"gas_price">>         := _,
      <<"gas_used">>          := _,
      <<"height">>            := CallRound,
      <<"return_type">>       := <<"ok">>,
      <<"return_value">>      := ReturnValue} = CallRes,
    decode_call_result(Contract, Function, ok, ReturnValue).

decode_call_result(ContractName, Fun, ResType, ResValue) ->
    {ok, BinCode} = aect_test_utils:read_contract(?SOPHIA_LIMA_AEVM, ContractName),
    aect_test_utils:decode_call_result(binary_to_list(BinCode), Fun, ResType, ResValue).


ws_get_call_params(Update, UnsignedTx) ->
    {CB1, Tx1} = aetx:specialize_callback(UnsignedTx),
    CallRound = CB1:round(Tx1),
    CallerPubKey = extract_caller(Update),
    ContractPubKey = extract_contract_pubkey(Update),
    CallerId = aeser_api_encoder:encode(account_pubkey, CallerPubKey),
    ContractId = aeser_api_encoder:encode(contract_pubkey, ContractPubKey),
    #{contract   => ContractId,
      caller     => CallerId,
      round      => CallRound}.

%% wait_for_channel_msg(ConnPid, Action, Config) ->
%%     wait_for_channel_msg_(ConnPid, Action, sc_ws_protocol(Config)).

%% wait_for_channel_msg_(ConnPid, Action, <<"json-rpc">>) ->
%%     wait_for_channel_event_(ConnPid, Action, <<"json-rpc">>).

wait_for_channel_event(ConnPid, Action, Config) ->
    case wait_for_channel_event_(ConnPid, Action, sc_ws_protocol(Config)) of
        {ok, #{error := Error}} ->
            {ok, Error};
        {ok, #{data := Data}} ->
            {ok, Data};
        {ok, Tag, #{data := Data}} ->
            {ok, Tag, Data}
    end.

wait_for_channel_event_full(ConnPid, Action, Config) ->
    wait_for_channel_event_(ConnPid, Action, sc_ws_protocol(Config)).

wait_for_channel_event_(ConnPid, error, <<"json-rpc">>) ->
    case ?WS:wait_for_channel_msg(ConnPid, error) of   % whole msg
        {ok, #{ <<"jsonrpc">> := <<"2.0">>
              , <<"channel_id">> := ChId
              , <<"id">>      := null
              , <<"error">>   := E } } ->
            {ok, #{ channel_id => ChId
                  , error      => lift_reason(E)}}
    end;
wait_for_channel_event_(ConnPid, Action, <<"json-rpc">>) ->
    Method = method_pfx(Action),
    Sz = byte_size(Method),
    case {?WS:wait_for_channel_msg(ConnPid, Action), Method} of   % whole msg
        {{ok, #{ <<"jsonrpc">> := <<"2.0">>
               , <<"method">>  := <<Method:Sz/binary, _/binary>>
               , <<"params">>  := #{<<"channel_id">> := ChId} = Params }}, _} ->
            Data = maps:get(<<"data">>, Params, no_data),
            {ok, #{channel_id => ChId, data => Data}};
        {{ok, Tag, #{ <<"jsonrpc">> := <<"2.0">>
                    , <<"method">>  := <<Method:Sz/binary, _/binary>>
                    , <<"params">>  := #{<<"channel_id">> := ChId} = Params }}, _} ->
            Data = maps:get(<<"data">>, Params, no_data),
            {ok, Tag, #{channel_id => ChId, data => Data}}
    end.

wait_for_channel_event(Event, ConnPid, Type, Config) ->
    wait_for_channel_event_(Event, ConnPid, Type, sc_ws_protocol(Config)).

wait_for_channel_event_(Event, ConnPid, Action, <<"json-rpc">>) ->
    {ok, #{ <<"data">> := #{ <<"event">> := Event } }} =
        wait_for_json_rpc_action(ConnPid, Action),
    ok.

match_info(Info, Match) ->
    maps:fold(fun(K,V,Acc) ->
                      case maps:find(K, Info) of
                          {ok, V} ->
                              Acc;
                          {ok, V1} when is_map(V), is_map(V1) ->
                              match_info(V, V1);
                          {ok, Other} ->
                              error({info_mismatch, {K, [V, Other]}});
                          error ->
                              error({no_such_key, K})
                      end
              end, ok, Match).


wait_for_channel_leave_msg(ConnPid, Config) ->
    wait_for_channel_leave_msg_(ConnPid, sc_ws_protocol(Config)).

wait_for_channel_leave_msg_(ConnPid, <<"json-rpc">>) ->
    {ok, #{ <<"channel_id">> := ChId,
            <<"data">> := #{ <<"state">> := St } }} =
        wait_for_json_rpc_action(ConnPid, leave),
    {ok, #{id => ChId, state => St}}.

wait_for_json_rpc_action(ConnPid, Action) ->
    Method0 = method_pfx(Action),
    Sz = byte_size(Method0),
    {ok, #{ <<"jsonrpc">> := <<"2.0">>
          , <<"method">>  := <<Method0:Sz/binary, _/binary>>
          , <<"params">>  := #{<<"channel_id">> := _} = Params }} =
        ?WS:wait_for_channel_msg(ConnPid, Action),
    {ok, Params}.

lift_reason(#{ <<"message">> := <<"Rejected">>
             , <<"data">>    := Data } = E) ->
    Codes = lists:sort([Code || #{<<"code">> := Code} <- Data]),
    E#{ <<"reason">> => data_code_to_reason(Codes) };
lift_reason(#{ <<"code">> := Code } = E) ->
    E#{ <<"reason">> => data_code_to_reason([Code]) };
lift_reason(#{ <<"reason">> := _ } = E) ->
    E.

data_code_to_reason([100 ])      -> <<"not_found">>;
data_code_to_reason([107 ])      -> <<"conflict">>;
data_code_to_reason([1001])      -> <<"insufficient_balance">>;
data_code_to_reason([1002])      -> <<"negative_amount">>;
data_code_to_reason([1003])      -> <<"invalid_pubkeys">>;
data_code_to_reason([1004])      -> <<"call_not_found">>;
data_code_to_reason([1005])      -> <<"broken_encoding: accounts">>;
data_code_to_reason([1006])      -> <<"broken_encoding: contracts">>;
data_code_to_reason([1007])      -> <<"contract_init_failed">>;
data_code_to_reason([1008])      -> <<"not_a_number">>;
data_code_to_reason([1005,1006]) -> <<"broken_encoding: accounts, contracts">>;
data_code_to_reason([1009])      -> <<"broken_encoding: bytearray">>;
data_code_to_reason([1010])      -> <<"broken_encoding: transaction">>;
data_code_to_reason([Code])      -> sc_ws_api_jsonrpc:error_data_msg(Code).

method_pfx(Action) ->
    <<"channels.", (bin(Action))/binary>>.

ws_send_tagged(ConnPid, Method, Payload, Config) ->
    ws_send_tagged_(ConnPid, Method, Payload, sc_ws_protocol(Config)).

ws_call_async_method(ConnPid, Method, Payload, Config) ->
    ws_call_async_method(ConnPid, Method, Payload, sc_ws_protocol(Config), Config).

ws_call_async_method(ConnPid, Method, Payload, <<"json-rpc">>, _Config) ->
    <<"ok">> = ?WS:json_rpc_call(
                  ConnPid, #{ <<"method">> => Method
                            , <<"params">> => Payload }),
    ok.


ws_send_tagged_(ConnPid, Method, Payload, <<"json-rpc">>) ->
    ?WS:json_rpc_notify(
       ConnPid,
       #{ <<"method">> => Method
        , <<"params">> => Payload }).

bin(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
bin(B) when is_binary(B) ->
    B.

sc_ws_protocol(Config) ->
    {_, Protocol} = lists:keyfind(sc_ws_protocol, 1, Config),
    Protocol.

sc_ws_wrong_call_data(Config) ->
    [sc_ws_contract_generic_(Role, offchain, fun sc_ws_broken_init_code_/9, Config, [])
        || Role <- [initiator, responder]],
    [sc_ws_contract_generic_(Role, offchain, fun sc_ws_broken_call_code_/9, Config, [])
        || Role <- [initiator, responder]],
    ok.

sc_ws_broken_init_code_(Owner, GetVolley, _CreateContract, _ConnPid1, _ConnPid2,
                   _OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    %% Example broken init code will be calling not the init function
    {ok, EncodedCode} = get_contract_bytecode(identity),
    %% call main instead of init
    {ok, EncodedInitData} = encode_call_data(identity, "main", ["1"]),
    {_CreateVolley, OwnerConnPid, _OwnerPubKey} = GetVolley(Owner),
    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract">>,
                   #{vm_version  => aect_test_utils:vm_version(),
                     abi_version => aect_test_utils:abi_version(),
                     deposit     => 10,
                     code        => EncodedCode,
                     call_data   => EncodedInitData}, Config),

    {ok, ErrPayload} = wait_for_channel_event(OwnerConnPid, error, Config),
    #{<<"reason">> := <<"contract_init_failed">>} = lift_reason(ErrPayload),
    ok.

sc_ws_broken_call_code_(Owner, GetVolley, _CreateContract, _ConnPid1, _ConnPid2,
                   _OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    %% Example broken call code data will be calling a function from another
    %% contract
    {ok, EncodedCode} = get_contract_bytecode(identity),
    {ok, EncodedInitData} = encode_call_data(identity, "init", []),

    {SignVolley, OwnerConnPid, OwnerPubKey} = GetVolley(Owner),
    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract">>,
                   #{vm_version  => aect_test_utils:vm_version(),
                     abi_version => aect_test_utils:abi_version(),
                     deposit     => 10,
                     code        => EncodedCode,
                     call_data   => EncodedInitData}, Config),
    #{tx := UnsignedCreateTx, updates := _Updates} = SignVolley(),
    % contract is succesfully created
    ContractPubKey = contract_id_from_create_update(OwnerPubKey,
                                                    UnsignedCreateTx),

    % have some other contract with some other function
    {ok, EncodedCalcCallData} = encode_call_data(safe_math, "add", ["1", "2"]),
    % call the existing contract with the other contract's call data
    ws_send_tagged(OwnerConnPid, <<"channels.update.call_contract">>,
                   #{contract    => aeser_api_encoder:encode(contract_pubkey, ContractPubKey),
                     abi_version => aect_test_utils:abi_version(),
                     amount      => 1,
                     call_data   => EncodedCalcCallData}, Config),
    % contract call succeeds executing
    #{tx := UnsignedCallTx, updates := [Update]} = SignVolley(),
    ws_send_tagged(OwnerConnPid, <<"channels.get.contract_call">>,
                   ws_get_call_params(Update, UnsignedCallTx), Config),
    % contract call is present in FSM
    {ok, <<"contract_call">>, Res} = wait_for_channel_event(OwnerConnPid, get, Config),
    % call result is still an error
    #{<<"return_type">> := <<"error">>} = Res,
    ok.

extract_caller(#{<<"op">> := <<"OffChainNewContract">>,
                 <<"owner">> := EncOwnerId}) ->
    {ok, Owner} = aeser_api_encoder:safe_decode(account_pubkey, EncOwnerId),
    Owner;
extract_caller(#{<<"op">> := <<"OffChainCallContract">>,
                 <<"caller">> := EncCallerId}) ->
    {ok, Caller} = aeser_api_encoder:safe_decode(account_pubkey, EncCallerId),
    Caller.

extract_contract_pubkey(#{<<"op">> := <<"OffChainCallContract">>,
                          <<"contract">> := EncContractId}) ->
    {ok, ContractPubKey} = aeser_api_encoder:safe_decode(contract_pubkey, EncContractId),
    ContractPubKey.

with_trace(F, Config, File) ->
    with_trace(F, Config, File, on_error).

with_trace(F, Config, File, When) ->
    ct:log("with_trace ...", []),
    TTBRes = aesc_ttb:on_nodes([node()|get_nodes()], File),
    ct:log("Trace set up: ~p", [TTBRes]),
    try F(Config)
    catch
	error:R ->
	    Stack = erlang:get_stacktrace(),
	    ct:pal("Error ~p~nStack = ~p", [R, Stack]),
	    ttb_stop(),
	    erlang:error(R);
	exit:R ->
	    Stack = erlang:get_stacktrace(),
	    ct:pal("Exit ~p~nStack = ~p", [R, Stack]),
	    ttb_stop(),
	    exit(R);
        throw:Res ->
            ct:pal("Caught throw:~p", [Res]),
            throw(Res)
    end,
    case When of
        on_error ->
            ct:log("Discarding trace", []),
            aesc_ttb:stop_nofetch();
        always ->
            ttb_stop()
    end,
    ok.

ttb_stop() ->
    Dir = aesc_ttb:stop(),
    Out = filename:join(filename:dirname(Dir),
			filename:basename(Dir) ++ ".txt"),
    case aesc_ttb:format(Dir, Out, #{limit => 30000}) of
        {error, Reason} ->
            ct:pal("TTB formatting error: ~p", [Reason]);
        _ ->
            ok
    end,
    ct:pal("Formatted trace log in ~s~n", [Out]).

get_nodes() ->
    [aecore_suite_utils:node_name(?NODE)].

attach({Owner, OwnerPrivkey}, Contract, AuthFun, Args) ->
   attach({Owner, OwnerPrivkey}, Contract, AuthFun, Args, #{}).


attach({Owner, OwnerPrivkey}, Contract, AuthFun, Args, Opts) ->
   case aega_test_utils:get_contract(Contract) of
       {ok, #{src := Src, bytecode := C, map := #{type_info := TI}}} ->
           attach_({Owner, OwnerPrivkey}, Src, C, TI, AuthFun, Args, Opts);
       _ ->
           error(bad_contract)
   end.

attach_({Owner, OwnerPrivkey}, Src, ByteCode, TypeInfo, AuthFun, Args, Opts) ->
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [Owner]),
    Calldata = aega_test_utils:make_calldata(Src, "init", Args),
    FunName = list_to_binary(AuthFun),
    {ok, AuthFunHash} =
        case aect_test_utils:backend() of
            aevm -> aeb_aevm_abi:type_hash_from_function_name(FunName, TypeInfo);
            fate ->{ok, <<(aeb_fate_code:symbol_identifier(FunName)):4/binary, 0:(28*8)>>}
        end,
    Options1 = maps:merge(#{nonce => Nonce, code => ByteCode,
                            auth_fun => AuthFunHash, call_data => Calldata},
                            Opts),
    AttachTx = aega_test_utils:ga_attach_tx(Owner, Options1),

    SignedTx = aec_test_utils:sign_tx(AttachTx, [OwnerPrivkey]),
    ok = rpc(aec_tx_pool, push, [SignedTx]),
    wait_for_signed_transaction_in_block(SignedTx),
    ok.

slogan(F, L) ->
    S = iolist_to_binary([?MODULE_STRING, ":", atom_to_binary(F, latin1),
                          "/", integer_to_binary(L)]),
    ct:log("slogan: ~p", [S]),
    S.

slogan(F, L, X) ->
    S = iolist_to_binary([?MODULE_STRING, ":", atom_to_binary(F, latin1),
                          "/", integer_to_binary(L), "[", to_binary(X), "]"]),
    ct:log("slogan: ~p", [S]),
    S.

to_binary(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
to_binary(I) when is_integer(I) ->
    integer_to_binary(I);
to_binary([_|_] = L) ->
    iolist_to_binary([to_binary(X) || X <- L]).

simple_auth_meta(Owner, Secret, GANonce, InnerMostTx, InnerTx) ->
    AuthTx =
        case aetx:specialize_type(InnerMostTx) of
            {channel_offchain_tx, _} -> InnerMostTx;
            {_, _} -> InnerTx
        end,
    AuthData = simple_auth(Secret, GANonce, AuthTx),
    meta(Owner, AuthData, InnerTx).

simple_auth(Secret, Nonce, _Tx) ->
    aega_test_utils:make_calldata("simple_auth", "authorize", [Secret, Nonce]).

meta(Owner, AuthData, InnerTx) ->
    aecore_suite_utils:meta_tx(Owner, #{}, AuthData, InnerTx).

account_type(Pubkey) ->
    {value, Account} = rpc(aec_chain, get_account, [Pubkey]),
    aec_accounts:type(Account).

