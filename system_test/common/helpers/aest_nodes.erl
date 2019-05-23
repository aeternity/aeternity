-module(aest_nodes).

-include_lib("stdlib/include/assert.hrl").

%=== EXPORTS ===================================================================

%% Common Test API exports
-export([ct_setup/1]).
-export([ct_cleanup/1]).

%% QuickCheck API exports
-export([eqc_setup/2]).
-export([eqc_cleanup/1]).

%% Generic API exports
-export([setup_nodes/2]).
-export([start_node/2]).
-export([stop_node/3]).
-export([kill_node/2]).
-export([stop_container/3]).
-export([extract_archive/4]).
-export([run_cmd_in_node_dir/4]).
-export([connect_node/3]).
-export([disconnect_node/3]).
-export([get_service_address/3]).
-export([get_internal_address/3]).
-export([get_node_pubkey/2]).
-export([export/3]).
-export([read_metric/3]).
-export([gas_price/0]).


%% Helper function exports
-export([shared_temp_file/2]).
-export([read_last_metric/2]).
-export([cluster/2]).
-export([spec/3]).
-export([request/3]).
-export([request/4]).
-export([get_node_config/2]).
-export([get/5]).
-export([get_status/1]).
-export([get_block/2]).
-export([get_top/1]).
-export([get_mempool/1]).
-export([get_account/2]).
-export([get_channel/2]).
-export([post_spend_tx/5]).
-export([post_create_state_channel_tx/4,
         post_close_mutual_state_channel_tx/5,
         post_withdraw_state_channel_tx/5,
         post_deposit_state_channel_tx/5]).
-export([post_force_progress_state_channel_tx/4]).
-export([post_oracle_register_tx/3,
         post_oracle_extend_tx/3,
         post_oracle_query_tx/4,
         post_oracle_response_tx/3]).
-export([post_name_preclaim_tx/4,
         post_name_claim_tx/3,
         post_name_update_tx/4,
         post_name_transfer_tx/5,
         post_name_revoke_tx/4]).
-export([post_contract_create_tx/3,
         post_contract_call_tx/3]).
-export([wait_for_value/4]).
-export([wait_for_time/3]).
-export([wait_for_time/4]).
-export([wait_for_startup/3]).
-export([time_to_ms/1]).
-export([assert_in_sync/1]).

%=== MACROS ====================================================================

-define(CALL_TAG, ?MODULE).
-define(CT_CONF_KEY, node_manager).
-define(CALL_TIMEOUT, 120000).
-define(NODE_TEARDOWN_TIMEOUT, 0).
-define(DEFAULT_HTTP_TIMEOUT, 3000).

-define(BASE_SPEC, #{
    backend => aest_docker,
    source  => {pull, "aeternity/aeternity:local"}
}).

%% AWK script to keep only error, critical, alert and emergency log lines with
%% all the extra lines following the log lines.
%% FIXME: Temporarily ignore dispatch_worker errors, remove when PT-159071812 fixes it.
%% Example of ignored lines:
%% 2018-07-10 15:48:59.649 [error] <0.1270.0>@aec_conductor:dispatch_worker:394 Disallowing dispatch of additional create_key_block_candidate worker
%% 2018-07-10 15:52:10.864 [error] <0.1270.0>@aec_conductor:dispatch_worker:394 Disallowing dispatch of additional micro_sleep worker
-define(DEFAULT_LOG_SCAN_AWK_SCRIPT, "
    /^.*\\[error\\].*aec_conductor:dispatch_worker.*Disallowing dispatch of additional.*$/ {
      matched = 1
      if (buff != \"\") {
        buff = \"\"
      }
    }
    /^.*\\[(error|critical|alert|emergency)\\].*$/ {
      if (!matched) {
        matched = 1
        buff = $0
      }
    }
    /^.*\\[(debug|info|notice|warning)\\].*$/ {
      matched = 1
      if (buff != \"\") {
        print buff
        buff = \"\"
      }
    }
    {
      if (!matched && (buff != \"\")) {
        buff = buff \"\\n\" $0
      }
      matched = 0
    }"
).

%% AWK script to filter out the crash from eper/watchdog
-define(CRASH_LOG_SCAN_AWK_SCRIPT, "
    /^[-0-9: ]*=ERROR REPORT====$/ {
        matched = 1
        state = 1
        if (buff != \"\") print buff
        buff = $0
    }
    /^Error in process <[0-9.]*> on node (epoch|aeternity)@localhost with exit value:$/ {
        if (state == 1) {
            matched = 1
            state = 2
            buff = buff \"\\n\" $0
        }
    }
    {
        if (!matched) {
            if (buff != \"\") print buff
            print $0
            buff = \"\"
            state = 0
        }
        matched = 0
    }"
).

%=== TYPES ====================================================================

-type test_ctx() :: pid() | proplists:proplist().
-type node_service() :: ext_http | int_http | ext_ws.
-type http_path() :: [atom() | binary() | number()] | binary().
-type http_query() :: #{atom() | binary() => atom() | binary()}.
-type json_object() :: term().
-type milliseconds() :: non_neg_integer().
-type path() :: binary() | string().
-type peer_spec() :: atom() | binary().

-type node_spec() :: #{
    % The unique name of the node
    name    := atom(),
    % If peer is given as an atom it is expected to be a node name,
    % if given as a binary it is expected to be the external URL of the peer.
    peers   := [peer_spec()],
    backend := aest_docker,

%% When `backend` is `aest_docker`:

    % The source of the docker image
    source  := {pull, binary() | string()},
    % Public/private peer key can be specified explicity for the node.
    % Both are required and will be saved, overriding any present keys.
    pubkey => binary(),
    privkey => binary()
}.

-export_type([test_ctx/0]).

%% Use this gas price in tests
gas_price() ->
    1000000000.

%=== COMMON TEST API FUNCTIONS =================================================

%% @doc Setups the the node manager for Common Test.
%% The CT config passed as argument is returned with extra values used
%% to contact with the node manager. This config must be passed to all
%% the the other functions as the `Ctx` parameter.
-spec ct_setup(proplists:proplist()) -> proplists:proplist().
ct_setup(Config) ->
    ok = application:set_env(aecore, network_id, <<"ae_system_test">>),
    {data_dir, DataDir} = proplists:lookup(data_dir, Config),
    {priv_dir, PrivDir} = proplists:lookup(priv_dir, Config),
    ct:log("Node logs can be found here: ~n<a href=\"file://~s\">~s</a>",
        [PrivDir, PrivDir]
    ),
    LogFun = fun(Fmt, Args) -> ct:log(Fmt, Args) end,
    case aest_nodes_mgr:start_link([aest_docker], #{ test_id => uid(),
                                                     log_fun => LogFun,
                                                     data_dir => DataDir,
                                                     temp_dir => PrivDir}) of
        {ok, Pid} -> [{?CT_CONF_KEY, Pid} | Config];
        {error, Reason} ->
            erlang:error({system_test_setup_failed, [{reason, Reason}]})
    end.

%% @doc Stops the node manager and all the nodes that were started.
-spec ct_cleanup(test_ctx()) -> ok.
ct_cleanup(Ctx) ->
    Pid = ctx2pid(Ctx),
    Result = validate_logs(Ctx),
    call(Pid, dump_logs),
    call(Pid, cleanup),
    call(Pid, stop),
    wait_for_exit(Pid, ?CALL_TIMEOUT),
    case Result of
        {error, Reason} ->
            %% returning fail will cause common test to see it as test failure
            {fail, Reason};
        ok -> ok
    end.

%=== QICKCHECK API FUNCTIONS ===================================================

%% @doc Setups the node manager for Quick Check tests.
-spec eqc_setup(path(), path()) -> test_ctx().
eqc_setup(DataDir, TempDir) ->
    case aest_nodes_mgr:start([aest_docker], #{data_dir => DataDir, temp_dir => TempDir}) of
        {ok, Pid} -> Pid;
        {error, Reason} ->
            erlang:error({system_test_setup_failed, [{reason, Reason}]})
    end.

%% @doc Stops the node manager for QuickCheck tests.
-spec eqc_cleanup(test_ctx()) -> ok.
eqc_cleanup(Ctx) ->
    Pid = ctx2pid(Ctx),
    Result = validate_logs(Ctx),
    call(Pid, cleanup),
    call(Pid, stop),
    wait_for_exit(Pid, 120000),
    case Result of
        {error, Reason} -> erlang:error(Reason);
        ok -> ok
    end.

%=== GENERIC API FUNCTIONS =====================================================

%% @doc Creates and setups a list of nodes.
%% The nodes are not started, use `start_node/2` for that.
-spec setup_nodes([node_spec()], test_ctx()) -> ok.
setup_nodes(NodeSpecs, Ctx) when is_list(NodeSpecs) ->
    call(ctx2pid(Ctx), {setup_nodes, NodeSpecs}).

%% @doc Starts a node previously setup.
-spec start_node(atom(), test_ctx()) -> ok.
start_node(NodeName, Ctx) ->
    call(ctx2pid(Ctx), {start_node, NodeName}).

%% @doc Stops a node previously started with explicit timeout (in milliseconds)
%% after which the node will be killed.
-spec stop_node(atom(), milliseconds() | infinity, test_ctx()) -> ok.
stop_node(NodeName, Timeout, Ctx) ->
    call(ctx2pid(Ctx), {stop_node, NodeName, Timeout}).

%% @doc Kills a node.
-spec kill_node(atom(), test_ctx()) -> ok.
kill_node(NodeName, Ctx) ->
    call(ctx2pid(Ctx), {kill_node, NodeName}).

-spec stop_container(atom(), milliseconds() | infinity, test_ctx()) -> ok.
stop_container(NodeName, Timeout, Ctx) ->
    call(ctx2pid(Ctx), {stop_container, NodeName, Timeout}).

extract_archive(NodeName, Path, Archive, Ctx) ->
    call(ctx2pid(Ctx), {extract_archive, NodeName, Path, Archive}).

run_cmd_in_node_dir(NodeName, Cmd, #{timeout := _} = Opts, Ctx) ->
    call(ctx2pid(Ctx), {run_cmd_in_node_dir, NodeName, Cmd, Opts}).

%% @doc Connect a node to a network.
-spec connect_node(atom(), atom(), test_ctx()) -> ok.
connect_node(NodeName, NetName, Ctx) ->
    call(ctx2pid(Ctx), {connect_node, NodeName, NetName}).

%% @doc Disconnect a node from a network.
-spec disconnect_node(atom(), atom(), test_ctx()) -> ok.
disconnect_node(NodeName, NetName, Ctx) ->
    call(ctx2pid(Ctx), {disconnect_node, NodeName, NetName}).

%% @doc Retrieves the address of a given node's service.
-spec get_service_address(atom(), node_service(), test_ctx()) -> binary().
get_service_address(NodeName, Service, Ctx) ->
    call(ctx2pid(Ctx), {get_service_address, NodeName, Service}).

%% @doc Retrieves the internal address of a given node's service.
-spec get_internal_address(atom(), node_service(), test_ctx()) -> binary().
get_internal_address(NodeName, Service, Ctx) ->
    call(ctx2pid(Ctx), {get_internal_address, NodeName, Service}).

-spec get_node_pubkey(atom(), test_ctx()) -> binary().
get_node_pubkey(NodeName, Ctx) ->
    call(ctx2pid(Ctx), {get_node_pubkey, NodeName}).

%% @doc Performs and HTTP get on a node service (ext_http or int_http).
-spec http_get(atom(), ext_http | int_http, http_path(), http_query(), test_ctx()) ->
        {ok, pos_integer(), json_object()} | {error, term()}.
http_get(NodeName, Service, Path, Query, Ctx) ->
    Addr = get_service_address(NodeName, Service, Ctx),
    http_addr_get(Addr, Path, Query).

export(NodeName, Name, Ctx) ->
    call(ctx2pid(Ctx), {export, NodeName, Name}).

read_metric(NodeName, MetricName, Ctx) ->
    call(ctx2pid(Ctx), {read_metric, NodeName, MetricName}).

%=== HELPER FUNCTIONS ==========================================================

%% @doc Return a tuple with the path of a temporary file in the given node
%% context and in the host context. This temporary file will ve in the log
%% directory for now.
-spec shared_temp_file(atom(), string() | binary()) -> {binary(), binary()}.
shared_temp_file(NodeName, FileName) ->
    {HostLogPath, GuestLogPath} = aest_nodes_mgr:get_log_path(NodeName),
    HostFilePath = filename:join(HostLogPath, FileName),
    GuestFilePath = filename:join(GuestLogPath, FileName),
    {HostFilePath, GuestFilePath}.

read_last_metric(NodeName, MetricName) ->
    {LogPath, _} = aest_nodes_mgr:get_log_path(NodeName),
    MetricsLogPath = binary_to_list(filename:join(LogPath, "aeternity_metrics.log")),
    case filelib:is_file(MetricsLogPath) of
        false -> undefined;
        true ->
            EscapedName = escap_for_regex(MetricName),
            Command = "grep '[0-9:\\.]* " ++ EscapedName ++ ":.*' '"
                ++ MetricsLogPath ++ "' | tail -n 1 | sed 's/^[0-9:\\.]*.*:\\([0-9]*\\)|.*$/\\1/'",
            case os:cmd(Command) of
                "" -> undefined;
                ValueStr ->
                    ValueStripped = string:strip(ValueStr, right, $\n),
                    list_to_integer(ValueStripped)
            end
    end.


cluster(Names, Spec) -> [spec(N, Names -- [N], Spec) || N <- Names].

spec(Name, Peers, Spec) ->
    maps:merge(maps:merge(?BASE_SPEC, Spec), #{name => Name, peers => Peers}).

request(Node, Id, Params) ->
    aehttp_client:request(Id, Params, [
        {ext_http, aest_nodes_mgr:get_service_address(Node, ext_http)},
        {int_http, aest_nodes_mgr:get_service_address(Node, int_http)},
        {ct_log, fun(X,Y) -> aest_nodes_mgr:log(X, Y) end}  %% use the log function configured by mgr
    ]).

get_node_config(Node, Path) ->
    aest_nodes_mgr:get_config(Node, Path).

%% @doc Performs an HTTP get request on the node external API.
-spec request(atom(), http_path(), http_query(), test_ctx()) -> json_object().
request(NodeName, Path, Query, Ctx) ->
    get(NodeName, ext_http, Path, Query, Ctx).

%% @doc Performs an HTTP get request on a node HTTP service.
-spec get(atom(), int_http | ext_http, http_path(), http_query(), test_ctx()) -> json_object().
get(NodeName, Service, Path, Query, Ctx) ->
    case http_get(NodeName, Service, Path, Query, Ctx) of
        {ok, 200, Response} -> Response;
        {ok, Status, _Response} -> error({unexpected_status, Status});
        {error, Reason} -> error({http_error, Reason})
    end.

get_status(NodeName) ->
    case request(NodeName, 'GetStatus', #{}) of
        {ok, 200, Status} -> Status;
        Other -> erlang:error({NodeName, Other})
    end.

get_block(NodeName, Height) ->
    case request(NodeName, 'GetKeyBlockByHeight', #{height => Height}) of
        {ok, 200, Block} -> Block;
        {ok, 404, _} -> undefined;
        Other -> erlang:error({NodeName, Other})
    end.

get_top(NodeName) ->
    Top = verify(200, request(NodeName, 'GetTopBlock', #{})),
    case Top of
        #{key_block := KeyBlock} -> KeyBlock;
        #{micro_block := MicroBlock} -> MicroBlock
    end.

get_mempool(NodeName) ->
    Txs = verify(200, request(NodeName, 'GetPendingTransactions', #{})),
    case {maps:get(<<"transactions">>, Txs, undefined),
          maps:get(transactions, Txs, undefined)} of
        {undefined, Mempool} -> Mempool; %% future proof
        {Mempool, undefined} -> Mempool
        %% nomatch if none of the two
    end.

get_account(NodeName, PubKey) ->
    Params = #{pubkey => aeser_api_encoder:encode(account_pubkey, PubKey)},
    verify(200, request(NodeName, 'GetAccountByPubkey', Params)).

get_channel(NodeName, PubKey) ->
    Params = #{pubkey => aeser_api_encoder:encode(channel, PubKey)},
    verify(200, request(NodeName, 'GetChannelByPubkey', Params)).

post_spend_tx(Node, From, To, Nonce, Map) ->
    #{ pubkey := SendPubKey, privkey := SendSecKey } = From,
    #{ pubkey := RecvPubKey} = To,
    PayLoad = iolist_to_binary(io_lib:format("~p", [Node])),
    Params = maps:merge(#{ sender_id => aeser_id:create(account, SendPubKey)
                         , recipient_id => aeser_id:create(account, RecvPubKey)
                         , amount => 10000 * gas_price()
                         , fee => 20000 * gas_price()
                         , ttl => 10000000
                         , nonce => Nonce
                         , payload => PayLoad }, Map),
    post_transaction(Node, aec_spend_tx, SendSecKey, Params, #{}).

post_create_state_channel_tx(Node, Initiator, Responder, #{nonce := Nonce} = Map) ->
    #{ pubkey := InPubKey, privkey := InSecKey } = Initiator,
    #{ pubkey := RespPubKey, privkey := RespSecKey } = Responder,
    Round = 0, %% this needs a data structure containing round!!
    {ok, CreateTx} = aesc_create_tx:new(maps:merge(
                                          #{initiator_id => aeser_id:create(account, InPubKey),
                                            responder_id => aeser_id:create(account, RespPubKey),
                                            state_hash => <<Round:256>>,
                                            initiator_amount => 200000 * gas_price(),
                                            responder_amount => 200000 * gas_price(),
                                            push_amount => 0,
                                            lock_period => 0,
                                            ttl => 100000,
                                            fee => 50000 * gas_price(),
                                            channel_reserve => 40}, Map)),
    BothSigned = aec_test_utils:sign_tx(CreateTx, [InSecKey, RespSecKey]),
    Transaction = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(BothSigned)),
    Response = verify(200, request(Node, 'PostTransaction', #{tx => Transaction})),
    ChPubKey = aesc_channels:pubkey(InPubKey, Nonce, RespPubKey),
    Response#{
        channel_pubkey => ChPubKey,
        channel_id => aeser_id:create(channel, ChPubKey)
    }.

post_close_mutual_state_channel_tx(Node, Initiator, Responder, ChannelId, #{nonce := _} = Map) ->
    #{ pubkey := InPubKey, privkey := InSecKey } = Initiator,
    #{ privkey := RespSecKey } = Responder,
    {ok, CloseTx} =
        aesc_close_mutual_tx:new(maps:merge(#{channel_id => ChannelId,
                                              from_id => aeser_id:create(account, InPubKey),
                                              initiator_amount_final => 100000 * gas_price(),
                                              responder_amount_final => 100000 * gas_price(),
                                              fee => 50000 * gas_price(),
                                              ttl => 100000},
                                            Map)),
    BothSigned = aec_test_utils:sign_tx(CloseTx, [InSecKey, RespSecKey]),
    Transaction = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(BothSigned)),
    verify(200, request(Node, 'PostTransaction', #{tx => Transaction})).

post_deposit_state_channel_tx(Node, PayingParty, OtherParty, ChannelId, #{nonce := _} = Map) ->
    #{ pubkey := InPubKey, privkey := InSecKey } = PayingParty,
    #{ privkey := RespSecKey } = OtherParty,
    {ok, DepositTx} =
        aesc_deposit_tx:new(maps:merge(#{channel_id => ChannelId,
                                         from_id => aeser_id:create(account, InPubKey),
                                         state_hash => <<0:256>>,
                                         amount => 20 * gas_price(),
                                         round => 1,
                                         fee => 50000 * gas_price(),
                                         ttl => 100000},
                                       Map)),
    BothSigned = aec_test_utils:sign_tx(DepositTx, [InSecKey, RespSecKey]),
    Transaction = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(BothSigned)),
    verify(200, request(Node, 'PostTransaction', #{tx => Transaction})).

post_withdraw_state_channel_tx(Node, RecParty, OtherParty, ChannelId, #{nonce := _} = Map) ->
    #{ pubkey := InPubKey, privkey := InSecKey } = RecParty,
    #{ privkey := RespSecKey } = OtherParty,
    {ok, WithdrawTx} =
        aesc_withdraw_tx:new(maps:merge(#{channel_id => ChannelId,
                                         to_id => aeser_id:create(account, InPubKey),
                                         state_hash => <<0:256>>,
                                         amount => 20 * gas_price(),
                                         round => 1,
                                         fee => 50000 * gas_price(),
                                         ttl => 100000},
                                       Map)),
    BothSigned = aec_test_utils:sign_tx(WithdrawTx, [InSecKey, RespSecKey]),
    Transaction = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(BothSigned)),
    verify(200, request(Node, 'PostTransaction', #{tx => Transaction})).

post_force_progress_state_channel_tx(Node, From, ChannelId, #{nonce := _} = Map) ->
    #{ pubkey := SendPubKey, privkey := SendSecKey } = From,
    Params = maps:merge(#{ channel_id => ChannelId
                         , from_id => aeser_id:create(account, SendPubKey)
                         , payload => <<>>
                         , update => aesc_offchain_update:op_call_contract(aeser_id:create(account, SendPubKey), aeser_id:create(contract, <<42:32/unit:8>>), 3, 0, <<"calldata">>, [], gas_price(), 1000000)
                         , state_hash => <<42:32/unit:8>>
                         , round => 0
                         , offchain_trees => aec_trees:new()
                         , fee => 4000000 * gas_price()
                         , ttl => 10000000 }, Map),
    post_transaction(Node, aesc_force_progress_tx, SendSecKey, Params, #{}).

post_oracle_register_tx(Node, OAccount, Opts) ->
    #{ pubkey := OPubKey, privkey := OPrivKey } = OAccount,
    post_transaction(Node, aeo_register_tx, OPrivKey, Opts, #{
        account_id => aeser_id:create(account, OPubKey),
        abi_version => 0
    }).

post_oracle_extend_tx(Node, OAccount, Opts) ->
    #{ pubkey := OPubKey, privkey := OPrivKey } = OAccount,
    post_transaction(Node, aeo_extend_tx, OPrivKey, Opts, #{
        oracle_id => aeser_id:create(oracle, OPubKey)
    }).

post_oracle_query_tx(Node, QuerierAcc, OracleAcc, #{ nonce := _ } = Opts) ->
    #{ pubkey := QPubKey, privkey := QPrivKey } = QuerierAcc,
    #{ pubkey := OPubKey } = OracleAcc,
    post_transaction(Node, aeo_query_tx, QPrivKey, Opts, #{
        sender_id    => aeser_id:create(account, QPubKey),
        oracle_id    => aeser_id:create(oracle, OPubKey)
    }).

post_oracle_response_tx(Node, OracleAcc, #{ nonce := _ } = Opts) ->
    #{ pubkey := OPubKey, privkey := OPrivKey } = OracleAcc,
    post_transaction(Node, aeo_response_tx, OPrivKey, Opts, #{
        oracle_id => aeser_id:create(oracle, OPubKey)
    }).

post_name_preclaim_tx(Node, OwnerAcc, CommitmentHash, Opts) ->
    #{ pubkey := OPubKey, privkey := OPrivKey } = OwnerAcc,
    post_transaction(Node, aens_preclaim_tx, OPrivKey, Opts, #{
        account_id => aeser_id:create(account, OPubKey),
        commitment_id => aeser_id:create(commitment, CommitmentHash)
    }).

post_name_claim_tx(Node, OwnerAcc, Opts) ->
    #{ pubkey := OPubKey, privkey := OPrivKey } = OwnerAcc,
    post_transaction(Node, aens_claim_tx, OPrivKey, Opts, #{
        account_id => aeser_id:create(account, OPubKey)
    }).

post_name_update_tx(Node, OwnerAcc, Name, Opts) ->
    #{ pubkey := OPubKey, privkey := OPrivKey } = OwnerAcc,
    post_transaction(Node, aens_update_tx, OPrivKey, Opts, #{
        account_id => aeser_id:create(account, OPubKey),
        name_id => aeser_id:create(name, aens_hash:name_hash(Name))
    }).

post_name_transfer_tx(Node, OwnerAcc, RecipientAcc, Name, Opts) ->
    #{ pubkey := OPubKey, privkey := OPrivKey } = OwnerAcc,
    post_transaction(Node, aens_transfer_tx, OPrivKey, Opts, #{
        account_id => aeser_id:create(account, OPubKey),
        name_id => aeser_id:create(name, aens_hash:name_hash(Name)),
        recipient_id => aeser_id:create(account, maps:get(pubkey, RecipientAcc))
    }).

post_name_revoke_tx(Node, OwnerAcc, Name, Opts) ->
    #{ pubkey := OPubKey, privkey := OPrivKey } = OwnerAcc,
    post_transaction(Node, aens_revoke_tx, OPrivKey, Opts, #{
        account_id => aeser_id:create(account, OPubKey),
        name_id => aeser_id:create(name, aens_hash:name_hash(Name))
    }).

post_contract_create_tx(Node, PrivKey, Opts) ->
    post_transaction(Node, aect_create_tx, PrivKey, #{}, Opts).

post_contract_call_tx(Node, PrivKey, Opts) ->
    post_transaction(Node, aect_call_tx, PrivKey, #{}, Opts).

post_transaction(Node, TxMod, PrivKey, ExtraTxArgs, TxArgs) ->
    {ok, RespTx} = TxMod:new(maps:merge(TxArgs, ExtraTxArgs)),
    Signed = aec_test_utils:sign_tx(RespTx, [PrivKey]),
    SignedEnc = aetx_sign:serialize_to_binary(Signed),
    Transaction = aeser_api_encoder:encode(transaction, SignedEnc),
    verify(200, request(Node, 'PostTransaction', #{tx => Transaction})).

%% Use values that are not yet api encoded in test cases
wait_for_value({balance, PubKey, MinBalance}, NodeNames, Timeout, Ctx) ->
    FaultInject = proplists:get_value(fault_inject, Ctx, #{}),
    Delay = proplists:get_value(delay, Ctx, 500),
    CheckF =
        fun(Node) ->
                 case request(Node, 'GetAccountByPubkey', maps:merge(#{pubkey => aeser_api_encoder:encode(account_pubkey, PubKey)}, FaultInject)) of
                     {ok, 200, #{balance := Balance}} when Balance >= MinBalance -> {done, #{PubKey => Balance}};
                     _ -> wait
                end
        end,
    loop_for_values(CheckF, NodeNames, [], Delay, Timeout);
wait_for_value({height, MinHeight}, NodeNames, Timeout, Ctx) ->
    FaultInject = proplists:get_value(fault_inject, Ctx, #{}),
    Delay = proplists:get_value(delay, Ctx, 500),
    CheckF =
        fun(Node) ->
                case request(Node, 'GetKeyBlockByHeight', maps:merge(#{height => MinHeight}, FaultInject)) of
                    {ok, 200, Block} -> {done, Block};
                    _ -> wait
                end
        end,
    loop_for_values(CheckF, NodeNames, [], Delay, Timeout, {"Height ~p on nodes ~p", [MinHeight, NodeNames]});
wait_for_value({contract_id, CId}, NodeNames, Timeout, Ctx) ->
    Delay = proplists:get_value(delay, Ctx, 1000),
    CheckF =
        fun(Node) ->
          case request(Node, 'GetContract', #{ pubkey => CId }) of
            {ok, 200, _} -> {done, ok};
            _ -> wait
          end
        end,
    loop_for_values(CheckF, NodeNames, [], Delay, Timeout, {"", []});
wait_for_value({txs_on_chain, Txs}, NodeNames, Timeout, Ctx) ->
    %% Not very optimal, since found Txs' are searched for in next round.
    FaultInject = proplists:get_value(fault_inject, Ctx, #{}),
    KeyBlocksWaiting = proplists:get_value(key_blocks, Ctx, 2),
    Delay = proplists:get_value(delay, Ctx, 500),
    CheckF =
        fun(Node) ->
                Found =
                    lists:usort([ case request(Node, 'GetTransactionByHash', maps:merge(#{hash => Tx}, FaultInject)) of
                                      {ok, 200, #{ block_height := H}} when H > 0 -> {Tx, H};
                                      _ -> wait
                                  end || Tx <- Txs]),
                case lists:member(wait, Found) of
                    false ->
                        case {get_top(Node), lists:max([0 |[ H || {_,H} <- Found]])} of
                            {#{height := Top}, Max} when Top >= Max + KeyBlocksWaiting ->
                                %% We wait some key block on top of the microblock that contains the transaction
                                {done, Found};
                            _ ->
                                wait
                        end;
                    true -> wait
                end
        end,
    loop_for_values(CheckF, NodeNames, [], Delay, Timeout, {"Txs found ~p", [Txs]});
wait_for_value({txs_on_node, Txs}, NodeNames, Timeout, Ctx) ->
    %% Reached the mempool at least, probably even in a block.
    FaultInject = proplists:get_value(fault_inject, Ctx, #{}),
    Delay = proplists:get_value(delay, Ctx, 500),
    CheckF =
        fun(Node) ->
                Found = [ case request(Node, 'GetTransactionByHash', maps:merge(#{hash => Tx}, FaultInject)) of
                                      {ok, 200, #{ block_height := H}} -> {Tx, H};
                                      _ -> wait
                                  end || Tx <- Txs],
                case lists:member(wait, Found) of
                    false -> {done, Found};
                    true -> wait
                end
        end,
    loop_for_values(CheckF, NodeNames, [], Delay, Timeout, {"Txs found ~p", [Txs]});
wait_for_value({txs_all_dropped, Txs}, NodeNames, Timeout, Ctx) ->
    FaultInject = proplists:get_value(fault_inject, Ctx, #{}),
    Delay = proplists:get_value(delay, Ctx, 500),
    CheckF =
        fun(Node) ->
            Exists = lists:foldl(fun(Tx, Acc) ->
                Params = maps:merge(#{hash => Tx}, FaultInject),
                case request(Node, 'GetTransactionByHash', Params) of
                    {ok, 200, Block} -> [Block | Acc];
                    {ok, 404, _} -> Acc
                end
            end, [], Txs),
            case length(Exists) > 0 of
                true -> wait;
                false -> {done, undefined}
            end
        end,
    loop_for_values(CheckF, NodeNames, [], Delay, Timeout, {"Txs found ~p", [Txs]});
wait_for_value({txs_any_dropped, Txs}, NodeNames, Timeout, Ctx) ->
    FaultInject = proplists:get_value(fault_inject, Ctx, #{}),
    Delay = proplists:get_value(delay, Ctx, 500),
    CheckF =
        fun(Node) ->
            Exists = lists:foldl(fun(Tx, Acc) ->
                Params = maps:merge(#{hash => Tx}, FaultInject),
                case request(Node, 'GetTransactionByHash', Params) of
                    {ok, 200, Block} -> [Block | Acc];
                    {ok, 404, _} -> Acc
                end
            end, [], Txs),
            case length(Exists) < length(Txs) of
                false -> wait;
                true -> {done, Exists}
            end
        end,
    loop_for_values(CheckF, NodeNames, [], Delay, Timeout, {"Txs found ~p", [Txs]}).


wait_for_time(height, NodeNames, Time) ->
    wait_for_time(height, NodeNames, Time, #{}).

wait_for_time(height, NodeNames, TimeUnit, Opts) ->
    Time = time_to_ms(TimeUnit),
    Interval = time_to_ms(maps:get(interval, Opts, {seconds, 10})),
    ProgressFun = fun(Elapsed) ->
        [{_Node, Lowest}|_] = Tops = lists:sort(fun({_, A}, {_, B}) ->
            maps:get(height, A) =< maps:get(height, B)
        end, [{N, get_top(N)} || N <- NodeNames]),
        aest_nodes_mgr:log("Heights after ~p s: ~p", [
            floor(Elapsed / 1000),
            [{N, H} || {N, #{height := H}} <- Tops]
        ]),
        Lowest
    end,
    Block = repeat(ProgressFun, Interval, Time),
    maps:get(height, Block).

wait_for_startup(Nodes, Height, Cfg) ->
    StartupTimeout = proplists:get_value(node_startup_time, Cfg, 20000),
    wait_for_value({height, Height}, Nodes, StartupTimeout, Cfg).

repeat(Fun, Interval, Max) ->
    Start = erlang:system_time(millisecond),
    timer:sleep(Interval),
    repeat(Fun, Interval, Max, Start).

repeat(Fun, Interval, Time, Start) ->
    Elapsed = erlang:system_time(millisecond) - Start,
    case Elapsed >= Time of
        true ->
            Fun(Elapsed);
        false ->
            timer:sleep(Interval),
            _Result = Fun(Elapsed),
            repeat(Fun, Interval, Time, Start)
    end.

time_to_ms(Time) when is_integer(Time) -> Time;
time_to_ms({milliseconds, Time})       -> Time;
time_to_ms({seconds, Time})            -> Time * 1000;
time_to_ms({minutes, Time})            -> Time * 1000 * 60;
time_to_ms({hours, Time})              -> Time * 1000 * 60 * 60.

assert_in_sync(Blocks) when is_map(Blocks) ->
    assert_in_sync(maps:values(Blocks));
assert_in_sync([_Block]) ->
    ok;
assert_in_sync([B1, B2|Blocks]) ->
    ?assertEqual(B1, B2),
    assert_in_sync([B2|Blocks]).

%=== INTERNAL FUNCTIONS ========================================================

escap_for_regex(Str) ->
    lists:flatten(re:replace(Str, "[\\\\^\\$\\.\\|\\(\\)\\?\\*\\+\\{\\-\\[\\]]",
                             "\\\\&", [global, {return, list}])).

uid() ->
    iolist_to_binary([[io_lib:format("~2.16.0B",[X])
                       || <<X:8>> <= crypto:strong_rand_bytes(8) ]]).

ctx2pid(Pid) when is_pid(Pid) -> Pid;
ctx2pid(Props) when is_list(Props) ->
    case proplists:lookup(?CT_CONF_KEY, Props) of
        {?CT_CONF_KEY, Pid} when is_pid(Pid) -> Pid;
        _ ->
            erlang:error({system_test_not_setup, []})
    end.

call(Pid, Msg) ->
    case gen_server:call(Pid, Msg, ?CALL_TIMEOUT) of
        {'$error', Reason, Stacktrace} ->
            erlang:raise(throw, Reason, Stacktrace);
        Reply ->
            Reply
    end.

wait_for_exit(Pid, Timeout) ->
    Ref = erlang:monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _Reason} -> ok
    after Timeout -> error({process_not_stopped, Pid})
    end.

loop_for_values(CheckF, Nodes, Rem, Delay, Timeout) ->
    loop_for_values(CheckF, Nodes, Rem, Delay, Timeout, {"", []}).

loop_for_values(CheckF, Nodes, Rem, Delay, Timeout, FinalMessage)
  when is_integer(Timeout) ->
    Start = erlang:system_time(millisecond),
    Expiration = Start + Timeout,
    ExpFun = make_time_expiration(Expiration),
    Results = wait_for_value(CheckF, Nodes, Rem, Delay, ExpFun, #{}),
    End = erlang:system_time(millisecond),
    {Format, Args} = FinalMessage,
    aest_nodes_mgr:log("Reached after ~.2f seconds (slack ~p ms) " ++ Format,
                       [ (End - Start) / 1000, Timeout - (End - Start) | Args]),
    Results;
loop_for_values(CheckF, Nodes, Rem, Delay, {blocks_delta, MaxDeltaHeight}, FinalMessage)
  when is_integer(MaxDeltaHeight) ->
    RefHeight = get_min_top_height(Nodes),
    MaxHeight = RefHeight + MaxDeltaHeight,
    loop_for_values(CheckF, Nodes, Rem, Delay, {blocks, MaxHeight}, FinalMessage);
loop_for_values(CheckF, Nodes, Rem, Delay, {blocks, MaxHeight}, FinalMessage)
  when is_integer(MaxHeight) ->
    Start = erlang:system_time(millisecond),
    ExpFun = make_height_expiration(Nodes, MaxHeight),
    Results = wait_for_value(CheckF, Nodes, Rem, Delay, ExpFun, #{}),
    End = erlang:system_time(millisecond),
    {Format, Args} = FinalMessage,
    aest_nodes_mgr:log("Reached after ~.2f seconds" ++ Format,
                       [ (End - Start) / 1000 | Args]),
    Results.

get_min_top_height(Nodes) ->
    lists:min([ H || #{ height := H } <- [get_top(N) || N <- Nodes ] ]).

make_height_expiration(Nodes, MaxHeight) ->
    fun() -> get_min_top_height(Nodes) > MaxHeight end.

make_time_expiration(MaxTime) ->
    fun() -> erlang:system_time(millisecond) > MaxTime end.

wait_for_value(_CheckF, [], [], _Delay, _ExpFun, Results) -> Results;
wait_for_value(CheckF, [], Rem, Delay, ExpFun, Results) ->
    case ExpFun() of
        false ->
            timer:sleep(Delay),
            wait_for_value(CheckF, lists:reverse(Rem), [], Delay, ExpFun, Results);
        true ->
            error(timeout)
    end;
wait_for_value(CheckF, [Node | Nodes], Rem, Delay, ExpFun, Results) ->
    case CheckF(Node) of
        {done, Result} ->
            wait_for_value(CheckF, Nodes, Rem, Delay, ExpFun, maps:put(Node, Result, Results));
        wait ->
            wait_for_value(CheckF, Nodes, [Node | Rem], Delay, ExpFun, Results)
    end.

http_addr_get(Addr, Path, Query) ->
    http_send(get, Addr, Path, Query, [], <<>>, #{}).

http_send(Method, Addr, Path, Query, Headers, Body, Opts) ->
    Timeout = maps:get(timeout, Opts, ?DEFAULT_HTTP_TIMEOUT),
    HttpOpts = [{recv_timeout, Timeout}],
    case hackney:request(Method, url(Addr, Path, Query), Headers, Body, HttpOpts) of
        {error, _Reason} = Error -> Error;
        {ok, Status, _RespHeaders, ClientRef} ->
            case hackney_json_body(ClientRef) of
                {error, _Reason} = Error -> Error;
                {ok, Response} -> {ok, Status, Response}
            end
    end.

url(Base, Path, QS) when is_list(Path) ->
    hackney_url:make_url(Base, [to_binary(P) || P <- Path], maps:to_list(QS));
url(Base, Item, QS) ->
    url(Base, [Item], QS).

to_binary(Term) when is_atom(Term) -> atom_to_binary(Term, utf8);
to_binary(Term) when is_integer(Term) -> integer_to_binary(Term);
to_binary(Term)                    -> Term.

hackney_json_body(ClientRef) ->
    case hackney:body(ClientRef) of
        {error, _Reason} = Error -> Error;
        {ok, BodyJson} -> decode(BodyJson)
    end.

decode(<<>>) -> {ok, undefined};
decode(Data) -> decode_json(Data).

decode_json(Data) ->
    try jsx:decode(Data, [{labels, attempt_atom}, return_maps]) of
        JsonObj -> {ok, JsonObj}
    catch
        error:badarg -> {error, {bad_json, Data}}
    end.

validate_logs(Cfg) ->
    Logs = aest_nodes_mgr:get_log_paths(),
    maps:fold(fun(NodeName, {LogPath, _}, Result) ->
        Result1 = check_crash_log(NodeName, LogPath, Cfg, Result),
        Result2 = check_log_for_errors(NodeName, LogPath, "aeternity.log", Cfg, Result1),
        Result3 = check_log_for_errors(NodeName, LogPath, "aeternity_sync.log", Cfg, Result2),
                  check_log_for_errors(NodeName, LogPath, "aeternity_mining.log", Cfg, Result3)
    end, ok, Logs).

check_crash_log(NodeName, LogPath, Cfg, Result) ->
    LogFile = binary_to_list(filename:join(LogPath, "crash.log")),
    case filelib:is_file(LogFile) of
        false -> Result;
        true ->
            case filelib:file_size(LogFile) of
                0 -> Result;
                _ ->
                    Command = "awk '" ?CRASH_LOG_SCAN_AWK_SCRIPT "' '" ++ LogFile ++ "'",
                    case os:cmd(Command) of
                        "" -> Result;
                        ErrorLines ->
                            aest_nodes_mgr:log("Node ~p's crash logs is not empty:~n~s",
                                               [NodeName, ErrorLines]),
                            case proplists:get_value(verify_logs, Cfg, true) of
                                true -> {error, crash_log_not_empty};
                                false -> Result
                            end
                    end
            end
    end.

check_log_for_errors(NodeName, LogPath, LogName, Cfg, Result) ->
    LogFile = binary_to_list(filename:join(LogPath, LogName)),
    case filelib:is_file(LogFile) of
        false -> Result;
        true ->
            Command = "awk '" ?DEFAULT_LOG_SCAN_AWK_SCRIPT "' '" ++ LogFile ++ "'",
            case os:cmd(Command) of
                "" -> Result;
                ErrorLines ->
                    aest_nodes_mgr:log("Node ~p's log ~p contains errors:~n~s",
                                       [NodeName, LogName, ErrorLines]),
                    case proplists:get_value(verify_logs, Cfg, true) of
                        true -> {error, log_has_errors};
                        false -> Result
                    end

            end
    end.

verify(Code, {ok, Code, Result}) ->
    Result;
verify(_Code, {ok, Other, Result}) ->
    erlang:error({unexpected_response, Other, Result});
verify(_Code, Other) ->
    erlang:error({request_error, Other}).
