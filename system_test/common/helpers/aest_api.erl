-module(aest_api).

%=== EXPORTS ===================================================================

-export([ sc_open/2
        , sc_withdraw/3
        , sc_close_mutual/2
        , sc_transfer/3
        , sc_deploy_contract/4
        , sc_call_contract/4
        , sc_leave/1
        , sc_reestablish/5
        , sc_wait_close/1]).

%=== INCLUDES ==================================================================

%=== MACROS ====================================================================

-define(WS, aehttp_ws_test_utils).
-define(WS_URL_REGEX, "^ws://([a-zA-Z0-9-._]*):([0-9]*)/?$").

%=== TYPES =====================================================================

-type account() :: #{
  pubkey := binary(),
  privkey := binary()
}.


-type sc_open_params() :: #{
    initiator_node    := atom(),
    initiator_id      := account(),
    initiator_amount  := non_neg_integer(),
    responder_node    := atom(),
    responder_id      := account(),
    responder_amount  := non_neg_integer(),
    responder_port    => pos_integer()
}.

-type channel() :: map().
-type party() :: initiator | responder.

%=== API FUNCTIONS =============================================================

-spec sc_open(sc_open_params(), aest_nodes:test_ctx())
    -> {ok, channel(), binary(), pos_integer()}.
sc_open(Params, Cfg) ->
    #{
        initiator_node    := INodeName,
        initiator_id      := IAccount,
        initiator_amount  := IAmt,
        responder_node    := RNodeName,
        responder_id      := RAccount,
        responder_amount  := RAmt
    } = Params,
    #{ pubkey := IPubKey, privkey := IPrivKey } = IAccount,
    #{ pubkey := RPubKey, privkey := RPrivKey } = RAccount,

    {Host, _Port} = node_ws_int_addr(RNodeName, Cfg),
    MaybeOpts = maps:merge(maybe_version_opts(Cfg),
                           maybe_block_hash_opts(Params)),
    Opts = maps:merge(
             #{
               protocol => <<"json-rpc">>,
               host => Host,
               port => maps:get(responder_port, Params, 9000),
               initiator_id => aeser_api_encoder:encode(account_pubkey, IPubKey),
               responder_id => aeser_api_encoder:encode(account_pubkey, RPubKey),
               lock_period => maps:get(lock_period, Params, 10),
               push_amount => maps:get(push_amount, Params, 10),
               initiator_amount => IAmt,
               responder_amount => RAmt,
               channel_reserve => maps:get(channel_reserve, Params, 2),
               minimum_depth_factor => 0
              },
             MaybeOpts),

    {IConn, RConn} = sc_start_ws_peers(INodeName, RNodeName, Opts, Cfg),
    {ok, #{ <<"event">> := <<"fsm_up">>, <<"fsm_id">> := IFsmId }} = sc_wait_for_channel_event(IConn, info),
    {ok, #{ <<"event">> := <<"fsm_up">>, <<"fsm_id">> := RFsmId }} = sc_wait_for_channel_event(RConn, info),
    true = IFsmId /= RFsmId,

    ok = sc_wait_channel_open(IConn, RConn),

    %% initiator gets to sign a create_tx
    CrTx = sc_wait_and_sign(IConn, IPrivKey, <<"initiator_sign">>),
    {ok, #{ <<"event">> := <<"funding_created">>} } = sc_wait_for_channel_event(RConn, info),
    %% responder gets to sign a create_tx
    CrTx = sc_wait_and_sign(RConn, RPrivKey, <<"responder_sign">>),
    {ok, #{ <<"event">> := <<"funding_signed">>} } = sc_wait_for_channel_event(IConn, info),
    %% both of them receive the same co-signed channel_create_tx
    {ok, #{ <<"info">> := <<"funding_signed">>
          , <<"tx">> := EncSignedCrTx} } = sc_wait_for_channel_event(IConn, on_chain_tx),
    {ok, #{ <<"info">> := <<"funding_created">>
          , <<"tx">> := EncSignedCrTx} } = sc_wait_for_channel_event(RConn, on_chain_tx),

    {ok, BinSignedCrTx} = aeser_api_encoder:safe_decode(transaction, EncSignedCrTx),
    SignedCrTx = aetx_sign:deserialize_from_binary(BinSignedCrTx),
    %% same transaction
    CrTx = aetx_sign:tx(SignedCrTx),

    {channel_create_tx, Tx} = aetx:specialize_type(CrTx),
    %% Check public keys
    IPubKey = aesc_create_tx:initiator_pubkey(Tx),
    RPubKey = aesc_create_tx:responder_pubkey(Tx),
    Fee = aesc_create_tx:fee(Tx),
    ChannelId = aesc_create_tx:channel_pubkey(Tx),

    ok = sc_wait_channel_changed(IConn, RConn, <<"channel_create_tx">>),

    ok = sc_wait_funding_locked(IConn, RConn),

    ok = sc_wait_open(IConn, RConn),

    Channel = #{ channel_id => ChannelId
               , initiator => {IAccount, IConn, IFsmId}
               , responder => {RAccount, RConn, RFsmId} },

    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedCrTx)),

    {ok, Channel, TxHash, Fee}.

-spec sc_reestablish(channel(), atom(), atom(), binary(), aest_nodes:test_ctx()) -> {ok, channel()}.
sc_reestablish(#{ channel_id := ChannelId
                , initiator := {IAccount, _, IFsmId0}
                , responder := {RAccount, _, RFsmId0}} = Chan
              , INodeName, RNodeName, LatestState, Cfg) ->
    {Host, _Port} = node_ws_int_addr(RNodeName, Cfg),
    Opts = maps:merge(
             #{ existing_channel_id => aeser_api_encoder:encode(channel, ChannelId)
              , existing_fsm_id => [{initiator, IFsmId0}, {responder, RFsmId0}]
              , host => Host
              , offchain_tx => LatestState
              , port => 9000
              , protocol => <<"json-rpc">>
              }, maybe_version_opts(Cfg)),

    {IConn, RConn} = sc_start_ws_peers(INodeName, RNodeName, Opts, Cfg),
    {ok, #{ <<"event">> := <<"fsm_up">>, <<"fsm_id">> := IFsmId1 }} = sc_wait_for_channel_event(IConn, info),
    {ok, #{ <<"event">> := <<"fsm_up">>, <<"fsm_id">> := RFsmId1 }} = sc_wait_for_channel_event(RConn, info),
    true = IFsmId0 /= IFsmId1,
    true = RFsmId0 /= RFsmId1,
    {ok, #{ <<"event">> := <<"channel_reestablished">> }} = sc_wait_for_channel_event(IConn, info),
    {ok, #{ <<"event">> := <<"channel_reestablished">> }} = sc_wait_for_channel_event(RConn, info),
    ok = sc_wait_reestablish(IConn, RConn),

    {ok, Chan#{initiator => {IAccount, IConn, IFsmId1}, responder => {RAccount, RConn, RFsmId1}}}.

-spec sc_withdraw(channel(), party(), non_neg_integer())
    -> {ok, binary(), non_neg_integer()}.
sc_withdraw(Channel, Origin, Amount)
  when Origin =:= initiator orelse Origin =:= responder ->
    #{ initiator := {IAccount, IConn, _}, responder := {RAccount, RConn, _} } = Channel,
    #{ privkey := IPrivKey } = IAccount,
    #{ privkey := RPrivKey } = RAccount,

    case Origin of
        initiator -> sc_withdraw(IConn, IPrivKey, Amount, RConn, RPrivKey);
        responder -> sc_withdraw(RConn, RPrivKey, Amount, IConn, IPrivKey)
    end.

-spec sc_close_mutual(channel(), party())
    -> {ok, binary(), non_neg_integer(), non_neg_integer()}.
sc_close_mutual(Channel, Closer)
  when Closer =:= initiator orelse Closer =:= responder ->
    #{ initiator := {IAccount, IConn, _}, responder := {RAccount, RConn, _} } = Channel,
    #{ privkey := IPrivKey } = IAccount,
    #{ privkey := RPrivKey } = RAccount,

    ShutdownTx = case Closer of
        initiator -> sc_close_mutual(IConn, IPrivKey, RConn, RPrivKey);
        responder -> sc_close_mutual(RConn, RPrivKey, IConn, IPrivKey)
    end,

    {ok, #{ <<"tx">> := EncSignedMutualTx} } = sc_wait_for_channel_event(IConn, on_chain_tx),
    {ok, #{ <<"tx">> := EncSignedMutualTx} } = sc_wait_for_channel_event(RConn, on_chain_tx),

    {ok, BinSignedMutualTx} = aeser_api_encoder:safe_decode(transaction, EncSignedMutualTx),
    SignedMutualTx = aetx_sign:deserialize_from_binary(BinSignedMutualTx),
    %% same transaction
    ShutdownTx = aetx_sign:tx(SignedMutualTx),

    {channel_close_mutual_tx, MutualTx} = aetx:specialize_type(ShutdownTx),

    IChange = aesc_close_mutual_tx:initiator_amount_final(MutualTx),
    RChange = aesc_close_mutual_tx:responder_amount_final(MutualTx),
    Fee     = aesc_close_mutual_tx:fee(MutualTx),

    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedMutualTx)),

    {ok, TxHash, Fee, IChange, RChange}.

-spec sc_transfer(channel(), party(), non_neg_integer()) -> ok.
sc_transfer(Channel, Sender, Amount) when Sender =:= initiator orelse Sender =:= responder ->
    #{ initiator := {IAccount, IConn, _}, responder := {RAccount, RConn, _} } = Channel,
    #{ pubkey := IPubKey, privkey := IPrivKey } = IAccount,
    #{ pubkey := RPubKey, privkey := RPrivKey } = RAccount,

    case Sender of
        initiator -> sc_transfer(IConn, IPubKey, IPrivKey, RConn, RPubKey, RPrivKey, Amount);
        responder -> sc_transfer(RConn, RPubKey, RPrivKey, IConn, IPubKey, IPrivKey, Amount)
    end.

-spec sc_deploy_contract(channel(), party(), map(), binary()) -> {ok, map()}.
sc_deploy_contract(Channel, Who, Contract, CallData) ->
    #{ initiator := {IAccount, IConn, _}, responder := {RAccount, RConn, _} } = Channel,
    #{ pubkey := IPubKey, privkey := IPrivKey } = IAccount,
    #{ pubkey := RPubKey, privkey := RPrivKey } = RAccount,

    case Who of
        initiator -> sc_deploy_contract(IConn, IPubKey, IPrivKey, RConn, RPrivKey, Contract, CallData);
        responder -> sc_deploy_contract(RConn, RPubKey, RPrivKey, IConn, IPrivKey, Contract, CallData)
    end.

-spec sc_call_contract(channel(), party(), map(), binary()) -> {ok, term()}.
sc_call_contract(Channel, Who, Contract, CallData) ->
    #{ initiator := {IAccount, IConn, _}, responder := {RAccount, RConn, _} } = Channel,
    #{ pubkey := IPubKey, privkey := IPrivKey } = IAccount,
    #{ pubkey := RPubKey, privkey := RPrivKey } = RAccount,

    case Who of
        initiator -> sc_call_contract(IConn, IPubKey, IPrivKey, RConn, RPrivKey, Contract, CallData);
        responder -> sc_call_contract(RConn, RPubKey, RPrivKey, IConn, IPrivKey, Contract, CallData)
    end.

-spec sc_leave(channel()) -> {ok, binary()}.
sc_leave(Channel) ->
    #{ initiator := {_, IConn, _}, responder := {_, RConn, _} } = Channel,
    %% if the initiator drops or leaves, the responder waits for a while for it
    %% to reconnect, so we make the responder to leave instead
    ws_send(RConn, <<"leave">>, #{}),
    WaitEvents =
        fun(Conn) ->
            {ok, #{ <<"state">> := LatestState0 }} = sc_wait_for_channel_event(Conn, leave),
            {ok, #{ <<"event">> := <<"died">> }} = sc_wait_for_channel_event(Conn, info),
            LatestState0
        end,
    LatestState = WaitEvents(RConn),
    LatestState = WaitEvents(IConn),
    timer:sleep(300),

    {ok, LatestState}.

%--- NODE FUNCTIONS ------------------------------------------------------------

node_ws_int_addr(NodeName, Cfg) ->
    Url = aest_nodes:get_internal_address(NodeName, ext_ws, Cfg),
    {match, [Host, PortStr]} = re:run(Url, ?WS_URL_REGEX, [{capture, all_but_first, list}]),
    {Host, list_to_integer(PortStr)}.

node_ws_ext_addr(NodeName, Cfg) ->
    Url = aest_nodes:get_service_address(NodeName, ext_ws, Cfg),
    {match, [Host, PortStr]} = re:run(Url, ?WS_URL_REGEX, [{capture, all_but_first, list}]),
    {Host, list_to_integer(PortStr)}.

%--- CHANNEL FUNCTIONS ---------------------------------------------------------

sc_start_ws_peers(INodeName, RNodeName, Opts, Cfg) ->
    {ok, IConn} = sc_start_ws(INodeName, initiator, Opts, Cfg),
    ok = ?WS:register_test_for_channel_events(IConn, [info, sign, get, on_chain_tx, update, leave]),
    {ok, RConn} = sc_start_ws(RNodeName, responder, Opts, Cfg),
    ok = ?WS:register_test_for_channel_events(RConn, [info, sign, get, on_chain_tx, update, leave]),
    {IConn, RConn}.

sc_start_ws(NodeName, Role, Opts, Cfg) ->
    {Host, Port} = node_ws_ext_addr(NodeName, Cfg),
    ?WS:start_channel(Host, Port, Role, maybe_add_fsm_id(Role, Opts)).

maybe_add_fsm_id(Role, #{existing_fsm_id := Props} = Opts) when is_list(Props) ->
    Opts#{existing_fsm_id => proplists:get_value(Role, Props)};
maybe_add_fsm_id(_, Opts) ->
    Opts.

sc_wait_for_channel_event(ConnPid, Action) ->
    case ?WS:wait_for_channel_event(ConnPid, Action) of
        {ok, #{ <<"params">> := #{ <<"data">> := Data }}} ->
            {ok, Data};
        {ok, Tag, #{ <<"params">> := #{ <<"data">> := Data }}} ->
            {ok, Tag, Data}
    end.

sc_deploy_contract(SenderConn, SenderPubKey, SenderPrivKey, AckConn, AckPrivKey, Contract, CallData) ->
    #{ bytecode := Bytecode, vm := Vm, abi := Abi } = Contract,
    ws_send(SenderConn, <<"update.new_contract">>, #{ <<"code">>        => Bytecode
                                                    , <<"call_data">>   => CallData
                                                    , <<"vm_version">>  => Vm
                                                    , <<"abi_version">> => Abi
                                                    , <<"deposit">>     => 0}),
    OffchainTx = sc_process_offchain_tx(SenderConn, SenderPrivKey, AckConn, AckPrivKey),
    {CB, Tx} = aetx:specialize_callback(OffchainTx),
    Round = CB:round(Tx),
    Address = aect_contracts:compute_contract_pubkey(SenderPubKey, Round),
    {ok, Contract#{address => Address}}.

sc_call_contract(SenderConn, SenderPubKey, SenderPrivKey, AckConn, AckPrivKey, Contract, CallData) ->
    #{ address := ContractAddress, abi := Abi } = Contract,
    SenderPub = aeser_api_encoder:encode(account_pubkey, SenderPubKey),
    ContractPub = aeser_api_encoder:encode(contract_pubkey, ContractAddress),
    ws_send(SenderConn, <<"update.call_contract">>, #{ <<"contract_id">> => ContractPub
                                                     , <<"call_data">>   => CallData
                                                     , <<"abi_version">> => Abi
                                                     , <<"amount">>      => 0}),
    OffchainTx = sc_process_offchain_tx(SenderConn, SenderPrivKey, AckConn, AckPrivKey),
    {CB, Tx} = aetx:specialize_callback(OffchainTx),
    Round = CB:round(Tx),
    ws_send(SenderConn, <<"get.contract_call">>, #{ <<"caller_id">>   => SenderPub
                                                  , <<"contract_id">> => ContractPub
                                                  , <<"round">>       => Round}),
    {ok, <<"contract_call">>, CallRes} = sc_wait_for_channel_event(SenderConn, get),
    {ok, CallRes}.

sc_transfer(SenderConn, SenderPubKey, SenderPrivKey, AckConn, AckPubKey, AckPrivKey, Amount) ->
    SenderPub = aeser_api_encoder:encode(account_pubkey, SenderPubKey),
    AckPub = aeser_api_encoder:encode(account_pubkey, AckPubKey),
    ws_send(SenderConn, <<"update.new">>, #{ <<"amount">> => Amount
                                          , <<"from">>   => SenderPub
                                          , <<"to">>     => AckPub}),
    _ = sc_process_offchain_tx(SenderConn, SenderPrivKey, AckConn, AckPrivKey),
    ok.

sc_process_offchain_tx(SenderConn, SenderPrivKey, AckConn, AckPrivKey) ->
    UnsignedStateTx = sc_wait_and_sign(SenderConn, SenderPrivKey, <<"update">>),
    {ok, #{ <<"event">> := <<"update">> }} = sc_wait_for_channel_event(AckConn, info),
    UnsignedStateTx = sc_wait_and_sign(AckConn, AckPrivKey, <<"update_ack">>),

    {ok, #{ <<"state">> := EncodedSignedTx }} = sc_wait_for_channel_event(SenderConn, update),
    {ok, #{ <<"state">> := EncodedSignedTx }} = sc_wait_for_channel_event(AckConn, update),

    %% Assert we received an offchain_update
    {ok, OffchainTxBin} = aeser_api_encoder:safe_decode(transaction, EncodedSignedTx),
    OffchainTx = aetx_sign:deserialize_from_binary(OffchainTxBin),
    Tx = aetx_sign:innermost_tx(OffchainTx),
    {aesc_offchain_tx, _} = aetx:specialize_callback(Tx),
    Tx.

sc_close_mutual(CloserConn, CloserPrivKey, OtherConn, OtherPrivKey) ->
    ws_send(CloserConn, <<"shutdown">>, #{}),
    ShTx = sc_wait_and_sign(CloserConn, CloserPrivKey, <<"shutdown_sign">>),
    ShTx = sc_wait_and_sign(OtherConn, OtherPrivKey, <<"shutdown_sign_ack">>).

sc_withdraw(SenderConn, SenderPrivKey, Amount, AckConn, AckPrivKey) ->
    ws_send(SenderConn, <<"withdraw">>, #{amount => Amount}),
    UnsignedStateTx = sc_wait_and_sign(SenderConn, SenderPrivKey, <<"withdraw_tx">>),
    {ok, #{ <<"event">> := <<"withdraw_created">> }} = sc_wait_for_channel_event(AckConn, info),
    UnsignedStateTx = sc_wait_and_sign(AckConn, AckPrivKey, <<"withdraw_ack">>),
    {ok, #{ <<"tx">> := EncodedSignedWTx }} = sc_wait_for_channel_event(SenderConn, on_chain_tx),
    {ok, #{ <<"tx">> := EncodedSignedWTx }} = sc_wait_for_channel_event(AckConn, on_chain_tx),

    {ok, BinSignedWTx} = aeser_api_encoder:safe_decode(transaction, EncodedSignedWTx),
    SignedWTx = aetx_sign:deserialize_from_binary(BinSignedWTx),
    WTx = aetx_sign:tx(SignedWTx),
    Fee = aetx:fee(WTx),

    ok = sc_wait_channel_changed(SenderConn, AckConn, <<"channel_withdraw_tx">>),

    ok = sc_wait_withdraw_locked(SenderConn, AckConn),

    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedWTx)),

    {ok, TxHash, Fee}.

sc_wait_channel_open(IConn, RConn) ->
    {ok, #{ <<"event">> := <<"channel_open">> }} = sc_wait_for_channel_event(RConn, info),
    {ok, #{ <<"event">> := <<"channel_accept">> }} = sc_wait_for_channel_event(IConn, info),
    ok.

sc_wait_and_sign(Conn, Privkey, Tag) ->
    {ok, Tag, #{ <<"signed_tx">> := EncTx }} = sc_wait_for_channel_event(Conn, sign),
    {ok, BinTx} = aeser_api_encoder:safe_decode(transaction, EncTx),
    SignedTx0 = aetx_sign:deserialize_from_binary(BinTx),
    SignedTx = aec_test_utils:co_sign_tx(SignedTx0, Privkey),
    BinSignedTx = aetx_sign:serialize_to_binary(SignedTx),
    EncSignedTx = aeser_api_encoder:encode(transaction, BinSignedTx),
    ws_send(Conn, Tag, #{signed_tx => EncSignedTx}),
    aetx_sign:tx(SignedTx).

sc_wait_channel_changed(InitiatorConn, ResponderConn, Type) ->
    {ok, #{ <<"info">> := <<"channel_changed">>
          , <<"type">> := Type }}
        = sc_wait_for_channel_event(InitiatorConn, on_chain_tx),
    {ok, #{ <<"info">> := <<"channel_changed">>
          , <<"type">> := Type }}
        = sc_wait_for_channel_event(ResponderConn, on_chain_tx),
    ok.

sc_wait_funding_locked(InitiatorConn, ResponderConn) ->
    {ok, #{ <<"event">> := <<"own_funding_locked">> }} = sc_wait_for_channel_event(InitiatorConn, info),
    {ok, #{ <<"event">> := <<"own_funding_locked">> }} = sc_wait_for_channel_event(ResponderConn, info),
    {ok, #{ <<"event">> := <<"funding_locked">> }} = sc_wait_for_channel_event(InitiatorConn, info),
    {ok, #{ <<"event">> := <<"funding_locked">> }} = sc_wait_for_channel_event(ResponderConn, info),
    ok.

sc_wait_open(IConn, RConn) ->
    sc_wait_open_(IConn, RConn, aesc_create_tx).

sc_wait_reestablish(IConn, RConn) ->
    sc_wait_open_(IConn, RConn, aesc_offchain_tx).

sc_wait_open_(IConn, RConn, Type) ->
    {ok, #{ <<"event">> := <<"open">> }} = sc_wait_for_channel_event(IConn, info),
    {ok, #{ <<"event">> := <<"open">> }} = sc_wait_for_channel_event(RConn, info),

    %% Both peers receive the same initial state
    {ok, #{<<"state">> := InitialState}} = sc_wait_for_channel_event(IConn, update),
    {ok, #{<<"state">> := InitialState}} = sc_wait_for_channel_event(RConn, update),

    %% Assert we received a channel_create_tx
    {ok, InitialStateTxBin} = aeser_api_encoder:safe_decode(transaction, InitialState),
    InitialStateTx = aetx_sign:deserialize_from_binary(InitialStateTxBin),
    Tx = aetx_sign:innermost_tx(InitialStateTx),
    {Type, _} = aetx:specialize_callback(Tx),

    ok.

sc_wait_close(Channel) ->
    #{ initiator := {_IAccount, IConn}
     , responder := {_RAccount, RConn} } = Channel,
    case sc_wait_for_channel_event(IConn, info) of
        {ok, #{ <<"event">> := <<"closing">> }} ->
            {ok, #{ <<"event">> := <<"closed_confirmed">> }} = sc_wait_for_channel_event(IConn, info),
            {ok, #{ <<"event">> := <<"died">> }} = sc_wait_for_channel_event(IConn, info);
        {ok, #{ <<"event">> := <<"close_mutual">> }} ->
            %% initiator just dies
            {ok, #{ <<"event">> := <<"died">> }} = sc_wait_for_channel_event(IConn, info)
    end,
    {ok, #{ <<"event">> := <<"shutdown">> }} = sc_wait_for_channel_event(RConn, info),
    {ok, #{ <<"event">> := <<"closing">> }} = sc_wait_for_channel_event(RConn, info),
    {ok, #{ <<"event">> := <<"closed_confirmed">> }} = sc_wait_for_channel_event(RConn, info),
    {ok, #{ <<"event">> := <<"died">> }} = sc_wait_for_channel_event(RConn, info),
    %% remove timer at own risk
    timer:sleep(1000),
    ok.

sc_wait_withdraw_locked(SenderConn, AckConn) ->
    {ok, #{ <<"event">> := <<"own_withdraw_locked">> }} = sc_wait_for_channel_event(SenderConn, info),
    {ok, #{ <<"event">> := <<"own_withdraw_locked">> }} = sc_wait_for_channel_event(AckConn, info),
    {ok, #{ <<"event">> := <<"withdraw_locked">> }} = sc_wait_for_channel_event(SenderConn, info),
    {ok, #{ <<"event">> := <<"withdraw_locked">> }} = sc_wait_for_channel_event(AckConn, info),

    {ok, #{ <<"state">> := EncodedSignedTx }} = sc_wait_for_channel_event(SenderConn, update),
    {ok, #{ <<"state">> := EncodedSignedTx }} = sc_wait_for_channel_event(AckConn, update),

    %% Assert we received an widthdraw_tx
    {ok, WithdrawTxBin} = aeser_api_encoder:safe_decode(transaction, EncodedSignedTx),
    WithdrawTx = aetx_sign:deserialize_from_binary(WithdrawTxBin),
    Tx = aetx_sign:innermost_tx(WithdrawTx),
    {aesc_withdraw_tx, _} = aetx:specialize_callback(Tx),
    ok.

%--- TRANSACTION FUNCTIONS -----------------------------------------------------

ws_send(ConnPid, Tag, Data) ->
    Method = <<"channels.", Tag/binary>>,
    ?WS:json_rpc_notify(ConnPid,
        #{ <<"method">> => Method
         , <<"params">> => Data}).

%--- HELPER TO MANAGE ENCODING OPTIONS -----------------------------------------

maybe_version_opts(Cfg) ->
    case lists:keyfind(channel_opts, 1, Cfg) of
        {_, Opts} ->
            version_opts(Opts);
        false ->
            #{}
    end.

version_opts(Params) ->
    maps:with([version_offchain_update], Params).

maybe_block_hash_opts(Params) ->
    Find = fun(Key) -> maps:find(Key, Params) end,
    case {Find(bh_delta_not_newer_than),
          Find(bh_delta_not_older_than),
          Find(bh_delta_pick)} of
        {{ok, NNT}, {ok, NOT}, {ok, Pick}} ->
            #{ bh_delta_not_newer_than => NNT
             , bh_delta_not_older_than => NOT
             , bh_delta_pick           => Pick};
        {_, _, _} ->
            #{}
    end.

