-module(aest_api).

%=== EXPORTS ===================================================================

-export([sc_open/2]).
-export([sc_withdraw/3]).
-export([sc_close_mutual/2]).

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
    Opts = #{
        protocol => <<"json-rpc">>,
        host => Host,
        port => maps:get(responder_port, Params, 9000),
        initiator_id => aeser_api_encoder:encode(account_pubkey, IPubKey),
        responder_id => aeser_api_encoder:encode(account_pubkey, RPubKey),
        lock_period => maps:get(lock_period, Params, 10),
        push_amount => maps:get(push_amount, Params, 10),
        initiator_amount => IAmt,
        responder_amount => RAmt,
        channel_reserve => maps:get(channel_reserve, Params, 2)
    },

    {ok, IConn} = sc_start_ws(INodeName, initiator, Opts, Cfg),
    ok = ?WS:register_test_for_channel_events(IConn, [info, sign, on_chain_tx]),
    {ok, RConn} = sc_start_ws(RNodeName, responder, Opts, Cfg),
    ok = ?WS:register_test_for_channel_events(RConn, [info, sign, on_chain_tx]),

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

    ok = sc_wait_channel_changed(IConn, RConn, <<"channel_create_tx">>),

    ok = sc_wait_funding_locked(IConn, RConn),

    ok = sc_wait_open(IConn, RConn),

    Channel = #{ initiator => {IAccount, IConn}, responder => {RAccount, RConn} },

    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedCrTx)),

    {ok, Channel, TxHash, Fee}.

-spec sc_withdraw(channel(), party(), non_neg_integer())
    -> {ok, binary(), non_neg_integer()}.
sc_withdraw(Channel, Origin, Amount)
  when Origin =:= initiator orelse Origin =:= responder ->
    #{ initiator := {IAccount, IConn}, responder := {RAccount, RConn} } = Channel,
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
    #{ initiator := {IAccount, IConn}, responder := {RAccount, RConn} } = Channel,
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

sc_start_ws(NodeName, Role, Opts, Cfg) ->
    {Host, Port} = node_ws_ext_addr(NodeName, Cfg),
    ?WS:start_channel(Host, Port, Role, Opts).

sc_wait_for_channel_event(ConnPid, Action) ->
    case ?WS:wait_for_channel_event(ConnPid, Action) of
        {ok, #{ <<"params">> := #{ <<"data">> := Data }}} ->
            {ok, Data};
        {ok, Tag, #{ <<"params">> := #{ <<"data">> := Data }}} ->
            {ok, Tag, Data}
    end.

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
    {ok, Tag, #{ <<"tx">> := EncTx }} = sc_wait_for_channel_event(Conn, sign),
    {ok, BinTx} = aeser_api_encoder:safe_decode(transaction, EncTx),
    Tx = aetx:deserialize_from_binary(BinTx),
    SignedTx = aec_test_utils:sign_tx(Tx, Privkey),
    BinSignedTx = aetx_sign:serialize_to_binary(SignedTx),
    EncSignedTx = aeser_api_encoder:encode(transaction, BinSignedTx),
    ws_send(Conn, Tag, #{tx => EncSignedTx}),
    Tx.

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
    {ok, #{ <<"event">> := <<"open">> }} = sc_wait_for_channel_event(IConn, info),
    {ok, #{ <<"event">> := <<"open">> }} = sc_wait_for_channel_event(RConn, info),
    ok.

sc_wait_withdraw_locked(SenderConn, AckConn) ->
    {ok, #{ <<"event">> := <<"own_withdraw_locked">> }} = sc_wait_for_channel_event(SenderConn, info),
    {ok, #{ <<"event">> := <<"own_withdraw_locked">> }} = sc_wait_for_channel_event(AckConn, info),
    {ok, #{ <<"event">> := <<"withdraw_locked">> }} = sc_wait_for_channel_event(SenderConn, info),
    {ok, #{ <<"event">> := <<"withdraw_locked">> }} = sc_wait_for_channel_event(AckConn, info),
    ok.

%--- TRANSACTION FUNCTIONS -----------------------------------------------------

ws_send(ConnPid, Tag, Data) ->
    Method = <<"channels.", Tag/binary>>,
    ?WS:json_rpc_notify(ConnPid,
        #{ <<"method">> => Method
         , <<"params">> => Data}).
