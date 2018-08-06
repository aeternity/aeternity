-module(aest_api).

%=== EXPORTS ===================================================================

-export([get_balance/3]).

-export([tx_spend/6]).
-export([tx_state/3]).
-export([tx_wait/5]).

-export([sc_open/2]).
-export([sc_withdraw/3]).
-export([sc_close_mutual/2]).

%=== INCLUDES ==================================================================

-include_lib("eunit/include/eunit.hrl").

%=== MACROS ====================================================================

-define(WS, aehttp_ws_test_utils).
-define(WS_URL_REGEX, "^ws://([a-zA-Z0-9-._]*):([0-9]*)/?$").

%=== TYPES =====================================================================

-type account() :: #{
  pubkey := binary(),
  privkey := binary()
}.

-type tx_state() :: mempool | chain | undefined.

-type sc_open_params() :: #{
    initiator_node    := atom(),
    initiator_account := account(),
    initiator_amount  := non_neg_integer(),
    responder_node    := atom(),
    responder_account := account(),
    responder_amount  := non_neg_integer(),
    responder_port    => pos_integer()
}.

-type channel() :: map().
-type party() :: initiator | responder.

%=== API FUNCTIONS =============================================================

-spec get_balance(atom(), account(), aest_nodes:test_ctx()) -> non_neg_integer().
get_balance(NodeName, Account, Cfg) ->
    api_get_balance(NodeName, Account, Cfg).

-spec tx_spend(atom(), account(), account(), pos_integer(), pos_integer(),
               aest_nodes:test_ctx())
    -> binary().
tx_spend(NodeName, From, To, Amount, Fee, Cfg) ->
    #{ pubkey := FromPubKey } = From,
    #{ pubkey := ToPubKey } = To,
    TxArgs = #{
        sender           => FromPubKey,
        recipient_pubkey => ToPubKey,
        amount           => Amount,
        fee              => Fee,
        payload          => <<"foo">>
    },
    {TxHash, Tx} = tx_prepare(NodeName, spend_tx, From, TxArgs, Cfg),
    tx_post(NodeName, TxHash, Tx, Cfg).

-spec tx_state(atom(), binary(), aest_nodes:test_ctx()) -> tx_state().
tx_state(NodeName, TxHash, Cfg) ->
    case api_get_tx(NodeName, TxHash, json, Cfg) of
        {ok, 404, _} ->
            undefined;
        {ok, 200, #{ <<"transaction">> := #{ <<"block_hash">> := <<"none">> }} } ->
            mempool;
        {ok, 200, #{ <<"transaction">> := #{ <<"block_hash">> := _Other }} } ->
            chain
    end.

-spec tx_wait(atom() | [atom()], binary(), tx_state(), pos_integer(),
              aest_nodes:test_ctx())
    -> ok | timeout.
tx_wait(NodeNames, TxHash, Status, Timeout, Cfg) ->
    CheckFun = fun(NodeName) -> tx_state(NodeName, TxHash, Cfg) =:= Status end,
    wait_for(NodeNames, CheckFun, 100, Timeout).

-spec sc_open(sc_open_params(), aest_nodes:test_ctx())
    -> {ok, channel(), binary(), pos_integer()}.
sc_open(Params, Cfg) ->
    #{
        initiator_node    := INodeName,
        initiator_account := IAccount,
        responder_node    := RNodeName,
        responder_account := RAccount
    } = Params,
    #{ pubkey := IPubKey, privkey := IPrivKey } = IAccount,
    #{ pubkey := RPubKey, privkey := RPrivKey } = RAccount,

    Opts = sc_options(Params, #{}, Cfg),
    {ok, IConn} = sc_start_ws(INodeName, initiator, Opts, Cfg),
    ok = ?WS:register_test_for_channel_events(IConn, [info, sign, on_chain_tx]),
    {ok, RConn} = sc_start_ws(RNodeName, responder, Opts, Cfg),
    ok = ?WS:register_test_for_channel_events(RConn, [info, sign, on_chain_tx]),

    ok = sc_wait_channel_open(IConn, RConn),

    %% initiator gets to sign a create_tx
    CrTx = sc_wait_and_sign(IConn, IPrivKey, <<"initiator_sign">>),
    {ok, #{ <<"event">> := <<"funding_created">>} } = ?WS:wait_for_channel_event(RConn, info),
    %% responder gets to sign a create_tx
    CrTx = sc_wait_and_sign(RConn, RPrivKey, <<"responder_sign">>),
    {ok, #{ <<"event">> := <<"funding_signed">>} } = ?WS:wait_for_channel_event(IConn, info),
    %% both of them receive the same co-signed channel_create_tx
    {ok, #{ <<"tx">> := EncSignedCrTx} } = ?WS:wait_for_channel_event(IConn, on_chain_tx),
    {ok, #{ <<"tx">> := EncSignedCrTx} } = ?WS:wait_for_channel_event(RConn, on_chain_tx),

    {ok, BinSignedCrTx} = aec_base58c:safe_decode(transaction, EncSignedCrTx),
    SignedCrTx = aetx_sign:deserialize_from_binary(BinSignedCrTx),
    %% same transaction
    CrTx = aetx_sign:tx(SignedCrTx),

    {channel_create_tx, Tx} = aetx:specialize_type(CrTx),
    %% Check public keys
    IPubKey = aec_base58c:encode(account_pubkey, aesc_create_tx:initiator_pubkey(Tx)),
    RPubKey = aec_base58c:encode(account_pubkey, aesc_create_tx:responder_pubkey(Tx)),
    Fee = aesc_create_tx:fee(Tx),

    ok = sc_wait_funding_locked(IConn, RConn),

    ok = sc_wait_open(IConn, RConn),

    Channel = #{ initiator => {IAccount, IConn}, responder => {RAccount, RConn} },

    TxHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedCrTx)),

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

    {ok, #{ <<"tx">> := EncSignedMutualTx} } = ?WS:wait_for_channel_event(IConn, on_chain_tx),
    {ok, #{ <<"tx">> := EncSignedMutualTx} } = ?WS:wait_for_channel_event(RConn, on_chain_tx),

    {ok, BinSignedMutualTx} = aec_base58c:safe_decode(transaction, EncSignedMutualTx),
    SignedMutualTx = aetx_sign:deserialize_from_binary(BinSignedMutualTx),
    %% same transaction
    ShutdownTx = aetx_sign:tx(SignedMutualTx),

    {channel_close_mutual_tx, MutualTx} = aetx:specialize_type(ShutdownTx),

    IChange = aesc_close_mutual_tx:initiator_amount_final(MutualTx),
    RChange = aesc_close_mutual_tx:responder_amount_final(MutualTx),

    TxHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedMutualTx)),

    {ok, TxHash, IChange, RChange}.

%=== INTERNAL FUNCTIONS ========================================================

str(A) when is_atom(A) -> str(atom_to_binary(A, utf8));
str(S) when is_list(S); is_binary(S) -> S.

wait_for(NodeName, CheckFun, Interval, Timeout) when is_atom(NodeName) ->
    wait_for([NodeName], CheckFun, Interval, Timeout);
wait_for(NodeNames, CheckFun, Interval, Timeout) when is_list(NodeNames) ->
    TimerRef = erlang:start_timer(Timeout, self(), undefined),
    wait_for(NodeNames, CheckFun, Interval, TimerRef, NodeNames).

wait_for(_Nodes, _CheckFun, _Interval, TimerRef, []) ->
    erlang:cancel_timer(TimerRef),
    receive {timeout, TimerRef, _} -> ok after 0 -> ok end;
wait_for(AllNodes, CheckFun, Interval, TimerRef, [Node | Rest]) ->
    case CheckFun(Node) of
        true -> wait_for(AllNodes, CheckFun, Interval, TimerRef, Rest);
        false ->
            receive
                {timeout, TimerRef, _} -> timeout
            after Interval ->
                wait_for(AllNodes, CheckFun, Interval, TimerRef, AllNodes)
            end
    end.

%--- NODE FUNCTIONS ------------------------------------------------------------

node_http_base_url(NodeName, Cfg) ->
    aest_nodes:get_service_address(NodeName, ext_http, Cfg).

node_ws_int_addr(NodeName, Cfg) ->
    Url = aest_nodes:get_internal_address(NodeName, ext_ws, Cfg),
    {match, [Host, PortStr]} = re:run(Url, ?WS_URL_REGEX, [{capture, all_but_first, list}]),
    {Host, list_to_integer(PortStr)}.

node_ws_ext_addr(NodeName, Cfg) ->
    Url = aest_nodes:get_service_address(NodeName, ext_ws, Cfg),
    {match, [Host, PortStr]} = re:run(Url, ?WS_URL_REGEX, [{capture, all_but_first, list}]),
    {Host, list_to_integer(PortStr)}.

%--- CHANNEL FUNCTIONS ---------------------------------------------------------

sc_options(Params, ExtraOpts, Cfg) ->
    #{
        initiator_account := IAccount,
        initiator_amount  := IAmt,
        responder_node    := RNodeName,
        responder_account := RAccount,
        responder_amount  := RAmt
    } = Params,
    #{ pubkey := IPubKey } = IAccount,
    #{ pubkey := RPubKey } = RAccount,

    {Host, _Port} = node_ws_int_addr(RNodeName, Cfg),
    Opts = #{
        host => Host,
        port => maps:get(responder_port, Params, 9000),
        initiator => IPubKey,
        responder => RPubKey,
        lock_period => maps:get(lock_period, Params, 10),
        push_amount => maps:get(push_amount, Params, 10),
        initiator_amount => IAmt,
        responder_amount => RAmt,
        channel_reserve => maps:get(channel_reserve, Params, 2)
    },
    maps:merge(Opts, ExtraOpts).

sc_start_ws(NodeName, Role, Opts, Cfg) ->
    {Host, Port} = node_ws_ext_addr(NodeName, Cfg),
    ?WS:start_channel(Host, Port, Role, Opts).

sc_close_mutual(CloserConn, CloserPrivKey, OtherConn, OtherPrivKey) ->
    ?WS:send(CloserConn, <<"shutdown">>, #{}),
    ShTx = sc_wait_and_sign(CloserConn, CloserPrivKey, <<"shutdown_sign">>),
    ShTx = sc_wait_and_sign(OtherConn, OtherPrivKey, <<"shutdown_sign_ack">>).

sc_withdraw(SenderConn, SenderPrivKey, Amount, AckConn, AckPrivKey) ->
    ?WS:send(SenderConn, <<"withdraw">>, #{amount => Amount}),
    UnsignedStateTx = sc_wait_and_sign(SenderConn, SenderPrivKey, <<"withdraw_tx">>),
    {ok, #{ <<"event">> := <<"withdraw_created">> }} = ?WS:wait_for_channel_event(AckConn, info),
    UnsignedStateTx = sc_wait_and_sign(AckConn, AckPrivKey, <<"withdraw_ack">>),
    {ok, #{ <<"tx">> := EncodedSignedWTx }} = ?WS:wait_for_channel_event(SenderConn, on_chain_tx),
    {ok, #{ <<"tx">> := EncodedSignedWTx }} = ?WS:wait_for_channel_event(AckConn, on_chain_tx),

    {ok, BinSignedWTx} = aec_base58c:safe_decode(transaction, EncodedSignedWTx),
    SignedWTx = aetx_sign:deserialize_from_binary(BinSignedWTx),
    WTx = aetx_sign:tx(SignedWTx),
    Fee = aetx:fee(WTx),

    ok = sc_wait_withdraw_locked(SenderConn, AckConn),

    TxHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedWTx)),

    {ok, TxHash, Fee}.

sc_wait_channel_open(IConn, RConn) ->
    {ok, #{ <<"event">> := <<"channel_open">> }} = ?WS:wait_for_channel_event(RConn, info),
    {ok, #{ <<"event">> := <<"channel_accept">> }} = ?WS:wait_for_channel_event(IConn, info),
    ok.

sc_wait_and_sign(Conn, Privkey, Tag) ->
    {ok, Tag, #{ <<"tx">> := EncTx }} = ?WS:wait_for_channel_event(Conn, sign),
    {ok, BinTx} = aec_base58c:safe_decode(transaction, EncTx),
    Tx = aetx:deserialize_from_binary(BinTx),
    SignedTx = aec_test_utils:sign_tx(Tx, Privkey),
    BinSignedTx = aetx_sign:serialize_to_binary(SignedTx),
    EncSignedTx = aec_base58c:encode(transaction, BinSignedTx),
    ?WS:send(Conn, Tag, #{tx => EncSignedTx}),
    Tx.

sc_wait_funding_locked(InitiatorConn, ResponderConn) ->
    {ok, #{ <<"event">> := <<"own_funding_locked">> }} = ?WS:wait_for_channel_event(InitiatorConn, info),
    {ok, #{ <<"event">> := <<"own_funding_locked">> }} = ?WS:wait_for_channel_event(ResponderConn, info),
    {ok, #{ <<"event">> := <<"funding_locked">> }} = ?WS:wait_for_channel_event(InitiatorConn, info),
    {ok, #{ <<"event">> := <<"funding_locked">> }} = ?WS:wait_for_channel_event(ResponderConn, info),
    ok.

sc_wait_open(IConn, RConn) ->
    {ok, #{ <<"event">> := <<"open">> }} = ?WS:wait_for_channel_event(IConn, info),
    {ok, #{ <<"event">> := <<"open">> }} = ?WS:wait_for_channel_event(RConn, info),
    ok.

sc_wait_withdraw_locked(SenderConn, AckConn) ->
    {ok, #{ <<"event">> := <<"own_withdraw_locked">> }} = ?WS:wait_for_channel_event(SenderConn, info),
    {ok, #{ <<"event">> := <<"own_withdraw_locked">> }} = ?WS:wait_for_channel_event(AckConn, info),
    {ok, #{ <<"event">> := <<"withdraw_locked">> }} = ?WS:wait_for_channel_event(SenderConn, info),
    {ok, #{ <<"event">> := <<"withdraw_locked">> }} = ?WS:wait_for_channel_event(AckConn, info),
    ok.

%--- TRANSACTION FUNCTIONS -----------------------------------------------------

tx_prepare(NodeName, TxType, Account, Args, Cfg) ->
    #{ privkey := PrivKey } = Account,
    {ok, 200, #{ <<"tx">> := EncodedSerializedUnsignedTx }} = api_post(NodeName, TxType, Args, Cfg),
    {ok, SerializedUnsignedTx} = aec_base58c:safe_decode(transaction, EncodedSerializedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    Signature = enacl:sign_detached(SerializedUnsignedTx, PrivKey),
    SignedTx = aetx_sign:new(UnsignedTx, [Signature]),
    TxHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedTx)),
    EncodedSerializedSignedTx = aec_base58c:encode(transaction, aetx_sign:serialize_to_binary(SignedTx)),
    {TxHash, EncodedSerializedSignedTx}.

tx_post(NodeName, TxHash, Tx, Cfg) ->
    {ok, 200, Resp} = api_post_tx(NodeName, Tx, Cfg),
    ?assertEqual(TxHash, maps:get(<<"tx_hash">>, Resp)),
    TxHash.

tx_encoding_param(default) -> #{};
tx_encoding_param(json) -> #{tx_encoding => <<"json">>};
tx_encoding_param(message_pack) -> #{tx_encoding => <<"message_pack">>}.

%--- HTTP API FUNCTIONS --------------------------------------------------------

api_path(spend_tx) -> "tx/spend".

api_post(NodeName, ApiTag, Params, Cfg) ->
    BaseUrl = node_http_base_url(NodeName, Cfg),
    Path = api_path(ApiTag),
    http_request(BaseUrl, post, Path, Params).

api_get_balance(NodeName, #{ pubkey := PubKey }, Cfg) ->
    BaseUrl = node_http_base_url(NodeName, Cfg),
    Path = binary_to_list(<<"account/", PubKey/binary, "/balance">>),
    {ok, 200, #{ <<"balance">> := Bal }} = http_request(BaseUrl, get, Path, []),
    Bal.

api_get_tx(NodeName, TxHash, TxEncoding, Cfg) ->
    Params = tx_encoding_param(TxEncoding),
    BaseUrl = node_http_base_url(NodeName, Cfg),
    http_request(BaseUrl, get, "tx/" ++ binary_to_list(TxHash), Params).

api_post_tx(NodeName, Tx, Cfg) ->
    BaseUrl = node_http_base_url(NodeName, Cfg),
    http_request(BaseUrl, post, "ng-transactions", #{tx => Tx}).

%--- HTTP BACKEND FUNCTIONS ----------------------------------------------------

http_request(Host, get, Path, Params) ->
    EncodedParams = http_encode_get_params(Params),
    URL = binary_to_list(iolist_to_binary([Host, "v2/", Path, EncodedParams])),
    ct:log("GET ~p", [URL]),
    R = httpc_request(get, {URL, []}, [], []),
    http_process_response(R);
http_request(Host, post, Path, Params) ->
    URL = binary_to_list(iolist_to_binary([Host, "v2/", Path])),
    {Type, Body} = case Params of
                       Map when is_map(Map) ->
                           %% JSON-encoded
                           {"application/json", jsx:encode(Params)};
                       [] ->
                           {"application/x-www-form-urlencoded",
                            http_uri:encode(Path)}
                   end,
    %% lager:debug("Type = ~p; Body = ~p", [Type, Body]),
    ct:log("POST ~p, type ~p, Body ~p", [URL, Type, Body]),
    R = httpc_request(post, {URL, [], Type, Body}, [], []),
    http_process_response(R).

httpc_request(Method, Request, HTTPOptions, Options) ->
    httpc_request(Method, Request, HTTPOptions, Options, test_browser).

httpc_request(Method, Request, HTTPOptions, Options, Profile) ->
    {ok, Pid} = inets:start(httpc, [{profile, Profile}], stand_alone),
    Response = httpc:request(Method, Request, HTTPOptions, Options, Pid),
    ok = gen_server:stop(Pid, normal, infinity),
    Response.

http_process_response(R) ->
    case R of
        {ok, {{_, ReturnCode, _State}, _Head, Body}} ->
            try
                ct:log("Return code ~p, Body ~p", [ReturnCode, Body]),
                Result =
                    case iolist_to_binary(Body) of
                        <<>> ->
                            #{};
                        BodyB ->
                            jsx:decode(BodyB, [return_maps])
                    end,
                {ok, ReturnCode, Result}
            catch
                error:E ->
                    {error, {parse_error, [E, erlang:get_stacktrace()]}}
            end;
        {error, _} = Error ->
            Error
    end.

http_encode_get_params(#{} = Ps) ->
    http_encode_get_params(maps:to_list(Ps));
http_encode_get_params([{K,V}|T]) ->
    ["?", [str(K),"=",http_uenc(V)
           | [["&", str(K1), "=", http_uenc(V1)]
              || {K1, V1} <- T]]];
http_encode_get_params([]) ->
    [].

http_uenc(I) when is_integer(I) ->
    http_uenc(integer_to_list(I));
http_uenc(V) ->
    http_uri:encode(V).
