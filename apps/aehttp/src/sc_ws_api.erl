-module(sc_ws_api).

-define(PROTOCOLS, [jsonrpc]).

%TODO type -> opaque
-opaque protocol() :: jsonrpc.
-type reply() :: {{error, atom()}, map()} | map().
-type response() :: {reply, reply() | [reply()]} | no_reply | stop.

-export_type([protocol/0,
              response/0]).

-export([protocol/1,
         process_from_client/4,
         process_from_fsm/3
        ]).

-export([patterns/0]).

-include_lib("trace_runner/include/trace_runner.hrl").


%%%===================================================================
%%% Behaviour definition
%%%===================================================================

-callback unpack(Msg :: map() | list(map())) ->
    Req :: map() | list(Req :: map()).

-callback error_response(Reason :: atom(), OrigMsg :: map() | binary(),
                         ChannelId :: binary()) ->
    {reply, map()}.

-callback reply(response(), OrigMsg :: map(), ChannelId :: binary()) ->
    response().

-callback notify(map(), ChannelId :: binary()) ->
    {reply, map()}.

-callback process_incoming(Msg :: map() | list(map()), FsmPid :: pid()) ->
    response().

%%%==================================================================
%%% Trace settings
%%%==================================================================

patterns() ->
    [{sc_ws_handler, init, 2, []} |
     [{?MODULE, F, A, []} || {F, A} <- [ {protocol, 0}
                                       , {response, 0}
                                       , {process_from_client, 4}
                                       , {process_from_fsm, 3}
                                       ]]].


%%%===================================================================
%%% API
%%%===================================================================

-spec protocol(binary()) -> protocol().
protocol(P) ->
    case P of
        <<"json-rpc">> -> jsonrpc;
        _Other ->
            erlang:error(invalid_protocol)
    end.

-spec process_from_client(protocol(), binary(), pid(), binary())
    -> no_reply | {reply, binary()} | stop.
process_from_client(Protocol, MsgBin, FsmPid, ChannelId) ->
    Mod = protocol_to_impl(Protocol),
    try_seq([ fun jsx_decode/1
            , fun unpack_request/1
            , fun process_incoming/1 ], #{api        => Mod,
                                          fsm        => FsmPid,
                                          msg        => MsgBin,
                                          channel_id => non_undefined_channel_id(ChannelId)}).

process_from_fsm(Protocol, Msg, ChannelId) ->
    try_seq([ fun process_fsm/1], #{protocol   => Protocol,
                                    msg        => Msg,
                                    channel_id => non_undefined_channel_id(ChannelId)}).

protocol_to_impl(Protocol) ->
    case Protocol of
        jsonrpc -> sc_ws_api_jsonrpc
    end.

notify(Protocol, Msg, ChannelId) ->
    Mod = protocol_to_impl(Protocol),
    Mod:notify(Msg, non_undefined_channel_id(ChannelId)).

unpack_request(#{orig_msg := Msg, api := Mod} = Data) ->
    Unpacked = Mod:unpack(Msg),
    Data#{unpacked_msg => Unpacked}.

-spec try_seq(list(), map()) -> no_reply |
                                {reply, binary()} |
                                stop.
try_seq(Seq, #{msg := Msg} = Data0) ->
    try lists:foldl(fun(F, Data) ->
                            F(Data)
                    end, Data0, Seq) of
        no_reply             -> no_reply;
        {ok, _Data}          -> no_reply;
        {reply, Resp}        -> {reply, Resp};
        stop                 -> stop
    catch
        throw:{decode_error, Reason} ->
            lager:debug("CAUGHT THROW {decode_error, ~p} (Msg = ~p)",
                        [Reason, Msg]),
            no_reply;
        throw:{die_anyway, E} ->
            lager:debug("CAUGHT THROW E = ~p / Msg = ~p / ~p",
                        [E, Msg, erlang:get_stacktrace()]),
            erlang:error(E);
        error:E ->
            lager:debug("CAUGHT E=~p / Msg = ~p / ~p", [E, Msg, erlang:get_stacktrace()]),
            no_reply
    end.

jsx_decode(#{msg := Msg} = Data) ->
    try Data#{orig_msg => jsx:decode(Msg, [return_maps])}
    catch
        error:_ ->
            throw({decode_error, parse_error})
    end.

process_incoming(#{api := Mod, unpacked_msg := Msg, fsm := FsmPid,
                   channel_id := ChannelId}) ->
    Response = Mod:process_incoming(Msg, FsmPid),
    Mod:reply(Response, Msg, ChannelId).


process_fsm(#{msg := Msg,
              channel_id := ChannelId,
              protocol := Protocol}) ->
    process_fsm_(Msg, ChannelId, Protocol).

-spec process_fsm_(term(), binary(), protocol()) -> no_reply | {reply, map()} | {error, atom()}.
process_fsm_(#{type := sign,
               tag  := Tag,
               info := #{tx := Tx, 
                         updates := Updates}},
                ChannelId, Protocol) when Tag =:= create_tx
                                   orelse Tag =:= deposit_tx
                                   orelse Tag =:= deposit_created
                                   orelse Tag =:= withdraw_tx
                                   orelse Tag =:= withdraw_created
                                   orelse Tag =:= shutdown
                                   orelse Tag =:= shutdown_ack
                                   orelse Tag =:= funding_created
                                   orelse Tag =:= update
                                   orelse Tag =:= update_ack
                                   orelse Tag =:= slash_tx
                                   orelse Tag =:= close_solo_tx
                                   orelse Tag =:= settle_tx ->
    EncTx = aeser_api_encoder:encode(transaction, aetx:serialize_to_binary(Tx)),
    SerializedUpdates = [aesc_offchain_update:for_client(U) || U <- Updates],
    Tag1 =
        case Tag of
            create_tx -> <<"initiator_sign">>;
            funding_created -> <<"responder_sign">>;
            shutdown -> <<"shutdown_sign">>;
            shutdown_ack -> <<"shutdown_sign_ack">>;
            deposit_created -> <<"deposit_ack">>;
            withdraw_created -> <<"withdraw_ack">>;
            T -> atom_to_binary(T, utf8)
        end,
    notify(Protocol,
           #{action  => <<"sign">>,
             tag => Tag1,
             payload => #{tx => EncTx,
                          updates => SerializedUpdates}},
           ChannelId);
process_fsm_(#{type := report,
               tag  := Tag,
               info := Event}, ChannelId, Protocol) when Tag =:= info
                                                  orelse Tag =:= update
                                                  orelse Tag =:= conflict
                                                  orelse Tag =:= message
                                                  orelse Tag =:= leave
                                                  orelse Tag =:= error
                                                  orelse Tag =:= debug
                                                  orelse Tag =:= on_chain_tx ->
    Payload =
        case {Tag, Event} of
            {info, {died, _}} -> #{event => <<"died">>};
            {info, _} when is_atom(Event) -> #{event => atom_to_binary(Event, utf8)};
            {on_chain_tx, #{tx := Tx} = Info} ->
                EncodedTx = aeser_api_encoder:encode(
                              transaction,
                              aetx_sign:serialize_to_binary(Tx)),
                Info#{tx => EncodedTx};
            {_, NewState} when Tag == update; Tag == leave ->
                Bin = aeser_api_encoder:encode(transaction,
                                         aetx_sign:serialize_to_binary(NewState)),
                #{state => Bin};
            {conflict, #{channel_id := ChId,
                         round      := Round}} ->
                         #{channel_id => aeser_api_encoder:encode(channel, ChId),
                           round => Round};
            {message, #{channel_id  := ChId,
                        from        := From,
                        to          := To,
                        info        := Info}} ->
                #{message => #{channel_id => aeser_api_encoder:encode(channel, ChId),
                               from => aeser_api_encoder:encode(account_pubkey, From),
                               to => aeser_api_encoder:encode(account_pubkey, To),
                               info => Info}};
            {error, Msg} -> #{message => Msg};
            {debug, Msg} -> #{message => Msg}
        end,
    Action = atom_to_binary(Tag, utf8),
    notify(Protocol,
           #{action => Action,
             payload => Payload},
           ChannelId);
process_fsm_(#{type := Type, tag := Tag, info := Event}, _, _) ->
    error({unparsed_fsm_event, Type, Tag, Event}).

non_undefined_channel_id(undefined) -> null;
non_undefined_channel_id(Val)       -> Val.

