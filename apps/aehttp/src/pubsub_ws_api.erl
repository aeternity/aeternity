-module(pubsub_ws_api).

-export([protocol/1,
         init/1,
         process_from_client/3,
         process_from_event/3
        ]).

-opaque protocol() :: jsonrpc.
-opaque state() :: map().
-type reply() :: {{error, atom()}, map()} | map().
-type response() :: {reply, reply() | [reply()]} | no_reply | stop.

-export_type([protocol/0,
              state/0,
              response/0]).


%%%===================================================================
%%% Behaviour definition
%%%===================================================================
-callback decode(MsgBin :: binary) -> map().

-callback unpack(Msg :: map()) -> Req :: map().

-callback subscription_reply(SubId :: non_neg_integer(), Msg :: map()) -> binary.

-callback unsubscription_reply(SubId :: non_neg_integer(), Msg :: map()) -> binary.

-callback event_reply(SubId :: non_neg_integer(), Topic ::binary, Msg :: map) -> binary().


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

-spec init(protocol()) -> state().
init(_Protocol) ->
    #{subscriptions => #{}}.

-spec process_from_client(protocol(), binary(), map())
    -> no_reply | {reply, binary()} | stop.
process_from_client(Protocol, MsgBin, State) ->
    Mod = protocol_to_impl(Protocol),
    try_seq([ fun decode/1
            , fun unpack_request/1
            , fun process_incoming/1 ], #{api        => Mod,
                                          state      => State,
                                          msg        => MsgBin}).


-spec process_from_event(protocol(), tuple(), map())
    -> no_reply | {reply, binary()} | stop.
process_from_event(Protocol, Msg, State) ->
    Mod = protocol_to_impl(Protocol),
    try_seq([ fun process_event/1 ], #{api        => Mod,
                                       state      => State,
                                       msg        => Msg}).

protocol_to_impl(Protocol) ->
    case Protocol of
        jsonrpc -> pubsub_ws_api_jsonrpc
    end.

decode(#{msg := Msg, api := Mod} = Data) ->
    try Data#{orig_msg => Mod:decode(Msg)}
    catch
        error:_ ->
            throw({decode_error, parse_error})
    end.

unpack_request(#{orig_msg := Msg, api := Mod} = Data) ->
    Unpacked = Mod:unpack(Msg),
    Data#{unpacked_msg => Unpacked}.

process_incoming(#{api := Mod, unpacked_msg := Msg, state := State}) ->
    process_incoming_request(Mod, Msg, State).

process_incoming_request(Mod, #{topic := Topic, params := Params, is_subscribe := true} = Msg, State) ->
    Result = process_subscription_request(Topic, Params, State),
    SubId = erlang:unique_integer([positive]),
    Response = Mod:subscription_reply(SubId, Msg),
    {reply, Response, add_subscription(SubId, Result, Topic, State)};
process_incoming_request(Mod, #{topic := Topic, params := [SubId], is_subscribe := false} = Msg, State) ->
    process_unsubscription_request(Topic, State),
    Response = Mod:unsubscription_reply(SubId, Msg),
    {reply, Response, remove_subscription(SubId, State)}.

process_subscription_request(<<"top_changed">>, _Params, _State) ->
    %% TODO check if already subscribed
    true = aec_events:subscribe(top_changed),
    top_changed;
process_subscription_request(_, _Params, _State) ->
    throw({decode_error, invalid_request}).

process_unsubscription_request(<<"top_changed">>, _State) ->
    true = aec_events:unsubscribe(top_changed),
    ok;
process_unsubscription_request(_, _State) ->
    throw({decode_error, invalid_request}).

process_event(#{msg := Msg, api := Mod, state := State}) ->
    {EventKey, Event} = transform_event(Msg),
    case lookup_subscription_id_and_topic(EventKey, State) of
        undefined ->
            no_reply;
        {SubId, Topic} ->
            Reply = Mod:event_reply(SubId, Topic, Event),
            {reply, Reply}
    end.

transform_event({gproc_ps_event, top_changed, #{info := #{height := Height, block_hash := BlockHash, block_type := BlockType, prev_hash := _PrevHash}}}) ->
    EncBlockHash = encode_block_hash(BlockType, BlockHash),
    {top_changed, #{height     => Height
                   ,hash       => EncBlockHash}}.

add_subscription(SubId, Key, Topic, #{subscriptions := Subscriptions} = State) ->
    NewSubscriptions = Subscriptions#{{id, SubId} => #{key => Key, topic => Topic}, {key, Key} => #{id => SubId, topic => Topic}},
    State#{subscriptions => NewSubscriptions}.

remove_subscription(SubId, #{subscriptions := Subscriptions} = State) ->
    NewSubscriptions = case maps:get({id, SubId}, Subscriptions, undefined) of
                            #{key := Key} ->
                                maps:remove(Key, maps:remove({id, SubId}, Subscriptions));
                            _ ->
                                Subscriptions
                       end,
    State#{subscriptions => NewSubscriptions}.

lookup_subscription_id_and_topic(Key, #{subscriptions := Subscriptions}) ->
    #{id := Id, topic := Topic} = maps:get({key, Key}, Subscriptions),
    {Id, Topic}.


encode_block_hash(key, Hash) ->
    aeser_api_encoder:encode(key_block_hash, Hash);
encode_block_hash(micro, Hash) ->
    aeser_api_encoder:encode(micro_block_hash, Hash).

try_seq(Seq, #{msg := Msg} = Data0) ->
    try lists:foldl(fun(F, Data) ->
                            F(Data)
                    end, Data0, Seq) of
        no_reply                       -> no_reply;
        {reply, Resp}                  -> {reply, Resp};
        {reply, Resp, NewState}        -> {reply, Resp, NewState}
    catch E:R:StackTrace ->
        case {E, R} of
            {throw, {decode_error, Reason}} ->
                lager:debug("CAUGHT THROW {decode_error, ~p} (Msg = ~p)",
                            [Reason, Msg]),
                no_reply;
            {error, _E} ->
                lager:debug("CAUGHT E=~p / Msg = ~p / ~p", [R, Msg, StackTrace]),
                no_reply
        end
    end.
