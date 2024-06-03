-module(pubsub_ws_api_jsonrpc).

-define(JSONRPC_VERSION, <<"2.0">>).

-export([decode/1,
         unpack/1,
         subscription_reply/2,
         unsubscription_reply/2,
         event_reply/3
        ]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

decode(Msg) ->
    try jsx:decode(Msg, [return_maps])
    catch
        error:_ ->
            throw({decode_error, parse_error})
    end.

unpack(#{ <<"jsonrpc">> := ?JSONRPC_VERSION
        , <<"id">>      := Id } = Msg) ->
    {IsSubscribe, Topic, Params} = unpack_topic(Msg),
    #{id => Id, params => Params, is_subscribe => IsSubscribe, topic => Topic}.

subscription_reply(SubId, #{ id := Id}) ->
    reply(Id, SubId).

unsubscription_reply(_SubId, #{ id := Id}) ->
    reply(Id, true).

event_reply(SubId, Topic, Event) ->
    reply(SubId, Topic, Event).

reply(Id, Result) ->
    encode(#{ <<"jsonrpc">>    => ?JSONRPC_VERSION
            , <<"id">>         => Id
            , <<"result">>     => Result}).

reply(Id, Method, Result) ->
    encode(#{ <<"jsonrpc">>    => ?JSONRPC_VERSION
            , <<"id">>         => Id
            , <<"method">>     => Method
            , <<"result">>     => Result}).

encode(Msg) ->
    jsx:encode(Msg).

unpack_topic(#{ <<"method">>  := <<"subscribe_",Topic/binary>>} = Msg) ->
    Params = maps:get(<<"params">>, Msg, #{}),
    {true, Topic, Params};
unpack_topic(#{ <<"method">>  := <<"unsubscribe_",Topic/binary>>
              , <<"params">>  := [Id] = Params}) when is_integer(Id) ->
    %% Params are mandatory for unsubscribe to find the subscription
    {false, Topic, Params};
unpack_topic(_) ->
    throw({decode_error, invalid_request}).
