-module(sc_ws_api_legacy).

-behavior(sc_ws_api).

-export([unpack/1,
         error_response/3,
         reply/3,
         notify/2,
         process_incoming/2
        ]).

-define(VERSION, 1).

-spec unpack(map()) -> map().
unpack(#{ <<"action">>   := Action
        ,  <<"tag">>     := Tag
        ,  <<"payload">> := Payload }) ->
    Method = legacy_to_method_in(Action, Tag),
    #{ <<"method">> => Method
     , <<"params">> => Payload };
unpack(#{ <<"action">>  := Action
        , <<"payload">> := Payload}) ->
    Method = legacy_to_method_in(Action),
    #{ <<"method">> => Method
     , <<"params">> => Payload};
unpack(#{ <<"action">> := Action }) ->
    Method = legacy_to_method_in(Action),
    #{ <<"method">> => Method }.
    

error_response(Reason, Req, ChannelId) ->
    {reply, #{ <<"action">>     => <<"error">>
             , <<"version">>    => ?VERSION
             , <<"channel_id">> => ChannelId
             , <<"payload">>    => #{ <<"request">> => Req
                                    , <<"reason">> => legacy_error_reason(Reason)} }
    }.

legacy_to_method_in(Action) ->
    <<"channels.", Action/binary>>.

legacy_to_method_in(Action, Tag) ->
    <<"channels.", Action/binary, ".", Tag/binary>>.

bin(A) when is_atom(A)   -> atom_to_binary(A, utf8);
bin(B) when is_binary(B) -> B.

%% this should be generalized more
legacy_error_reason({broken_encoding, [accounts, contracts]}) ->
    <<"broken_encoding: accounts, contracts">>;
legacy_error_reason({broken_encoding, [accounts]}) ->
    <<"broken_encoding: accounts">>;
legacy_error_reason({broken_encoding, [contracts]}) ->
    <<"broken_encoding: contracts">>;
legacy_error_reason(Reason) ->
    bin(Reason).

notify(Msg, ChannelId) ->
    {reply, clean_reply(Msg, ChannelId)}.

reply(no_reply, _, _) -> no_reply;
reply(stop, _, _)     -> stop;
reply({reply, Reply}, _, ChannelId) ->
    {reply, clean_reply(Reply, ChannelId)};
reply({error, Err}, Req, ChannelId) ->
    error_response(Err, Req, ChannelId).

clean_reply(Map, ChannelId) ->
    clean_reply_(Map#{channel_id => ChannelId,
                      version    => ?VERSION}).

clean_reply_(Map) when is_map(Map) ->
    maps:filter(fun(K,_) ->
                        is_atom(K) orelse is_binary(K)
                end, Map).

process_incoming(Req, FsmPid) ->
    try sc_ws_api_jsonrpc:process_request(Req, FsmPid) of
        {error, _} =Err-> Err;
        no_reply       -> no_reply;
        {reply, Reply} -> {reply, Reply}
    catch
        error:{validation_error, _Name, invalid_number} ->
            {error, invalid_number};
        error:E ->
            lager:debug("CAUGHT E=~p / Req = ~p / ~p",
                        [E, Req, erlang:get_stacktrace()]),
            no_reply
    end.


