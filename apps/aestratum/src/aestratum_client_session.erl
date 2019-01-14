-module(aestratum_client_session).

-export([new/0,
         handle_event/2
        ]).

-ifdef(TEST).
-export([state/1]).
-endif.

-record(state, {
          phase,
          req_id = 0,
          reqs = maps:new(),   %% cache of sent requests
          retries = 0
        }).

-define(MAX_RETRIES, application:get_env(aestratum, max_retries, 3)).
-define(MSG_TIMEOUT, application:get_env(aestratum, timeout, 30000)).

%% API.

new() ->
    #state{phase = connected}.

handle_event({conn, What}, State)  ->
    handle_conn_event(What, State);
handle_event({miner, What}, State) ->
    handle_miner_event(What, State).

%% Internal functions.

handle_conn_event(init, #state{phase = connected} = State) ->
    send_req(configure, State);
handle_conn_event(RawMsg, State) when is_binary(RawMsg) ->
    case aestratum_jsonrpc:decode(RawMsg) of
        {ok, Msg}    -> recv_msg(Msg, State);
        {error, Rsn} -> recv_msg_error(Rsn, State)
    end;
%% TODO: {reconnect, Host, Port, WaitTime},...
handle_conn_event({timeout, Id, Phase}, State) ->
    handle_conn_timeout(Id, Phase, State);
handle_conn_event(close, #state{reqs = Reqs} = State) ->
    %% TODO: reason, log
    {stop, State#state{phase = disconnected, reqs = clean_reqs(Reqs)}}.

handle_miner_event(_What, State) ->
    %% TODO
    {no_send, State}.

%% Handle received messages.

recv_msg(#{type := rsp, id := Id} = Rsp, #state{reqs = Reqs} = State) ->
    case find_req(Id, Reqs) of
        {_TRef, _Phase, #{method := Method}} ->
            %% Received response with correct Id.
            case aestratum_jsonrpc:validate_rsp(Method, Rsp) of
                {ok, Rsp1} ->
                    %% Response validation success. The request to which the
                    %% response was sent is deleted from the sent requests and
                    %% timer is cancelled, too.
                    recv_msg1(Rsp1, State#state{reqs = del_req(Id, Reqs)});
                {error, Rsn} ->
                    %% Response validation error.
                    recv_msg_error(Rsn, State)
            end;
        not_found ->
            %% Received unexpected response (no matching Id in sent requests).
            %% TODO: log
            {no_send, State}
    end;
recv_msg(#{type := ntf} = _Ntf, State) ->
    {no_send, State};
recv_msg(#{type := req, method := reconnect} = Req, State) ->
    %% TODO
    {no_send, State}.

%% Handle decoded (without error) message from server.

recv_msg1(#{method := configure, result := []},
          #state{phase = connected} = State) ->
    %% TODO: configure has no params (yet).
    send_req(subscribe, State#state{phase = configured, retries = 0});
recv_msg1(#{method := subscribe, result := [SessionId, ExtraNonce]},
          #state{phase = configured} = State) ->
    %% TODO: log successful subscribe
    %% TODO: save SessionId(?) and ExtraNonce
    send_req(authorize, State#state{phase = subscribed, retries = 0});
recv_msg1(#{method := authorize, result := true},
          #state{phase = subscribed} = State) ->
    %% TODO: log authorization success
    {no_send, State#state{phase = authorized, retries = 0}};
recv_msg1(#{method := authorize, result := false},
          #state{phase = subscribed} = State) ->
    %% TODO: log invalid user/password
    {stop, State#state{phase = disconnected, retries = 0}};
recv_msg1(#{method := submit, result := true},
          #state{phase = authorized} = State) ->
    %% TODO: log successful submit
    {no_send, State};
recv_msg1(#{method := submit, result := false},
          #state{phase = authorized} = State) ->
    %% TODO: log unsuccessful submit
    {no_send, State};
recv_msg1(#{method := Method, reason := Rsn, msg := ErrMsg,
            data := ErrData}, State) ->
    %% TODO: log error response
    %% TODO: maybe retry
    {no_send, State}.

%% Handle badly encoded/invalid messages from server.

recv_msg_error(parse_error, #state{reqs = Reqs} = State) ->
    %% TODO: log
    {stop, State#state{phase = disconnected, reqs = clean_reqs(Reqs)}};
recv_msg_error({invalid_msg, _MaybeId}, #state{reqs = Reqs} = State) ->
    %% TODO: log
    {stop, State#state{phase = disconnected, reqs = clean_reqs(Reqs)}};
recv_msg_error({invalid_method, _MaybeId}, #state{reqs = Reqs} = State) ->
    %% TODO: log
    {stop, State#state{phase = disconnected, reqs = clean_reqs(Reqs)}};
recv_msg_error({invalid_param, _MaybeId, _Param}, #state{reqs = Reqs} = State) ->
    %% TODO: log
    {stop, State#state{phase = disconnected, reqs = clean_reqs(Reqs)}};
recv_msg_error({internal_error, _MaybeId}, #state{reqs = Reqs} = State) ->
    %% TODO: log
    {stop, State#state{phase = disconnected, reqs = clean_reqs(Reqs)}}.

%% Handle timeout.

handle_conn_timeout(Id, connected, #state{phase = connected, reqs = Reqs} = State) ->
    send_req(configure, State#state{reqs = del_req(Id, Reqs)});
handle_conn_timeout(Id, configured, #state{phase = configured, reqs = Reqs} = State) ->
    send_req(subscribe, State#state{reqs = del_req(Id, Reqs)});
handle_conn_timeout(Id, subscribed, #state{phase = subscribed, reqs = Reqs} = State) ->
    send_req(authorize, State#state{reqs = del_req(Id, Reqs)});
%% This timeout was set in one phase and got triggered when the session moved
%% to a next phase (it got triggered during phase switch). The timeout is not
%% valid anymore, there is not action required.
handle_conn_timeout(Id, Phase, #state{phase = Phase1, reqs = Reqs} = State) ->
    {no_send, State#state{reqs = del_req(Id, Reqs)}}.

%% Client to server requests.

send_req(ReqType, #state{retries = Retries, reqs = Reqs} = State) ->
    case Retries > ?MAX_RETRIES of
        true ->
            {stop, State#state{phase = disconnected, reqs = clean_reqs(Reqs)}};
        false ->
            State1 = State#state{retries = Retries + 1},
            case ReqType of
                configure -> send_configure_req(State1);
                subscribe -> send_subscribe_req(State1);
                authorize -> send_authorize_req(State1);
                submit    -> send_submit_req(State1)
            end
    end.

send_configure_req(#state{req_id = Id, reqs = Reqs} = State) ->
    ReqMap = #{type => req, method => configure, id => Id, params => []},
    {send, encode(ReqMap),
     State#state{req_id = next_id(Id),
                 reqs = add_req(Id, connected, ReqMap, Reqs)}}.

send_subscribe_req(#state{req_id = Id, reqs = Reqs} = State) ->
    UserAgent = <<"ae/0.0.1">>,
    Host = <<"localhost">>,
    Port = 9999,
    ReqMap = #{type => req, method => subscribe, id => Id,
               user_agent => UserAgent, session_id => null, host => Host,
               port => Port},
    {send, encode(ReqMap),
     State#state{req_id = next_id(Id),
                 reqs = add_req(Id, configured, ReqMap, Reqs)}}.

send_authorize_req(#state{req_id = Id, reqs = Reqs} = State) ->
    User = <<"ae_user">>,
    Password = binary:copy(<<"0">>, 64),
    ReqMap = #{type => req, method => authorize, id => Id,
               user => User, password => Password},
    {send, encode(ReqMap),
     State#state{req_id = next_id(Id),
                 reqs = add_req(Id, subscribed, ReqMap, Reqs)}}.

send_submit_req(#state{req_id = Id, reqs = Reqs} = State) ->
    User = <<"ae_user">>,
    JobId = <<"0123456789abcdef">>,
    MinerNonce = <<"012356789">>,
    Pow = lists:seq(1, 42),
    ReqMap = #{type => req, method => submit, id => Id,
               user => User, job_id => JobId, miner_nonce => MinerNonce,
               pow => Pow},
    {send, encode(ReqMap),
     State#state{req_id = next_id(Id),
                 reqs = add_req(Id, authorized, ReqMap, Reqs)}}.

%% Helper functions.

add_req(Id, Phase, Req, Reqs) ->
    TRef = erlang:send_after(?MSG_TIMEOUT, self(), {timeout, Id, Phase}),
    maps:put(Id, {TRef, Phase, Req}, Reqs).

find_req(Id, Reqs) ->
    maps:get(Id, Reqs, not_found).

del_req(Id, Reqs) ->
    case maps:get(Id, Reqs, not_found) of
        {TRef, _Phase, _Req} ->
            erlang:cancel_timer(TRef),
            maps:remove(Id, Reqs);
        not_found ->
            Reqs
    end.

clean_reqs(Reqs) ->
    lists:foreach(fun({_Id, {TRef, _Phase, _Req}}) ->
                          erlang:cancel_timer(TRef)
                  end, maps:to_list(Reqs)),
    maps:new().

next_id(Id) ->
    aestratum_jsonrpc_utils:next_id(Id).

encode(Map) ->
    {ok, RawMsg} = aestratum_jsonrpc:encode(Map),
    RawMsg.

%% Used for testing.

-ifdef(TEST).
state(#state{phase = Phase, req_id = ReqId, reqs = Reqs, retries = Retries}) ->
    #{phase   => Phase,
      req_id  => ReqId,
      reqs    => maps:map(fun(Id, {_TRef, Phase, _Req}) -> Phase end, Reqs),
      retries => Retries}.
-endif.

