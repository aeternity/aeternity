-module(aestratum_jsonrpc).

-include("aestratum_jsonrpc.hrl").

-export([decode/1,
         encode/1,
         validate_rsp/2
        ]).

-type raw_msg()         :: binary().

-type req()             :: configure_req()
                         | subscribe_req()
                         | authorize_req()
                         | submit_req()
                         | reconnect_req().

-type ntf()             :: set_target_ntf()
                         | notify_ntf().

-type raw_rsp()         :: raw_result_rsp()
                         | raw_error_rsp().

-type rsp()             :: configure_rsp()
                         | subscribe_rsp()
                         | authorize_rsp()
                         | submit_rsp().

-type configure_req()   :: #{type          => req,
                             method        => configure,
                             id            => id(),
                             params        => []}.

-type subscribe_req()   :: #{type          => req,
                             method        => subscribe,
                             id            => id(),
                             user_agent    => user_agent(),
                             session_id    => session_id(),
                             host          => host(),
                             port          => integer_port()}.

-type authorize_req()   :: #{type          => req,
                             method        => authorize,
                             id            => id(),
                             user          => user(),
                             password      => password()}.

-type submit_req()      :: #{type          => req,
                             method        => submit,
                             id            => id(),
                             user          => user(),
                             job_id        => job_id(),
                             miner_nonce   => miner_nonce(),
                             pow           => pow()}.

-type reconnect_req()   :: #{type          => req,
                             method        => reconnect,
                             id            => id(),
                             host          => host(),
                             port          => maybe_null_port(),
                             wait_time     => wait_time()}.

-type set_target_ntf()  :: #{type          => ntf,
                             method        => set_target,
                             target        => target()}.

-type notify_ntf()      :: #{type          => ntf,
                             method        => notify,
                             job_id        => job_id(),
                             block_version => block_version(),
                             block_hash    => block_hash(),
                             empty_queue   => empty_queue()}.

-type configure_rsp()   :: #{type          => rsp,
                             method        => configure,
                             id            => id(),
                             result        => []}.

-type subscribe_rsp()   :: #{type          => rsp,
                             method        => subscribe,
                             id            => id(),
                             result        => nonempty_improper_list(
                                                session_id(), extra_nonce())}.

-type authorize_rsp()   :: #{type          => rsp,
                             method        => authorize,
                             id            => id(),
                             result        => boolean()}.

-type submit_rsp()      :: #{type          => rsp,
                             method        => submit,
                             id            => id(),
                             result        => boolean()}.

-type raw_result_rsp()  :: #{type          => rsp,
                             id            => id(),
                             result        => term()}.

-type raw_error_rsp()   :: #{type          => rsp,
                             id            => id() | null,
                             error         => term()}.

%% If there was an error in detecting the id in the Request object (e.g. Parse
%% error/Invalid Request), it MUST be Null.
-type error_dec_rsp()   :: #{type          => rsp,
                             method        => rsp_method(),
                             id            => id() | null,
                             reason        => error_reason(),
                             msg           => error_msg(),
                             data          => error_data()}.

%% Error response to be encoded. Id is allowed to be null since the request
%% (due to which this response is constructed) didn't have to have the Id
%% specified (or there was another error so the Id is unknown). We still try
%% to send a response to a request without Id.
-type error_enc_rsp()   :: #{type          => rsp,
                             method        => rsp_method(),
                             id            => id() | null,
                             reason        => error_reason(),
                             data          => error_data()}.

-type id()              :: ?ID_MIN..?ID_MAX.

-type user_agent()      :: binary().

-type session_id()      :: binary()
                         | null.

-type host()            :: binary()
                         | null.

-type integer_port()    :: ?PORT_MIN..?PORT_MAX.

-type maybe_null_port() :: integer_port()
                         | null.

-type user()            :: binary().

-type password()        :: binary().
                %% TODO: | null

-type job_id()          :: binary().

-type block_version()   :: ?BLOCK_VERSION_MIN..?BLOCK_VERSION_MAX.

-type block_hash()      :: binary().

-type miner_nonce()     :: binary().

-type extra_nonce()     :: binary().

-type pow()             :: [?POW_NUMBER_MIN..?POW_NUMBER_MAX].

-type target()          :: binary().

-type empty_queue()     :: boolean().

-type wait_time()       :: ?WAIT_TIME_MIN..?WAIT_TIME_MAX
                         | null.

-type rsp_method()      :: configure
                         | subscribe
                         | authorize
                         | submit
                         | undefined.

-type error_reason()    :: jsonrpc_reason()
                         | stratum_reason().

-type jsonrpc_reason()  :: parse_error
                         | invalid_msg
                         | invalid_method
                         | invalid_param
                         | internal_error.

-type stratum_reason()  :: unknown_error
                         | job_not_found
                         | duplicate_share
                         | low_difficulty_share
                         | unauthorized_worker
                         | not_subscribed.

-type error_msg()       :: binary()
                         | null.

-type error_data()      :: term()
                         | null.

-type reason()          :: parse_error
                         | {invalid_msg, maybe_id()}
                         | {invalid_method, maybe_id()}
                         | {invalid_param, param(), maybe_id()}
                         | {internal_error, maybe_id()}.

-type param()           :: atom().

-type maybe_id()        :: id()
                         | null
                         | undefined.

-define(JSONRPC_VERSION, <<"2.0">>).

%% API.

-spec decode(raw_msg()) ->
    {ok, req() | ntf() | raw_rsp()} | {error, reason()}.
decode(RawMsg) when is_binary(RawMsg) ->
    %% raw_msg -> msg -> map
    Data0 = #{op => decode, raw_msg => RawMsg},
    case raw_msg_to_msg(Data0) of
        {ok, Data} ->
            run([fun check_version/1,
                 fun msg_to_map/1], Data);
        {error, Rsn} ->
            decode_error(Rsn, Data0)
    end.

-spec encode(req() | ntf() | rsp() | error_enc_rsp()) ->
    {ok, raw_msg()} | {error, reason()}.
encode(Map) when is_map(Map) ->
    %% map -> msg -> raw_msg
    run([fun map_to_msg/1,
         fun msg_to_raw_msg/1], #{op => encode, map => Map}).

-spec validate_rsp(rsp_method(), raw_rsp()) ->
    {ok, rsp() | error_dec_rsp()} | {error, reason()}.
validate_rsp(Method, #{result := Result} = Rsp) ->
    try
        ok = check_result(Method, Result),
        {ok, Rsp#{method => Method}}
    catch
        throw:{validation_error, Rsn} ->
            validation_error(Rsn, #{op => validate_rsp, map => Rsp})
    end;
validate_rsp(Method, #{error := Error} = Rsp) ->
    try
        ok = check_error(Error),
        [Code, Msg, Data] = Error,
        %% replace error with reason, msg, data
        Rsp1 = maps:without([error], Rsp),
        {ok, Rsp1#{method => Method, reason => error_code_to_reason(Code),
                   msg => Msg, data => Data}}
    catch
        throw:{validation_error, Rsn} ->
            validation_error(Rsn, #{op => validate_rsp, map => Rsp})
    end.

%% Internal functions.

run(Funs, Data0) ->
    try
        {ok, lists:foldl(fun(Fun, Data) -> Fun(Data) end, Data0, Funs)}
    catch
        throw:{validation_error, Rsn} ->
            validation_error(Rsn, Data0);
        throw:{encode_error, Rsn} ->
            encode_error(Rsn, Data0);
        error:Rsn ->
            ST = erlang:get_stacktrace(),
            internal_error(Rsn, ST, Data0)
    end.

validation_error(Rsn, #{op := decode} = Data) ->
    lager:warning("Validation error, reason: ~p", [Rsn]),
    validation_erorr1(Rsn, id_from_msg(Data));
validation_error(Rsn, #{op := Op} = Data) when
      (Op =:= encode) or (Op =:= validate_rsp) ->
    lager:warning("Validation error, reason: ~p", [Rsn]),
    validation_erorr1(Rsn, id_from_map(Data)).

validation_erorr1(invalid_msg, Id) ->
    {error, {invalid_msg, Id}};
validation_erorr1({field, Field}, Id) when
      (Field =:= jsonrpc) or (Field =:= id) ->
    {error, {invalid_msg, Id}};
validation_erorr1({field, method}, Id) ->
    {error, {invalid_method, Id}};
validation_erorr1({param, Param}, Id) ->
    {error, {invalid_param, Param, Id}}.

decode_error(Rsn, #{op := decode, raw_msg := RawMsg}) ->
    lager:warning("Decode error, reason: ~p, message: ~p", [Rsn, RawMsg]),
    {error, parse_error}.

encode_error(Rsn, #{op := encode, map := Map}) ->
    lager:warning("Encode error, reason: ~p, map: ~p", [Rsn, Map]),
    {error, parse_error}.

internal_error(Rsn, ST, #{op := decode, raw_msg := RawMsg} = Data) ->
    lager:error(
      "Internal error, reason: ~p, message: ~p, stacktrace: ~9999p",
      [Rsn, RawMsg, ST]),
    {error, {internal_error, id_from_msg(Data)}};
internal_error(Rsn, ST, #{op := encode, map := Map} = Data) ->
    lager:error(
      "Internal error, reason: ~p, map: ~p, stacktrace: ~9999p",
      [Rsn, Map, ST]),
    {error, {internal_error, id_from_map(Data)}}.

id_from_msg(#{msg := #{<<"id">> := Id}}) when ?IS_ID(Id) -> Id;
id_from_msg(#{msg := #{<<"id">> := null}}) -> null;
id_from_msg(_Other) -> undefined.

id_from_map(#{map := #{id := Id}}) when ?IS_ID(Id) -> Id;
id_from_map(#{map := #{id := null}}) -> null;
id_from_map(_Other) -> undefined.

raw_msg_to_msg(#{raw_msg := RawMsg} = Data) ->
    try {ok, Data#{msg => jsx:decode(RawMsg, [return_maps])}}
    catch
        error:Rsn -> {error, Rsn}
    end.

msg_to_raw_msg(#{msg := Msg}) ->
    try list_to_binary([jsx:encode(Msg), $\n])
    catch
        error:Rsn -> throw({encode_error, Rsn})
    end.

check_version(#{msg := #{<<"jsonrpc">> := ?JSONRPC_VERSION}} = Data) ->
    Data;

check_version(#{msg := _Msg}) ->
    validation_exception({field, jsonrpc}).

%% Client requests
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"method">> := <<"mining.configure">>,
                      <<"params">> := Params}}) ->
    ok = check_configure_req(Id, Params),
    #{type => req, method => configure, id => Id, params => Params};
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"method">> := <<"mining.subscribe">>,
                      <<"params">> := Params}}) ->
    ok = check_subscribe_req(Id, Params),
    [UserAgent, SessionId, Host, Port] = Params,
    #{type => req, method => subscribe, id => Id, user_agent => UserAgent,
      session_id => lowercase(SessionId), host => Host,
      port => Port};
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"method">> := <<"mining.authorize">>,
                      <<"params">> := Params}}) ->
    ok = check_authorize_req(Id, Params),
    [User, Password] = Params,
    #{type => req, method => authorize, id => Id, user => User,
      password => lowercase(Password)};
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"method">> := <<"mining.submit">>,
                      <<"params">> := Params}}) ->
    ok = check_submit_req(Id, Params),
    [User, JobId, MinerNonce, Pow] = Params,
    #{type => req, method => submit, id => Id, user => User,
      job_id => lowercase(JobId), miner_nonce => lowercase(MinerNonce),
      pow => Pow};
%% Server requests
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"method">> := <<"client.reconnect">>,
                      <<"params">> := Params}}) ->
    ok = check_reconnect_req(Id, Params),
    case Params of
        [] ->
            #{type => req, method => reconnect, id => Id, host => null,
              port => null, wait_time => 0};
        [Host, Port, WaitTime] ->
            #{type => req, method => reconnect, id => Id, host => Host,
              port => Port, wait_time => WaitTime}
    end;
%% Server notifications
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"method">> := <<"mining.set_target">>,
                      <<"params">> := Params}}) ->
    ok = check_set_target_ntf(Id, Params),
    [Target] = Params,
    #{type => ntf, method => set_target, id => null,
      target => lowercase(Target)};
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"method">> := <<"mining.notify">>,
                      <<"params">> := Params}}) ->
    ok = check_notify_ntf(Id, Params),
    [JobId, BlockVersion, BlockHash, EmptyQueue] = Params,
    #{type => ntf, method => notify, id => null, job_id => lowercase(JobId),
      block_version => BlockVersion, block_hash => lowercase(BlockHash),
      empty_queue => EmptyQueue};
%% Responses
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"result">> := Result,
                      <<"error">> := null}}) when Result =/= null ->
    ok = check_id(int, Id),
    %% Result is not checked here, the check is done in
    %% validate_rsp_params/2. We don't have info about what
    %% response params are expected here. There is no info
    %% on what kind of response this is.
    #{type => rsp, id => Id, result => Result};
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"result">> := null,
                      <<"error">> := Error}}) when Error =/= null ->
    ok = check_id(allow_null, Id),
    %% Error is not checked here, the check is done in
    %% validate_rsp_params/2. It could be done here though,
    %% but let's follow the behaviour of how it's done with
    %% the result above.
    #{type => rsp, id => Id, error => Error};
%% Msg validation errors
msg_to_map(#{msg := #{<<"id">> := _Id,
                      <<"method">> := _Method,
                      <<"params">> := Params}}) when is_list(Params) ->
    validation_exception({field, method});
msg_to_map(_Data) ->
    validation_exception(invalid_msg).

map_to_msg(#{map := #{type := req, method := configure, id := Id,
                      params := Params}} = Data) ->
    ok = check_configure_req(Id, Params),
    to_req_msg(<<"mining.configure">>, Id, Params, Data);
map_to_msg(#{map := #{type := req, method := subscribe, id := Id,
                      user_agent := UserAgent, session_id := SessionId,
                      host := Host, port := Port}} = Data) ->
    Params = [UserAgent, SessionId, Host, Port],
    ok = check_subscribe_req(Id, Params),
    to_req_msg(<<"mining.subscribe">>, Id, Params, Data);
map_to_msg(#{map := #{type := req, method := authorize, id := Id,
                      user := User, password := Password}} = Data) ->
    Params = [User, Password],
    ok = check_authorize_req(Id, Params),
    to_req_msg(<<"mining.authorize">>, Id, Params, Data);
map_to_msg(#{map := #{type := req, method := submit, id := Id,
                      user := User, job_id := JobId, miner_nonce := MinerNonce,
                      pow := Pow}} = Data) ->
    Params = [User, JobId, MinerNonce, Pow],
    ok = check_submit_req(Id, Params),
    to_req_msg(<<"mining.submit">>, Id, Params, Data);
map_to_msg(#{map := #{type := req, method := reconnect, id := Id,
                      host := Host, port := Port,
                      wait_time := WaitTime}} = Data) ->
    Params =
        case [Host, Port, WaitTime] of
            [null, null, 0] -> [];
            [_Host1, _Port1, _WaitTime1] = Params1 -> Params1
        end,
    ok = check_reconnect_req(Id, Params),
    to_req_msg(<<"client.reconnect">>, Id, Params, Data);
map_to_msg(#{map := #{type := ntf, method := set_target,
                      target := Target}} = Data) ->
    ok = check_set_target_ntf(null, [Target]),
    to_ntf_msg(<<"mining.set_target">>, [Target], Data);
map_to_msg(#{map := #{type := ntf, method := notify, job_id := JobId,
                      block_version := BlockVersion, block_hash := BlockHash,
                      empty_queue := EmptyQueue}} = Data) ->
    Params = [JobId, BlockVersion, BlockHash, EmptyQueue],
    ok = check_notify_ntf(null, Params),
    to_ntf_msg(<<"mining.notify">>, Params, Data);
map_to_msg(#{map := #{type := rsp, method := Method, id := Id,
                      result := Result}} = Data) ->
    ok = check_id(int, Id),
    ok = check_result(Method, Result),
    to_result_rsp_msg(Id, Result, Data);
map_to_msg(#{map := #{type := rsp, id := Id, reason := Rsn,
                      data := ErrorData}} = Data) ->
    ok = check_id(allow_null, Id),
    ok = check_error_data(ErrorData),
    ErrorParams = reason_to_error_params(Rsn, ErrorData),
    to_error_rsp_msg(Id, ErrorParams, Data);
map_to_msg(_Other) ->
    validation_exception(invalid_msg).

to_req_msg(Method, Id, Params, Data) ->
    Data#{msg => #{<<"jsonrpc">> => ?JSONRPC_VERSION, <<"method">> => Method,
                   <<"id">> => Id, <<"params">> => Params}}.

to_ntf_msg(Method, Params, Data) ->
    Data#{msg => #{<<"jsonrpc">> => ?JSONRPC_VERSION, <<"method">> => Method,
                   <<"id">> => null, <<"params">> => Params}}.

to_result_rsp_msg(Id, Result, Data) ->
    Data#{msg => #{<<"jsonrpc">> => ?JSONRPC_VERSION, <<"id">> => Id,
                   <<"result">> => Result, <<"error">> => null}}.

to_error_rsp_msg(Id, Error, Data) ->
    Data#{msg => #{<<"jsonrpc">> => ?JSONRPC_VERSION, <<"id">> => Id,
                   <<"result">> => null, <<"error">> => Error}}.

check_configure_req(Id, []) ->
    ok = check_id(int, Id);
check_configure_req(_Id, _Params) ->
    validation_exception({param, configure_params}).

check_subscribe_req(Id, [UserAgent, SessionId, Host, Port]) ->
    ok = check_id(int, Id),
    ok = check_user_agent(UserAgent),
    ok = check_session_id(SessionId),
    ok = check_host(Host),
    ok = check_port(int, Port);
check_subscribe_req(_Id, _Params) ->
    validation_exception({param, subscribe_params}).

check_authorize_req(Id, [User, Password]) ->
    ok = check_id(int, Id),
    ok = check_user(User),
    ok = check_password(Password);
check_authorize_req(_Id, _Params) ->
    validation_exception({param, authorize_params}).

check_submit_req(Id, [User, JobId, MinerNonce, Pow]) ->
    ok = check_id(int, Id),
    ok = check_user(User),
    ok = check_job_id(JobId),
    ok = check_miner_nonce(MinerNonce),
    ok = check_pow(Pow);
check_submit_req(_Id, _Params) ->
    validation_exception({param, submit_params}).

check_reconnect_req(Id, [Host, Port, WaitTime]) ->
    ok = check_id(int, Id),
    ok = check_host(Host),
    ok = check_port(allow_null, Port),
    ok = check_wait_time(WaitTime);
check_reconnect_req(Id, []) ->
    ok = check_id(int, Id);
check_reconnect_req(_Id, _Params) ->
    validation_exception({param, reconnect_params}).

check_set_target_ntf(Id, [Target]) ->
    ok = check_id(null, Id),
    ok = check_target(Target);
check_set_target_ntf(_Id, _Params) ->
    validation_exception({param, set_target_params}).

check_notify_ntf(Id, [JobId, BlockVersion, BlockHash, EmptyQueue]) ->
    ok = check_id(null, Id),
    ok = check_job_id(JobId),
    ok = check_block_version(BlockVersion),
    ok = check_block_hash(BlockHash),
    ok = check_empty_queue(EmptyQueue);
check_notify_ntf(_Id, _Params) ->
    validation_exception({param, notify_params}).

check_id(null, null) ->
    ok;
check_id(int, Id) when ?IS_ID(Id) ->
    ok;
check_id(allow_null, Id) when (Id =:= null) or ?IS_ID(Id) ->
    ok;
check_id(_Type, _Id) ->
    validation_exception({field, id}).

check_user_agent(UserAgent) when ?IS_USER_AGENT(UserAgent) ->
    ok = check_valid_string(UserAgent, user_agent),
    case binary:split(UserAgent, <<"/">>) of
        [Client, Version] when Client =/= <<>>, Version =/= <<>> -> ok;
        _Other -> validation_exception({param, user_agent})
    end;
check_user_agent(_UserAgent) ->
    validation_exception({param, user_agent}).

check_session_id(null) ->
    ok;
check_session_id(SessionId) when ?IS_SESSION_ID(SessionId) ->
    ok = check_hex(SessionId, session_id);
check_session_id(_SessionId) ->
    validation_exception({param, session_id}).

check_host(null) ->
    ok;
check_host(Host) when ?IS_HOST(Host) ->
    ok = check_valid_string(Host, host);
check_host(_Host) ->
    validation_exception({param, host}).

check_port(allow_null, null) ->
    ok;
check_port(allow_null, Port) ->
    check_port_(Port);
check_port(int, Port) ->
    check_port_(Port).

check_port_(Port) when ?IS_PORT(Port) ->
    ok;
check_port_(_Port) ->
    validation_exception({param, port}).

check_user(User) when ?IS_USER(User) ->
    ok = check_valid_string(User, user);
check_user(_User) ->
    validation_exception({param, user}).

check_password(Password) when ?IS_PASSWORD(Password) ->
    ok = check_hex(Password, password);
check_password(_Password) ->
    validation_exception({param, password}).

check_target(Target) when ?IS_TARGET(Target) ->
    ok = check_hex(Target, target);
check_target(_Target) ->
    validation_exception({param, target}).

check_job_id(JobId) when ?IS_JOB_ID(JobId) ->
    ok = check_hex(JobId, job_id);
check_job_id(_JobId) ->
    validation_exception({param, job_id}).

check_block_version(BlockVersion) when ?IS_BLOCK_VERSION(BlockVersion) ->
    ok;
check_block_version(_BlockVersion) ->
    validation_exception({param, block_version}).

check_block_hash(BlockHash) when ?IS_BLOCK_HASH(BlockHash) ->
    ok = check_hex(BlockHash, block_hash);
check_block_hash(_BlockHash) ->
    validation_exception({param, block_hash}).

check_empty_queue(EmptyQueue) when ?IS_EMPTY_QUEUE(EmptyQueue) ->
    ok;
check_empty_queue(_EmptyQueue) ->
    validation_exception({param, empty_queue}).

check_wait_time(WaitTime) when ?IS_WAIT_TIME(WaitTime) ->
    ok;
check_wait_time(_WaitTime) ->
    validation_exception({param, wait_time}).

check_miner_nonce(MinerNonce) when ?IS_MINER_NONCE(MinerNonce) ->
    ok = check_hex(MinerNonce, miner_nonce);
check_miner_nonce(_MinerNonce) ->
    validation_exception({param, miner_nonce}).

check_extra_nonce(ExtraNonce) when ?IS_EXTRA_NONCE(ExtraNonce) ->
    ok = check_hex(ExtraNonce, extra_nonce);
check_extra_nonce(_ExtraNonce) ->
    validation_exception({param, extra_nonce}).

check_pow(Pow) when ?IS_POW(Pow) ->
    case lists:all(fun(N) when ?IS_POW_NUMBER(N) -> true;
                      (_N) -> false
                   end, Pow) of
        true -> ok;
        false -> validation_exception({param, pow})
    end;
check_pow(_Pow) ->
    validation_exception({param, pow}).

%% Configure response: [] (no config params supported)
%% Subscribe response: [SessionId, ExtraNonce]
%% Authorize response: true | false
%% Submit response:    true | false
check_result(configure, []) ->
    ok;
check_result(subscribe, [SessionId, ExtraNonce]) ->
    ok = check_session_id(SessionId),
    ok = check_extra_nonce(ExtraNonce);
check_result(authorize, Result) when is_boolean(Result) ->
    ok;
check_result(submit, Result) when is_boolean(Result) ->
    ok;
check_result(configure, _Result) ->
    validation_exception({param, configure_params});
check_result(subscribe, _Result) ->
    validation_exception({param, subscribe_params});
check_result(authorize, _Result) ->
    validation_exception({param, authorize_params});
check_result(submit, _Result) ->
    validation_exception({param, submit_params}).

check_error([Code, Msg, Data]) ->
    ok = check_error_code(Code),
    ok = check_error_msg(Msg),
    ok = check_error_data(Data);
check_error(_Error) ->
    validation_exception({param, error_params}).

check_error_code(Code) ->
    _ = error_code_to_reason(Code),
    ok.

check_error_msg(Msg) when ?IS_ERROR_MSG(Msg) ->
    ok;
check_error_msg(_Msg) ->
    validation_exception({param, error_msg}).

check_error_data(null) ->
    ok;
check_error_data(Data) when ?IS_ERROR_DATA(Data) ->
    ok;
check_error_data(_Data) ->
    validation_exception({param, error_data}).

check_hex(Bin, Param) ->
    case is_hex(Bin) of
        true -> ok;
        false -> validation_exception({param, Param})
    end.

check_valid_string(Bin, Param) ->
    case is_valid_string(Bin) of
        true -> ok;
        false -> validation_exception({param, Param})
    end.

validation_exception(Rsn) ->
    throw({validation_error, Rsn}).

reason_to_error_params(parse_error, Data) ->
    [-32700, <<"Parse error">>, Data];
reason_to_error_params(invalid_msg, Data) ->
    [-32000, <<"Invalid request">>, Data];
reason_to_error_params(invalid_method, Data) ->
    [-32601, <<"Method not found">>, Data];
reason_to_error_params(invalid_param, Data)  ->
    [-32602, <<"Invalid params">>, Data];
reason_to_error_params(internal_error, Data) ->
    [-32603, <<"Internal error">>, Data];
reason_to_error_params(unknown_error, Data) ->
    [20, <<"Other/Unknown">>, Data];
reason_to_error_params(job_not_found, Data) ->
    [21, <<"Job not found">>, Data];
reason_to_error_params(duplicate_share, Data) ->
    [22, <<"Duplicate share">>, Data];
reason_to_error_params(low_difficulty_share, Data) ->
    [23, <<"Low difficulty share">>, Data];
reason_to_error_params(unauthorized_worker, Data) ->
    [24, <<"Unauthorized worker">>, Data];
reason_to_error_params(not_subscribed, Data) ->
    [25, <<"Not subscribed">>, Data];
reason_to_error_params(_Rsn, _Data) ->
    validation_exception({param, error_reason}).

error_code_to_reason(-32700) -> parse_error;
error_code_to_reason(-32000) -> invalid_msg;
error_code_to_reason(-32601) -> invalid_method;
error_code_to_reason(-32602) -> invalid_param;
error_code_to_reason(-32603) -> internal_error;
error_code_to_reason(20)     -> unknown_error;
error_code_to_reason(21)     -> job_not_found;
error_code_to_reason(22)     -> duplicate_share;
error_code_to_reason(23)     -> low_difficulty_share;
error_code_to_reason(24)     -> unauthorized_worker;
error_code_to_reason(25)     -> not_subscribed;
error_code_to_reason(_Code)  -> validation_exception({param, error_code}).

is_hex(Bin) when is_binary(Bin) ->
    lists:all(fun(Byte) when Byte >= $0, Byte =< $9 -> true;
                 (Byte) when Byte >= $a, Byte =< $f -> true;
                 (Byte) when Byte >= $A, Byte =< $F -> true;
                 (_Byte) -> false end, binary_to_list(Bin)).

is_valid_string(Bin) when is_binary(Bin) ->
    lists:all(fun(Byte) when Byte =:= $\s -> false;
                 (Byte) when Byte =:= $\n -> false;
                 (Byte) when Byte =:= $\t -> false;
                 (Byte) when Byte =:= $\v -> false;
                 (Byte) when Byte =:= $\f -> false;
                 (Byte) when Byte =:= $\r -> false;
                 (_Byte) -> true end, binary_to_list(Bin)).

lowercase(Bin) when is_binary(Bin) ->
    string:lowercase(Bin);
lowercase(Other) ->
    Other.

