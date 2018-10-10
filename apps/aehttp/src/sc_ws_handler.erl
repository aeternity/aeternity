-module(sc_ws_handler).

%% API
-export([push/3,
         push_async/3]).

%% WS API
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-record(handler, {fsm_pid            :: pid() | undefined,
                  fsm_mref           :: reference() | undefined,
                  channel_id         :: aesc_channels:id() | undefined,
                  enc_channel_id     :: aec_base58c:encoded() | undefined,
                  job_id             :: term(),
                  protocol           :: legacy | jsonrpc | undefined,
                  orig_request       :: map() | undefined,
                  role               :: initiator | responder | undefined,
                  host               :: binary() | undefined,
                  port               :: non_neg_integer() | undefined}).

-opaque handler() :: #handler{}.
-export_type([handler/0]).
-define(METHOD_SIGNED(Method), Method =:= <<"channels.initiator_sign">>;
                               Method =:= <<"channels.deposit_tx">>;
                               Method =:= <<"channels.deposit_ack">>;
                               Method =:= <<"channels.withdraw_tx">>;
                               Method =:= <<"channels.withdraw_ack">>;
                               Method =:= <<"channels.responder_sign">>;
                               Method =:= <<"channels.shutdown_sign">>;
                               Method =:= <<"channels.shutdown_sign_ack">>;
                               Method =:= <<"channels.update">>;
                               Method =:= <<"channels.update_ack">>).
-define(METHOD_TAG(Method), case Method of
                                <<"channels.initiator_sign">>    -> create_tx;
                                <<"channels.deposit_tx">>        -> deposit_tx;
                                <<"channels.deposit_ack">>       -> deposit_created;
                                <<"channels.withdraw_tx">>       -> withdraw_tx;
                                <<"channels.withdraw_ack">>      -> withdraw_created;
                                <<"channels.responder_sign">>    -> funding_created;
                                <<"channels.update">>            -> update;
                                <<"channels.update_ack">>        -> update_ack;
                                <<"channels.shutdown_sign">>     -> shutdown;
                                <<"channels.shutdown_sign_ack">> -> shutdown_ack;
                                <<"channels.leave">>             -> leave
                            end).

init(Req, _Opts) ->
    lager:debug("init(~p, ~p)", [Req, _Opts]),
    {cowboy_websocket, Req,
     maps:merge(#{protocol => <<"legacy">>},
                maps:from_list(cowboy_req:parse_qs(Req)))}.

-spec websocket_init(map()) -> {ok, handler()} | {stop, undefined}.
websocket_init(Params) ->
    case {prepare_handler(Params), read_channel_options(Params)} of
        {{error, Err}, _} ->
            lager:info("Channel WS failed to start because of ~p; params ~p",
                       [Err, Params]),
            {stop, undefined};
        {_, {error, Err}} ->
            lager:info("Channel WS failed to start because of ~p; params ~p",
                       [Err, Params]),
            {stop, undefined};
        {Handler, ChannelOpts} ->
            lager:debug("Starting Channel WS with params ~p", [Params]),
            {ok, FsmPid} = start_link_fsm(Handler, ChannelOpts),
            MRef = erlang:monitor(process, FsmPid),
            {ok, Handler#handler{fsm_pid = FsmPid, fsm_mref = MRef}}
    end.

-spec websocket_handle(term(), handler()) -> {ok, handler()}.
websocket_handle({text, MsgBin}, #handler{protocol = P} = H) ->
    try unpack_request(P, jsx:decode(MsgBin, [return_maps]), H) of
        {Msg, H1} ->
            HNext = H1#handler{orig_request = undefined},
            try process_incoming(Msg, H1) of
                no_reply      -> {ok, HNext};
                {reply, Resp} -> {reply, {text, jsx:encode(Resp)}, HNext}
            catch
                error:E1 ->
                    lager:debug("CAUGHT E1=~p / ~p", [E1, erlang:get_stacktrace()]),
                    throw({die_anyway, E1})
            end
    catch
        error:E ->
            lager:debug("Caught E=~p / ~p", [E, erlang:get_stacktrace()]),
            {ok, H};
        throw:{die_anyway, E1_} ->
            erlang:error(E1_)
    end;
websocket_handle(_Data, H) ->
    {ok, H}.

websocket_info(Msg, #handler{protocol = P} = H) ->
    try unpack_info(P, Msg, H) of
        {Msg1, H1} ->
            try websocket_info_(Msg1, H1)
            catch
                error:E1 ->
                    lager:debug("CAUGHT E1=~p / ~p",
                                [E1, erlang:get_stacktrace()]),
                    {stop, H}
            end
    catch
        error:E ->
            lager:debug("CAUGHT E=~p / ~p", [E, erlang:get_stacktrace()]);
        throw:{die_anyway, E1_} ->
            erlang:error(E1_)
    end.

websocket_info_({push, ChannelId, Data, Options}, H) ->
    case ChannelId =:= channel_id(H) of
        false ->
            process_response({error, wrong_channel_id}, Options),
            {ok, H};
        true ->
            process_response(ok, Options),
            Msg = jsx:encode(Data),
            {reply, {text, Msg}, H}
    end;
websocket_info_({aesc_fsm, FsmPid, Msg}, #handler{fsm_pid=FsmPid}=H) ->
    H1 = set_channel_id(Msg, H),
    case process_fsm(Msg, H) of
        %no_reply -> {ok, H1};
        %{error, _} -> {ok, H1}
        {reply, Resp} -> {reply, {text, jsx:encode(Resp)}, H1}
    end;
websocket_info_({'DOWN', MRef, _, _, _}, #handler{fsm_mref = MRef} = H) ->
    {stop, H#handler{fsm_pid = undefined,
                     fsm_mref = undefined}};
websocket_info_(_Info, State) ->
    {ok, State}.

set_channel_id(#{channel_id := Id},
               #handler{channel_id = undefined} = H) when Id =/= undefined ->
    H#handler{channel_id = Id,
              enc_channel_id = aec_base58c:encode(channel, Id)};
set_channel_id(#{channel_id := A}, #handler{channel_id = B})
  when A =/= undefined, A =/= B ->
    erlang:error({channel_id_mismatch, [A, B]});
set_channel_id(_Msg, H) ->
    H.

fsm_reply(Msg, #handler{enc_channel_id = Id}) ->
    case Id of
        undefined -> {reply, Msg};
        _         -> {reply, maps:merge(#{channel_id => Id}, Msg)}
    end.

terminate(_Reason, _PartialReq, #{} = _H) ->
    % not initialized yet
    ok;
terminate(Reason, _PartialReq, State) ->
    lager:debug("WebSocket dying because of ~p", [Reason]),
    case fsm_pid(State) of
        undefined -> pass;
        FsmPid ->
            true = unlink(FsmPid),
            ok = aesc_fsm:client_died(FsmPid)
    end,
    jobs:done(job_id(State)),
    ok.

-spec job_id(handler()) -> term().
job_id(#handler{job_id = JobId}) ->
    JobId.

-spec channel_id(handler()) -> aesc_channels:id() | undefined.
channel_id(#handler{channel_id = ChannelId}) ->
    ChannelId.

-spec fsm_pid(handler()) -> pid() | undefined.
fsm_pid(#handler{fsm_pid = Pid}) ->
    Pid.

-spec push(pid(), aesc_channels:id(), map()) -> ok | {error, timeout}
                                                   | {error, noproc}
                                                   | {error, wrong_channel_id}.
push(WsPid, ChannelId, Data) ->
    WsPid ! {push, ChannelId, Data, [{sender, self()}]},
    receive
        {ws_proc_response, Resp} -> Resp
    after 5000 ->
        case is_ws_alive(WsPid) of
            false -> {error, noproc};
            true -> {error, timeout}
        end
    end.

-spec push_async(pid(), aesc_channels:id(), map()) -> ok | {error, noproc}.
push_async(WsPid, ChannelId, Data) ->
    case is_ws_alive(WsPid) of
        false -> {error, noproc};
        true ->
            WsPid ! {push, ChannelId, Data, []},
            ok
    end.

-spec process_response(term(), list()) -> ok.
process_response(Response, Options) ->
    lists:foreach(
        fun({Key, Fun}) ->
            case proplists:get_value(Key, Options) of
                undefined -> pass;
                Value -> Fun(Value)
            end
        end,
        [{sender, fun(SenderPid) -> SenderPid ! {ws_proc_response, Response} end}]),
    ok.

-spec is_ws_alive(pid()) -> boolean().
is_ws_alive(Pid) ->
    case erlang:process_info(Pid) of
        undefined -> false;
        _ -> true
    end.

-spec start_link_fsm(handler(), map()) -> {ok, pid()}.
start_link_fsm(#handler{role = initiator, host=Host, port=Port}, Opts) ->
    {ok, _Pid} = aesc_fsm:initiate(Host, Port, Opts);
start_link_fsm(#handler{role = responder, port=Port}, Opts) ->
    {ok, _Pid} = aesc_fsm:respond(Port, Opts).

set_field(H, host, Val) -> H#handler{host = Val};
set_field(H, role, Val) -> H#handler{role = Val};
set_field(H, port, Val) -> H#handler{port = Val}.

-spec read_param(binary(), atom(), map()) -> fun((map()) -> {ok, term()} |
                                                            not_set |
                                                            {error, {atom(), atom()}}).
read_param(ParamName, RecordField, Options) ->
    fun(Params) ->
        Mandatory = maps:get(mandatory, Options, true),
        case maps:get(ParamName, Params, undefined) of
            undefined when Mandatory ->
                {error, {RecordField, missing}};
            undefined when not Mandatory ->
                not_set;
            Val0 ->
                Type = maps:get(type, Options, binary),
                case parse_by_type(Type, Val0, RecordField) of
                    {error, _} = Err -> Err;
                    {ok, Val} ->
                        case maps:get(enum, Options, undefined) of
                            undefined ->  {ok, Val};
                            AllowedVals when is_list(AllowedVals) ->
                                case lists:member(Val, AllowedVals) of
                                    true -> {ok, Val};
                                    false ->
                                        {error, {RecordField, invalid}}
                                end
                        end
                end
        end
    end.

parse_by_type(binary, V, _) when is_binary(V) ->
    {ok, V};
parse_by_type(boolean, V, _) when is_binary(V) ->
    case V of
        <<"true">>  -> {ok, true};
        <<"false">> -> {ok, false};
        _           -> {error, not_bool}
    end;
parse_by_type(string, V, _) when is_binary(V) ->
    {ok, binary_to_list(V)};
parse_by_type(atom, V, _) when is_binary(V) ->
    {ok, binary_to_existing_atom(V, utf8)};
parse_by_type(integer, V, _) when is_binary(V) ->
    {ok, list_to_integer(binary_to_list(V))};
parse_by_type({hash, Type}, V, RecordField) when is_binary(V) ->
    case aec_base58c:safe_decode(Type, V) of
        {error, _} ->
            {error, {RecordField, broken_encoding}};
        {ok, _} = OK -> OK
    end;
parse_by_type(serialized_tx, V, RecordField) when is_binary(V) ->
    case aec_base58c:safe_decode(transaction, V) of
        {ok, TxBin} ->
            try {ok, aetx_sign:deserialize_from_binary(TxBin)}
            catch
                error:_ ->
                    {error, {RecordField, invalid_tx_serialization}}
            end;
        {error, _} ->
            {error, {RecordField, broken_encoding}}
    end.

-spec process_incoming(map(), handler()) -> no_reply | {reply, map()} | {error, atom()}.
process_incoming(#{<<"method">> := <<"channels.update.new">>,
                   <<"params">> := #{<<"from">>    := FromB,
                                     <<"to">>      := ToB,
                                     <<"amount">>  := Amount}}, H) ->
    case {aec_base58c:safe_decode(account_pubkey, FromB),
          aec_base58c:safe_decode(account_pubkey, ToB)} of
        {{ok, From}, {ok, To}} ->
            case aesc_fsm:upd_transfer(fsm_pid(H), From, To, Amount) of
                ok -> reply(ok, H, no_reply);
                {error, Reason} ->
                    error_response(Reason, H)
            end;
        _ -> error_response(broken_encoding, H)
    end;
process_incoming(#{<<"method">> := <<"channels.update.new_contract">>,
                   <<"params">> := #{<<"vm_version">> := VmVersion,
                                     <<"deposit">>    := Deposit,
                                     <<"code">>       := CodeE,
                                     <<"call_data">>  := CallDataE}}, H) ->
    case {bytearray_decode(CodeE), bytearray_decode(CallDataE)} of
        {{ok, Code}, {ok, CallData}} ->
            case aesc_fsm:upd_create_contract(fsm_pid(H),
                                              #{vm_version => VmVersion,
                                                deposit    => Deposit,
                                                code       => Code,
                                                call_data  => CallData}) of
                ok -> reply(ok, H, no_reply);
                {error, Reason} ->
                    error_response(Reason, H)
            end;
        _ -> error_response(broken_code, H)
    end;
process_incoming(#{<<"method">> := <<"channels.update.call_contract">>,
                   <<"params">> := #{<<"contract">>   := ContractE,
                                     <<"vm_version">> := VmVersion,
                                     <<"amount">>     := Amount,
                                     <<"call_data">>  := CallDataE}}, H) ->
    case {aec_base58c:safe_decode(contract_pubkey, ContractE),
          bytearray_decode(CallDataE)} of
        {{ok, Contract}, {ok, CallData}} ->
            case aesc_fsm:upd_call_contract(fsm_pid(H),
                                            #{contract   => Contract,
                                              vm_version => VmVersion,
                                              amount     => Amount,
                                              call_data  => CallData}) of
                ok -> reply(ok, H, no_reply);
                {error, Reason} ->
                    error_response(Reason, H)
            end;
        _ -> error_response(broken_code, H)
    end;
process_incoming(#{<<"method">> := <<"channels.get.contract_call">>,
                   <<"params">> := #{<<"contract">>   := ContractE,
                                     <<"caller">>     := CallerE,
                                     <<"round">>      := Round}}, H) ->
    case {aec_base58c:safe_decode(contract_pubkey, ContractE),
          aec_base58c:safe_decode(account_pubkey, CallerE)} of
        {{ok, Contract}, {ok, Caller}} ->
            case aesc_fsm:get_contract_call(fsm_pid(H),
                                            Contract, Caller, Round) of
                {ok, Call} ->
                    reply(aect_call:serialize_for_client(Call), H);
                {error, Reason} ->
                    error_response(Reason, H)
            end;
        _ -> error_response(broken_code, H)
    end;
process_incoming(#{<<"method">> := <<"channels.clean_contract_calls">>}, H) ->
    ok = aesc_fsm:prune_local_calls(fsm_pid(H)),
    reply(ok_response(calls_pruned), H);
process_incoming(#{<<"method">> := <<"channels.get.balances">>,
                   <<"params">> := #{<<"accounts">> := Accounts}}, H) ->
    case safe_decode_account_keys(Accounts) of
        {ok, AccountKeys} ->
            case aesc_fsm:get_balances(
                   fsm_pid(H), [K || {_,K} <- AccountKeys]) of
                {ok, Balances} ->
                    Resp = [#{<<"account">> => AcctExt,
                              <<"balance">> => Bal}
                            || {AcctExt, AcctInt} <- AccountKeys,
                               {Acct, Bal} <- Balances,
                               AcctInt =:= Acct],
                    reply(Resp, H);
                {error, Reason} ->
                    error_response(Reason, H)
            end;
        {error, _} ->
            error_response(invalid_arguments, H)
    end;
process_incoming(#{<<"method">> := <<"channels.get.poi">>,
                   <<"params">> := Filter}, H) ->
    AccountsE   = maps:get(<<"accounts">>, Filter, []),
    ContractsE  = maps:get(<<"contracts">>, Filter, []),
    Parse =
        fun(T, Keys) ->
            try {ok, lists:foldr(
                      fun(K, Acc) ->
                              {ok, Res} = aec_base58c:safe_decode(T, K),
                              [Res | Acc]
                      end, [], Keys)}
            catch
                error:_ ->
                    {error, invalid_pubkey}
            end
        end,
    BrokenEncodingReply =
        fun(What) ->
                error_response(<<"broken_encoding: ", What/binary>>, H)
        end,
    case {Parse(account_pubkey, AccountsE), Parse(contract_pubkey, ContractsE)} of
        {{ok, Accounts0}, {ok, Contracts0}} ->
            Accounts = [{account, A} || A <- Accounts0 ++ Contracts0],
            Contracts = [{contract, C} || C <- Contracts0],
            case aesc_fsm:get_poi(fsm_pid(H), Accounts ++ Contracts) of
                {ok, PoI} ->
                    Resp = #{
                      <<"poi">> => aec_base58c:encode(
                                     poi, aec_trees:serialize_poi(PoI))},
                    reply(Resp, H);
                {error, Reason} ->
                    error_response(Reason, H)
            end;
      {{error, _},  {ok, _}}    -> BrokenEncodingReply(<<"accounts">>);
      {{ok, _},     {error, _}} -> BrokenEncodingReply(<<"contracts">>);
      {{error, _},  {error, _}} -> BrokenEncodingReply(<<"accounts, contracts">>)
    end;
process_incoming(#{<<"method">> := <<"channels.message">>,
                   <<"params">> := #{<<"to">>    := ToB,
                                     <<"info">>  := Msg}}, H) ->
    case aec_base58c:safe_decode(account_pubkey, ToB) of
        {ok, To} ->
            case aesc_fsm:inband_msg(fsm_pid(H), To, Msg) of
                ok -> reply(ok, H, no_reply);
                {error, Reason} ->
                    error_response(Reason, H)
            end;
        _ -> error_response(broken_encoding, H)
    end;
process_incoming(#{<<"method">> := <<"channels.deposit">>,
                   <<"params">> := #{<<"amount">>  := Amount}}, H) ->
    case aesc_fsm:upd_deposit(fsm_pid(H), #{amount => Amount}) of
        ok -> reply(ok, H, no_reply);
        {error, Reason} ->
            error_response(Reason, H)
    end;
process_incoming(#{<<"method">> := <<"channels.withdraw">>,
                   <<"params">> := #{<<"amount">>  := Amount}}, H) ->
    case aesc_fsm:upd_withdraw(fsm_pid(H), #{amount => Amount}) of
        ok -> reply(ok, H, no_reply);
        {error, Reason} ->
            error_response(Reason, H)
    end;
process_incoming(#{<<"method">> := Method,
                   <<"params">> := #{<<"tx">> := EncodedTx}}, H)
    when ?METHOD_SIGNED(Method) ->
    Tag = ?METHOD_TAG(Method),
    case aec_base58c:safe_decode(transaction, EncodedTx) of
        {error, _} ->
            lager:warning("Channel WS: broken ~p tx ~p", [Method, EncodedTx]),
            error_response(invalid_tx, H);
        {ok, TxBin} ->
            SignedTx = aetx_sign:deserialize_from_binary(TxBin),%TODO: check tx
            aesc_fsm:signing_response(fsm_pid(H), Tag, SignedTx),
            reply(ok, H, no_reply)
    end;
process_incoming(#{<<"method">> := <<"channels.leave">>}, H) ->
    lager:debug("Channel WS: leave channel message received"),
    aesc_fsm:leave(fsm_pid(H)),
    reply(ok, H, no_reply);
process_incoming(#{<<"method">> := <<"channels.shutdown">>}, H) ->
    lager:warning("Channel WS: closing channel message received"),
    aesc_fsm:shutdown(fsm_pid(H)),
    reply(ok, H, no_reply);
process_incoming(#{<<"method">> := _} = Unhandled, H) ->
    lager:warning("Channel WS: unhandled action received ~p", [Unhandled]),
    error_response(unhandled, H);
process_incoming(Msg, H) ->
    lager:warning("Channel WS: missing action received ~p", [Msg]),
    error_response(unhandled, H).

-spec process_fsm(term(), handler()) -> no_reply | {reply, map()} | {error, atom()}.
process_fsm(#{type := sign,
              tag  := Tag,
              info := Tx}, H) when Tag =:= create_tx
                            orelse Tag =:= deposit_tx
                            orelse Tag =:= deposit_created
                            orelse Tag =:= withdraw_tx
                            orelse Tag =:= withdraw_created
                            orelse Tag =:= shutdown
                            orelse Tag =:= shutdown_ack
                            orelse Tag =:= funding_created
                            orelse Tag =:= update
                            orelse Tag =:= update_ack ->
    EncTx = aec_base58c:encode(transaction, aetx:serialize_to_binary(Tx)),
    Tag1 =
        case Tag of
            create_tx -> <<"initiator_sign">>;
            funding_created -> <<"responder_sign">>;
            shutdown -> <<"shutdown_sign">>;
            shutdown_ack -> <<"shutdown_sign_ack">>;
            deposit_created -> <<"deposit_ack">>;
            withdraw_created -> <<"withdraw_ack">>;
            T -> T
        end,
    reply(#{action  => <<"sign">>,
            tag => Tag1,
            payload => #{tx => EncTx}}, H);
process_fsm(#{type := report,
              tag  := Tag,
              info := Event}, H) when Tag =:= info
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
            {on_chain_tx, Tx} ->
                EncodedTx = aec_base58c:encode(transaction,
                                               aetx_sign:serialize_to_binary(Tx)),
                #{tx => EncodedTx};
            {_, NewState} when Tag == update; Tag == leave ->
                Bin = aec_base58c:encode(transaction,
                                         aetx_sign:serialize_to_binary(NewState)),
                #{state => Bin};
            {conflict, #{channel_id := ChId,
                         round      := Round}} ->
                         #{channel_id => aec_base58c:encode(channel, ChId),
                           round => Round};
            {message, #{channel_id  := ChId,
                        from        := From,
                        to          := To,
                        info        := Info}} ->
                #{message => #{channel_id => aec_base58c:encode(channel, ChId),
                               from => aec_base58c:encode(account_pubkey, From),
                               to => aec_base58c:encode(account_pubkey, To),
                               info => Info}};
            {error, Msg} -> #{message => Msg};
            {debug, Msg} -> #{message => Msg}
        end,
    Action = atom_to_binary(Tag, utf8),
    fsm_reply(#{action => Action,
                payload => Payload}, H);
process_fsm(#{type := Type, tag := Tag, info := Event}, _H) ->
    error({unparsed_fsm_event, Type, Tag, Event}).

prepare_handler(Params) ->
    lager:debug("prepare_handler() Params = ~p", [Params]),
    Read =
        fun(Key, RecordField, Opts) ->
            fun(H) ->
                case (read_param(Key, RecordField, Opts))(Params) of
                    not_set -> H;
                    {ok, Val} -> set_field(H, RecordField, Val);
                    {error, _} = Err -> Err
                end
            end
        end,
    Validators =
        [fun(H) ->
            case jobs:ask(ws_handlers) of
                {ok, JobId} ->
                    H#handler{job_id = JobId};
                {error, _} ->
                    {error, too_many_ws_sockets}
            end
        end,
        Read(<<"role">>, role, #{type => atom,
                                 enum => [responder, initiator]}),
        fun(#handler{role = Role} = H) ->
            case Role of
                initiator -> % require having a host only for initiator
                    F = Read(<<"host">>, host, #{type => string}),
                    F(H);
                responder -> H
            end
        end,
        Read(<<"port">>, port, #{type => integer})
        ],
    lists:foldl(
        fun(_, {error, _} = Err) -> Err;
            (Fun, Accum) -> Fun(Accum)
        end,
        #handler{protocol = protocol(Params)}, Validators).

protocol(#{protocol := P}) ->
    case P of
        <<"legacy">>   -> legacy;
        <<"json-rpc">> -> jsonrpc;
        _ when P==legacy;
               P==jsonrpc -> P;
        _Other ->
            erlang:error(invalid_protocol)
    end.

read_channel_options(Params) ->
    Read =
        fun(KeyBin, Key, Opts) ->
            fun(M) ->
                case (read_param(KeyBin, Key, Opts))(Params) of
                    not_set -> M;
                    {ok, Val} -> maps:put(Key, Val, M);
                    {error, _} = Err -> Err
                end
            end
        end,
    Put =
        fun(K, V) ->
            fun(M) -> maps:put(K, V, M) end
        end,
    ReadMap =
        fun(MapName, Prefix, Opts) ->
            fun(Name) ->
                NameBin = atom_to_binary(Name, utf8),
                Key = <<Prefix/binary, "_", NameBin/binary>>,
                fun(M) ->
                    OldVal = maps:get(MapName, M, #{}),
                    case (read_param(Key, Name, Opts))(Params) of
                        not_set -> M;
                        {ok, Val} -> maps:put(MapName, maps:put(Name, Val, OldVal), M);
                        {error, _} = Err -> Err
                    end
                end
            end
        end,
    ReadTimeout = ReadMap(timeouts, <<"timeout">>, #{type => integer,
                                                     mandatory => false}),
    ReadReport = ReadMap(report, <<"report">>, #{type => boolean,
                                                     mandatory => false}),
    lists:foldl(
        fun(_, {error, _} = Err) -> Err;
            (Fun, Accum) -> Fun(Accum)
        end,
        #{},
        [Read(<<"initiator_id">>, initiator, #{type => {hash, account_pubkey}}),
         Read(<<"responder_id">>, responder, #{type => {hash, account_pubkey}}),
         Read(<<"existing_channel_id">>, existing_channel_id,
              #{type => {hash, channel}, mandatory => false}),
         Read(<<"offchain_tx">>, offchain_tx,
              #{type => serialized_tx, mandatory => false}),
         Read(<<"lock_period">>, lock_period, #{type => integer}),
         Read(<<"push_amount">>, push_amount, #{type => integer}),
         Read(<<"initiator_amount">>, initiator_amount, #{type => integer}),
         Read(<<"responder_amount">>, responder_amount, #{type => integer}),
         Read(<<"channel_reserve">>, channel_reserve, #{type => integer}),
         Read(<<"ttl">>, ttl, #{type => integer, mandatory => false}),
         Put(noise, [{noise, <<"Noise_NN_25519_ChaChaPoly_BLAKE2b">>}])
        ] ++ lists:map(ReadTimeout, aesc_fsm:timeouts() ++ [awaiting_open,
                                                            initialized])
          ++ lists:map(ReadReport, aesc_fsm:report_tags())
     ).

error_response(Reason, #handler{protocol = legacy, orig_request = Req}) ->
    {reply, #{ <<"action">>  => <<"error">>
             , <<"payload">> => #{ <<"request">> => Req
                                 , <<"reason">> => Reason} }
    };
error_response(Reason, #handler{protocol = jsonrpc, orig_request = Req}) ->
    case Req of
        #{<<"id">> := Id} ->
            {reply, #{ <<"jsonrpc">> => <<"2.0">>
                     , <<"id">>      => Id
                     , <<"error">>   => Reason }
            };
        _ ->
            noreply
    end.

ok_response(Action) ->
    #{action => Action}.

bytearray_decode(Bytearray) ->
    aec_base58c:safe_decode(contract_bytearray, Bytearray).

safe_decode_account_keys(Keys) ->
    try {ok, lists:foldr(
               fun(K, Acc) ->
                       {ok, Res} = aec_base58c:safe_decode(account_pubkey, K),
                       [{K, Res}|Acc]
               end, [], Keys)}
    catch
        error:_ ->
            {error, invalid_pubkey}
    end.

unpack_request(jsonrpc, #{ <<"jsonrpc">> := <<"2.0">>
                         , <<"method">>  := _Method } = Req, H) ->
    { Req, H#handler{orig_request = Req} };
unpack_request(legacy, #{ <<"action">>  := Action
                        , <<"tag">>     := Tag
                        , <<"payload">> := Payload } = Req, H) ->
    Method = legacy_to_method_in(Action, Tag),
    { #{ <<"method">> => Method
       , <<"params">> => Payload }
    , H#handler{ orig_request = Req#{<<"method">> => Method} } };
unpack_request(legacy, #{ <<"action">>  := Action
                        , <<"payload">> := Payload } = Req, H) ->
    Method = legacy_to_method_in(Action),
    { #{ <<"method">> => Method
       , <<"params">> => Payload }
    , H#handler{ orig_request = Req#{<<"method">> => Method} } };
unpack_request(legacy, #{ <<"action">> := Action } = Req, H) ->
    Method = legacy_to_method_in(Action),
    { #{ <<"method">> => Method }
    , H#handler{ orig_request = Req#{<<"method">> => Method} } }.

unpack_info(Protocol, Msg, H) ->
    Req = info_to_req(Protocol, Msg),
    { Msg, H#handler{orig_request = Req} }.

info_to_req(_, {aesc_fsm, _, #{type := Type, tag := Tag} = Req})
  when Type == report; Type == info; Type == sign ->
    Method = iolist_to_binary(["channels.", atom_to_list(Type), ".", atom_to_list(Tag)]),
    #{ <<"method">> => Method
     , <<"params">> => Req };
info_to_req(_, _) ->
    undefined.

reply(Payload, #handler{protocol = legacy, orig_request = undefined}) ->
    {reply, Payload};
reply(Payload, #handler{protocol = legacy, orig_request = #{<<"method">> := Method}}) ->
    {reply, legacy_notify(Method, Payload)};
reply(Reply, #handler{protocol = jsonrpc} = H) ->
    json_rpc_reply(Reply, H).

legacy_notify(Method, Reply) ->
    case binary:split(Method, [<<".">>], [global]) of
        [<<"channels">>, Action, Tag] ->
            Action1 = opt_maps_get(action, Reply, Action),
            Tag1 = case find_tag(Reply) of
                       {ok, T} -> T;
                       error   -> Tag
                   end,
            add_payload(Reply, #{ <<"action">>  => Action1
                                , <<"tag">>     => Tag1 });
        [<<"channels">>, Action] ->
            Action1 = opt_maps_get(action, Reply, Action),
            case find_tag(Reply) of
                {ok, Tag} ->
                    add_payload(Reply, #{ <<"action">>  => Action1
                                        , <<"tag">>     => Tag});
                error ->
                    add_payload(Reply, #{ <<"action">>  => Action1 })
            end
    end.

opt_maps_get(Key, Map, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
opt_maps_get(_ , _, Default) ->
    Default.

find_tag(#{tag := Tag}) -> {ok, Tag};
find_tag(_)             -> error.

add_payload(#{payload := Payload}, Msg) -> Msg#{<<"payload">> => Payload};
add_payload(#{action := _}       , Msg) -> Msg;
add_payload(Reply                , Msg) -> Msg#{<<"payload">> => Reply}.

json_rpc_reply(Reply, #handler{orig_request = Req}) ->
    case Req of
        #{id := Id} ->
            {reply, #{ <<"jsonrpc">> => <<"2.0">>
                     , <<"id">>      => Id
                     , <<"result">>  => result(Reply) }
            };
        _ ->
            {reply, #{ <<"jsonrpc">> => <<"2.0">>
                     , <<"method">>  => legacy_to_method_out(Reply)
                     , <<"params">>  => result(Reply) }}
    end.

result(#{payload := Payload}) ->            
    Payload;
result(Result) -> 
    Result.


legacy_to_method_in(Action) ->
    <<"channels.", Action/binary>>.

legacy_to_method_in(Action, Tag) ->
    <<"channels.", Action/binary, ".", Tag/binary>>.

legacy_to_method_out(#{action := Action, tag := Tag}) ->
    <<"channels.", Action/binary, ".", Tag/binary>>.

reply(_, #handler{protocol = legacy}, no_reply) ->
    no_reply;
reply(Reply, #handler{protocol = jsonrpc} = H, _) ->
    json_rpc_reply(Reply, H).
