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
                  role               :: initiator | responder | undefined,
                  host               :: binary() | undefined,
                  port               :: non_neg_integer() | undefined}).

-opaque handler() :: #handler{}.
-export_type([handler/0]).
-define(ACTION_SIGNED(Action), Action =:= <<"initiator_sign">>;
                               Action =:= <<"deposit_tx">>;
                               Action =:= <<"deposit_ack">>;
                               Action =:= <<"withdraw_tx">>;
                               Action =:= <<"withdraw_ack">>;
                               Action =:= <<"responder_sign">>;
                               Action =:= <<"shutdown_sign">>;
                               Action =:= <<"shutdown_sign_ack">>;
                               Action =:= <<"update">>;
                               Action =:= <<"update_ack">>).
-define(ACTION_TAG(Action), case Action of
                                <<"initiator_sign">> -> create_tx;
                                <<"deposit_tx">> -> deposit_tx;
                                <<"deposit_ack">> -> deposit_created;
                                <<"withdraw_tx">> -> withdraw_tx;
                                <<"withdraw_ack">> -> withdraw_created;
                                <<"responder_sign">> -> funding_created;
                                <<"update">> -> update;
                                <<"update_ack">> -> update_ack;
                                <<"shutdown_sign">> -> shutdown;
                                <<"shutdown_sign_ack">> -> shutdown_ack;
                                <<"leave">> -> leave
                            end).

init(Req, _Opts) ->
    {cowboy_websocket, Req, maps:from_list(cowboy_req:parse_qs(Req))}.

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
websocket_handle({text, MsgBin}, State) ->
    try jsx:decode(MsgBin, [return_maps]) of
        Msg ->
            case process_incoming(Msg, State) of
                no_reply -> {ok, State};
                {reply, Resp} -> {reply, {text, jsx:encode(Resp)}, State};
                {error, _} -> {ok, State}
            end
    catch
      error:_ ->
          {ok, State}
    end;
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({push, ChannelId, Data, Options}, State) ->
    case ChannelId =:= channel_id(State) of
        false ->
            process_response({error, wrong_channel_id}, Options),
            {ok, State};
        true ->
            process_response(ok, Options),
            Msg = jsx:encode(Data),
            {reply, {text, Msg}, State}
    end;
websocket_info({aesc_fsm, FsmPid, Msg}, #handler{fsm_pid=FsmPid}=H) ->
    H1 = set_channel_id(Msg, H),
    case process_fsm(Msg, H) of
        %no_reply -> {ok, H1};
        %{error, _} -> {ok, H1}
        {reply, Resp} -> {reply, {text, jsx:encode(Resp)}, H1}
    end;
websocket_info({'DOWN', MRef, _, _, _}, #handler{fsm_mref = MRef} = H) ->
    {stop, H};
websocket_info(_Info, State) ->
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

reply(Msg, #handler{enc_channel_id = Id}) ->
    case Id of
        undefined -> {reply, Msg};
        _         -> {reply, maps:merge(#{channel_id => Id}, Msg)}
    end.

terminate(_Reason, _PartialReq, #{} = _State) ->
    % not initialized yet
    ok;
terminate(Reason, _PartialReq, State) ->
    FsmPid = fsm_pid(State),
    true = unlink(FsmPid),
    lager:debug("WebSocket dying because of ~p", [Reason]),
    ok = aesc_fsm:client_died(FsmPid),
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
process_incoming(#{<<"action">> := <<"update">>,
                   <<"tag">> := <<"new">>,
                   <<"payload">> := #{<<"from">>    := FromB,
                                      <<"to">>      := ToB,
                                      <<"amount">>  := Amount}} = R, State) ->
    case {aec_base58c:safe_decode(account_pubkey, FromB),
          aec_base58c:safe_decode(account_pubkey, ToB)} of
        {{ok, From}, {ok, To}} ->
            case aesc_fsm:upd_transfer(fsm_pid(State), From, To, Amount) of
                ok -> no_reply;
                {error, Reason} ->
                    {reply, error_response(R, Reason)}
            end;
        _ -> {reply, error_response(R, broken_encoding)}
    end;
process_incoming(#{<<"action">> := <<"update">>,
                   <<"tag">> := <<"new_contract">>,
                   <<"payload">> := #{<<"vm_version">> := VmVersion,
                                      <<"deposit">>    := Deposit,
                                      <<"code">>       := CodeE,
                                      <<"call_data">>  := CallDataE}} = R, State) ->
    case {hex_decode(CodeE), hex_decode(CallDataE)} of
        {{ok, Code}, {ok, CallData}} ->
            case aesc_fsm:upd_create_contract(fsm_pid(State),
                                              #{vm_version => VmVersion,
                                                deposit    => Deposit,
                                                code       => Code,
                                                call_data  => CallData}) of
                ok -> no_reply;
                {error, Reason} ->
                    {reply, error_response(R, Reason)}
            end;
        _ -> {reply, error_response(R, broken_hexcode)}
    end;
process_incoming(#{<<"action">> := <<"update">>,
                   <<"tag">> := <<"call_contract">>,
                   <<"payload">> := #{<<"contract">>   := ContractE,
                                      <<"vm_version">> := VmVersion,
                                      <<"amount">>     := Amount,
                                      <<"call_data">>  := CallDataE}} = R, State) ->
    case {aec_base58c:safe_decode(contract_pubkey, ContractE), hex_decode(CallDataE)} of
        {{ok, Contract}, {ok, CallData}} ->
            case aesc_fsm:upd_call_contract(fsm_pid(State),
                                            #{contract   => Contract,
                                              vm_version => VmVersion,
                                              amount     => Amount,
                                              call_data  => CallData}) of
                ok -> no_reply;
                {error, Reason} ->
                    {reply, error_response(R, Reason)}
            end;
        _ -> {reply, error_response(R, broken_hexcode)}
    end;
process_incoming(#{<<"action">> := <<"get">>,
                   <<"tag">> := <<"contract_call">>,
                   <<"payload">> := #{<<"contract">>   := ContractE,
                                      <<"caller">>     := CallerE,
                                      <<"round">>      := Round}} = R, State) ->
    case {aec_base58c:safe_decode(contract_pubkey, ContractE),
          aec_base58c:safe_decode(account_pubkey, CallerE)} of
        {{ok, Contract}, {ok, Caller}} ->
            case aesc_fsm:get_contract_call(fsm_pid(State),
                                            Contract, Caller, Round) of
                {ok, Call} ->
                    Resp = #{action => <<"get">>,
                             tag => <<"contract_call">>,
                             payload => aect_call:serialize_for_client(Call)},
                    {reply, Resp};
                {error, Reason} ->
                    {reply, error_response(R, Reason)}
            end;
        _ -> {reply, error_response(R, broken_hexcode)}
    end;
process_incoming(#{<<"action">> := <<"clean_contract_calls">>}, State) ->
    ok = aesc_fsm:prune_local_calls(fsm_pid(State)),
    {reply, ok_response(calls_prunned)};
process_incoming(#{<<"action">> := <<"get">>,
                   <<"tag">>    := <<"balances">>,
                   <<"payload">> := #{<<"accounts">> := Accounts}} = R, State) ->
    case safe_decode_account_keys(Accounts) of
        {ok, AccountKeys} ->
            case aesc_fsm:get_balances(
                   fsm_pid(State), [K || {_,K} <- AccountKeys]) of
                {ok, Balances} ->
                    Resp = #{action  => <<"get">>,
                             tag     => <<"balances">>,
                             payload => [#{<<"account">> => AcctExt,
                                           <<"balance">> => Bal}
                                         || {AcctExt, AcctInt} <- AccountKeys,
                                            {Acct, Bal} <- Balances,
                                            AcctInt =:= Acct]},
                    {reply, Resp};
                {error, Reason} ->
                    {reply, error_response(R, Reason)}
            end;
        {error, _} ->
            {reply, error_response(R, invalid_arguments)}
    end;
process_incoming(#{<<"action">> := <<"get">>,
                   <<"tag">> := <<"poi">>,
                   <<"payload">> := Filter} = R, State) ->
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
            {reply, error_response(R, <<"broken_encoding: ", What/binary>>)}
        end,
    case {Parse(account_pubkey, AccountsE), Parse(contract_pubkey, ContractsE)} of
        {{ok, Accounts0}, {ok, Contracts0}} ->
            Accounts = [{account, A} || A <- Accounts0 ++ Contracts0],
            Contracts = [{contract, C} || C <- Contracts0],
            case aesc_fsm:get_poi(fsm_pid(State), Accounts ++ Contracts) of
                {ok, PoI} ->
                    Resp = #{action => <<"get">>,
                             tag => <<"poi">>,
                             payload => #{
                                <<"poi">> => aec_base58c:encode(poi, aec_trees:serialize_poi(PoI))}},
                    {reply, Resp};
                {error, Reason} ->
                    {reply, error_response(R, Reason)}
            end;
      {{error, _},  {ok, _}}    -> BrokenEncodingReply(<<"accounts">>);
      {{ok, _},     {error, _}} -> BrokenEncodingReply(<<"contracts">>);
      {{error, _},  {error, _}} -> BrokenEncodingReply(<<"accounts, contracts">>)
    end;
process_incoming(#{<<"action">> := <<"message">>,
                   <<"payload">> := #{<<"to">>    := ToB,
                                      <<"info">>  := Msg}} = R, State) ->
    case aec_base58c:safe_decode(account_pubkey, ToB) of
        {ok, To} ->
            case aesc_fsm:inband_msg(fsm_pid(State), To, Msg) of
                ok -> no_reply;
                {error, Reason} ->
                    {reply, error_response(R, Reason)}
            end;
        _ -> {reply, error_response(R, broken_encoding)}
    end;
process_incoming(#{<<"action">> := <<"deposit">>,
                   <<"payload">> := #{<<"amount">>  := Amount}} = R, State) ->
    case aesc_fsm:upd_deposit(fsm_pid(State), #{amount => Amount}) of
        ok -> no_reply;
        {error, Reason} ->
            {reply, error_response(R, Reason)}
    end;
process_incoming(#{<<"action">> := <<"withdraw">>,
                   <<"payload">> := #{<<"amount">>  := Amount}} = R, State) ->
    case aesc_fsm:upd_withdraw(fsm_pid(State), #{amount => Amount}) of
        ok -> no_reply;
        {error, Reason} ->
            {reply, error_response(R, Reason)}
    end;
process_incoming(#{<<"action">> := Action,
                   <<"payload">> := #{<<"tx">> := EncodedTx}}, State)
    when ?ACTION_SIGNED(Action) ->
    Tag = ?ACTION_TAG(Action),
    case aec_base58c:safe_decode(transaction, EncodedTx) of
        {error, _} ->
            lager:warning("Channel WS: broken ~p tx ~p", [Action, EncodedTx]),
            {error, invalid_tx};
        {ok, TxBin} ->
             SignedTx = aetx_sign:deserialize_from_binary(TxBin),%TODO: check tx
             aesc_fsm:signing_response(fsm_pid(State), Tag, SignedTx),
             no_reply
    end;
process_incoming(#{<<"action">> := <<"leave">>}, State) ->
    lager:debug("Channel WS: leave channel message received"),
    aesc_fsm:leave(fsm_pid(State)),
    no_reply;
process_incoming(#{<<"action">> := <<"shutdown">>}, State) ->
    lager:warning("Channel WS: closing channel message received"),
    aesc_fsm:shutdown(fsm_pid(State)),
    no_reply;
process_incoming(#{<<"action">> := _} = Unhandled, _State) ->
    lager:warning("Channel WS: unhandled action received ~p", [Unhandled]),
    {error, unhandled};
process_incoming(Msg, _State) ->
    lager:warning("Channel WS: missing action received ~p", [Msg]),
    {error, unhandled}.

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
    reply(#{action => <<"sign">>,
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
    reply(#{action => Action,
            payload => Payload}, H);
process_fsm(#{type := Type, tag := Tag, info := Event}, _H) ->
    error({unparsed_fsm_event, Type, Tag, Event}).

prepare_handler(Params) ->
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
            case jobs:ask(ws_handlers_queue) of
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
        #handler{}, Validators).

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
        [Read(<<"initiator">>, initiator, #{type => {hash, account_pubkey}}),
         Read(<<"responder">>, responder, #{type => {hash, account_pubkey}}),
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

error_response(Req, Reason) ->
    #{<<"action">>  => <<"error">>,
      <<"payload">> => #{<<"request">> => Req,
                         <<"reason">> => Reason}}.

ok_response(Action) ->
    #{<<"action">>  => Action}.

hex_decode(Hex) ->
    try {ok, aeu_hex:hexstring_decode(Hex)}
    catch _:_ -> error
    end.

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
