-module(sc_ws_api_jsonrpc).

-behavior(sc_ws_api).

-export([unpack/1,
         error_response/3,
         reply/3,
         notify/2,
         process_incoming/2
        ]).

-export([error_msg/1,
         error_data_msg/1]).

-export([process_request/2]).

-define(JSONRPC_VERSION, <<"2.0">>).

-define(VERSION, 1).

-define(METHOD_SIGNED(Method), Method =:= <<"channels.initiator_sign">>;
                               Method =:= <<"channels.deposit_tx">>;
                               Method =:= <<"channels.deposit_ack">>;
                               Method =:= <<"channels.withdraw_tx">>;
                               Method =:= <<"channels.withdraw_ack">>;
                               Method =:= <<"channels.responder_sign">>;
                               Method =:= <<"channels.shutdown_sign">>;
                               Method =:= <<"channels.shutdown_sign_ack">>;
                               Method =:= <<"channels.update">>;
                               Method =:= <<"channels.update_ack">>;
                               Method =:= <<"channels.close_solo_tx">>;
                               Method =:= <<"channels.close_solo_sign">>;
                               Method =:= <<"channels.slash_tx">>;
                               Method =:= <<"channels.slash_sign">>;
                               Method =:= <<"channels.settle_tx">>;
                               Method =:= <<"channels.settle_sign">>).
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
                                <<"channels.leave">>             -> leave;
                                <<"channels.close_solo_tx">>     -> close_solo_tx;
                                <<"channels.close_solo_sign">>   -> close_solo_tx;
                                <<"channels.slash_tx">>          -> slash_tx;
                                <<"channels.slash_sign">>        -> slash_tx;
                                <<"channels.settle_tx">>         -> settle_tx;
                                <<"channels.settle_sign">>       -> settle_tx
                            end).

unpack(Msg) when is_map(Msg) ->
    unpack([Msg]);
unpack(Msg) ->
    lists:all(
        fun(#{ <<"jsonrpc">> := ?JSONRPC_VERSION
             , <<"method">>  := _Method }) -> true;
           (_) -> throw({decode_error, invalid_request})
        end,
        Msg),
    Msg.

error_response(Reason, Req, ChannelId) ->
    {reply, #{ <<"jsonrpc">>    => ?JSONRPC_VERSION
             , <<"version">>    => ?VERSION
             , <<"id">>         => error_id(Req)
             , <<"channel_id">> => ChannelId
             , <<"error">>      => json_rpc_error_object(Reason, Req) }
    }.

notify(Msg, ChannelId) ->
    {reply, #{ <<"jsonrpc">> => ?JSONRPC_VERSION
             , <<"version">>    => ?VERSION
             , <<"method">>  => method_out(Msg)
             , <<"params">>  => #{<<"data">> => result(Msg)
                                , <<"channel_id">> => ChannelId} }
    }.

reply(no_reply, _, _) -> no_reply;
reply(stop, _, _)     -> stop;
reply({reply,  L}, WholeMsg, ChannelId) when is_list(L) ->
    case [reply({reply, Reply}, WholeMsg, ChannelId) || Reply <- L] of
        [{reply, R}] -> {reply, R};
        R ->
            {reply, [Resp || {reply, Resp} <- R]}
    end;
reply({reply, {{error, Err}, Req}}, _, ChannelId) ->
    error_response(Err, Req, ChannelId);
reply({reply, {Reply, #{<<"id">> := Id}}}, _, ChannelId) ->
    {reply, #{ <<"jsonrpc">> => ?JSONRPC_VERSION
             , <<"version">>    => ?VERSION
             , <<"channel_id">> => ChannelId
             , <<"id">>      => Id
             , <<"result">>  => result(Reply) }
    };
reply({reply, {Reply, #{}}}, _, ChannelId) -> % no id
    notify(Reply, ChannelId).

result(#{payload := Payload0} = R) ->
    Payload = clean_reply(Payload0),
    case {Payload, R} of
        {#{channel_id := _}, _} ->
            Payload;
        {_, #{channel_id := Id}} when is_map(Payload) ->
            Payload#{<<"channel_id">> => Id};
        _ ->
            Payload
    end;
result(Result) ->
    clean_reply(Result).

clean_reply(Map) when is_map(Map) ->
    maps:filter(fun(K,_) ->
                        is_atom(K) orelse is_binary(K)
                end, Map);
clean_reply(Msg) -> Msg.

error_id(#{ <<"id">> := Id }) -> Id;
error_id(_) ->
    null.

%% JSON-RPC error objects. Try to follow
%% https://github.com/ethereum/wiki/wiki/JSON-RPC-Error-Codes-Improvement-Proposal
json_rpc_error_object(parse_error         , R) -> error_obj(-32700        , R);
json_rpc_error_object(invalid_request     , R) -> error_obj(-32000        , R);
json_rpc_error_object(unhandled           , R) -> error_obj(-32601        , R);
json_rpc_error_object(broken_encoding     , R) -> error_obj(3     , [104] , R);
json_rpc_error_object(broken_code         , R) -> error_obj(3     , [104] , R);
json_rpc_error_object(conflict            , R) -> error_obj(3     , [107] , R);
json_rpc_error_object(insufficient_balance, R) -> error_obj(3     , [1001], R);
json_rpc_error_object(negative_amount     , R) -> error_obj(3     , [1002], R);
json_rpc_error_object(invalid_pubkeys     , R) -> error_obj(3     , [1003], R);
json_rpc_error_object(call_not_found      , R) -> error_obj(3     , [1004], R);
json_rpc_error_object(contract_init_failed, R) -> error_obj(3     , [1007], R);
json_rpc_error_object(not_a_number        , R) -> error_obj(3     , [1008], R);
json_rpc_error_object({broken_encoding,What}, R) ->
    error_obj(3, [broken_encoding_code(W) || W <- What], R);
json_rpc_error_object(not_found           , R) -> error_obj(3     , [100] , R);
json_rpc_error_object(Other               , R) ->
    lager:debug("Unrecognized error reason: ~p", [Other]),
    error_obj(-32603        , R).

error_obj(Code, OrigReq) when is_map(OrigReq) ->
    #{ <<"code">>    => Code
     , <<"message">> => error_msg(Code)
     , <<"request">> => OrigReq };
error_obj(Code, undefined) ->
    #{ <<"code">>    => Code
     , <<"message">> => error_msg(Code) }.

error_obj(Code, Data, OrigReq) when is_map(OrigReq) ->
    #{ <<"code">>    => Code
     , <<"message">> => error_msg(Code)
     , <<"data">>    => error_data(Data)
     , <<"request">> => OrigReq };
error_obj(Code, Data, undefined) ->
    #{ <<"code">>    => Code
     , <<"message">> => error_msg(Code)
     , <<"data">>    => error_data(Data) }.

error_msg(Code) ->
    maps:get(Code, error_msgs(), <<"Unknown error">>).

error_msgs() ->
    #{
       -32700 => <<"Parse error">>
     , -32000 => <<"Invalid request">>
     , -32601 => <<"Method not found">>
     , -32602 => <<"Invalid params">>
     , -32603 => <<"Internal error">>
       %% Ethereum application error codes
     , 1      => <<"Unauthorized">>
     , 2      => <<"Action not allowed">>
     , 3      => <<"Rejected">>
     }.

error_data(Codes) ->
    [ #{ <<"code">>    => C
       , <<"message">> => error_data_msg(C) } || C <- Codes].

%% Mimicking Ethereum suggested custom error codes (not all relevant here)
error_data_msg(Code) ->
    maps:get(Code, error_data_msgs(), <<"Unknown error">>).

error_data_msgs() ->
    #{
       100 => <<"X doesn't exist">>
     , 101 => <<"Requires coin">>
     , 102 => <<"Gas too low">>
     , 103 => <<"Gas limit exceeded">>
     , 104 => <<"Rejected">>
     , 105 => <<"Value too low">>
     , 106 => <<"Timeout">>
     , 107 => <<"Conflict">>
     %% Aeternity error codes
     , 1001 => <<"Insufficient balance">>
     , 1002 => <<"Negative amount">>
     , 1003 => <<"Invalid pubkeys">>
     , 1004 => <<"Call not found">>
     , 1005 => <<"Broken encoding: account pubkey">>
     , 1006 => <<"Broken encoding: contract pubkey">>
     , 1007 => <<"Contract init failed">>
     , 1008 => <<"Not a number">>
     , 1009 => <<"Broken encoding: contract bytearray">>
     , 1010 => <<"Broken encoding: transaction">>
     }.

broken_encoding_code(account    ) -> 1005;
broken_encoding_code(contract   ) -> 1006;
broken_encoding_code(bytearray  ) -> 1009;
broken_encoding_code(transaction) -> 1010.

process_incoming(Msg, FsmPid) ->
    ResRev =
        lists:foldl(
            fun(Req, Accum) ->
                R =
                    try process_request(Req, FsmPid) of
                        {error, Err}   -> {error, Err};
                        no_reply       -> no_reply;
                        {reply, Reply} -> Reply
                    catch
                        error:{validation_error, not_a_number} ->
                            {error, not_a_number};
                        error:E ->
                            lager:debug("CAUGHT E=~p / Req = ~p / ~p",
                                        [E, Req, erlang:get_stacktrace()]),
                            no_reply
                    end,
                case R of
                    no_reply -> Accum;
                    V -> [{V, Req} | Accum]
                end
              end,
              [],
              Msg),
    Res = lists:reverse(ResRev),
    case Res of
        [] -> no_reply;
        [_|_] -> {reply, Res}
    end.

-spec process_request(map(), pid()) -> no_reply | {reply, map()} | {error, term()}.
process_request(#{<<"method">> := <<"channels.system">>,
                  <<"params">> := #{<<"action">> := <<"ping">>}}, _FsmPid) ->
    {reply, #{action => system, tag => pong}};
process_request(#{<<"method">> := <<"channels.update.new">>,
                   <<"params">> := #{<<"from">>    := FromB,
                                     <<"to">>      := ToB,
                                     <<"amount">>  := Amount}}, FsmPid) ->
    assert_integer(Amount),
    case {aeser_api_encoder:safe_decode(account_pubkey, FromB),
          aeser_api_encoder:safe_decode(account_pubkey, ToB)} of
        {{ok, From}, {ok, To}} ->
            case aesc_fsm:upd_transfer(FsmPid, From, To, Amount) of
                ok -> no_reply;
                {error, _Reason} = Err -> Err
            end;
        _ -> {error, {broken_encoding, [account]}}
    end;
process_request(#{<<"method">> := <<"channels.update.new_contract">>,
                  <<"params">> := #{<<"vm_version">>  := VmVersion,
                                    <<"abi_version">> := ABIVersion,
                                    <<"deposit">>     := Deposit,
                                    <<"code">>        := CodeE,
                                    <<"call_data">>   := CallDataE}}, FsmPid) ->
    assert_integer(Deposit),
    assert_integer(VmVersion),
    assert_integer(ABIVersion),
    case {bytearray_decode(CodeE), bytearray_decode(CallDataE)} of
        {{ok, Code}, {ok, CallData}} ->
            case aesc_fsm:upd_create_contract(FsmPid,
                                              #{vm_version  => VmVersion,
                                                abi_version => ABIVersion,
                                                deposit     => Deposit,
                                                code        => Code,
                                                call_data   => CallData}) of
                ok -> no_reply;
                {error, _Reason} = Err -> Err
            end;
        _ -> {error, {broken_encoding, [bytearray]}}
    end;
process_request(#{<<"method">> := <<"channels.update.new_contract_from_onchain">>,
                  <<"params">> := #{<<"deposit">>     := Deposit,
                                    <<"contract">>    := OnChainContractE,
                                    <<"call_data">>   := CallDataE}}, FsmPid) ->
    assert_integer(Deposit),
    case {aeser_api_encoder:safe_decode(contract_pubkey, OnChainContractE),
          bytearray_decode(CallDataE)} of
        {{ok, OnChainContract}, {ok, CallData}} ->
            case aec_chain:get_contract(OnChainContract) of
                {ok, Contract} ->
                    case aesc_fsm:upd_create_contract(FsmPid,
                        #{vm_version    => aect_contracts:vm_version(Contract),
                            abi_version => aect_contracts:abi_version(Contract),
                            deposit     => Deposit,
                            code        => aect_contracts:code(Contract),
                            call_data   => CallData}) of
                        ok -> no_reply;
                        {error, _Reason} = Err -> Err
                    end;
                {error, _Reason} = Err -> Err
            end;
        {{error, _}, {ok, _}} -> {error, {broken_encoding, [contract]}};
        {{ok, _}, {error, _}} -> {error, {broken_encoding, [bytearray]}};
        {{error, _}, {error, _}} -> {error, {broken_encoding, [contract, bytearray]}}
    end;
process_request(#{<<"method">> := <<"channels.update.call_contract">>,
                  <<"params">> := #{<<"contract">>    := ContractE,
                                    <<"abi_version">> := ABIVersion,
                                    <<"amount">>      := Amount,
                                    <<"call_data">>   := CallDataE}}, FsmPid) ->
    assert_integer(Amount),
    assert_integer(ABIVersion),
    case {aeser_api_encoder:safe_decode(contract_pubkey, ContractE),
          bytearray_decode(CallDataE)} of
        {{ok, Contract}, {ok, CallData}} ->
            case aesc_fsm:upd_call_contract(FsmPid,
                                            #{contract    => Contract,
                                              abi_version => ABIVersion,
                                              amount      => Amount,
                                              call_data   => CallData}) of
                ok -> no_reply;
                {error, _Reason} = Err -> Err
            end;
        {{error, _}, {ok, _}} -> {error, {broken_encoding, [contract]}};
        {{ok, _}, {error, _}} -> {error, {broken_encoding, [bytearray]}};
        {{error, _}, {error, _}} -> {error, {broken_encoding, [contract, bytearray]}}
    end;
process_request(#{<<"method">> := <<"channels.get.contract_call">>,
                  <<"params">> := #{<<"contract">>   := ContractE,
                                    <<"caller">>     := CallerE,
                                    <<"round">>      := Round}}, FsmPid) ->
    assert_integer(Round),
    case {aeser_api_encoder:safe_decode(contract_pubkey, ContractE),
          aeser_api_encoder:safe_decode(account_pubkey, CallerE)} of
        {{ok, Contract}, {ok, Caller}} ->
            case aesc_fsm:get_contract_call(FsmPid,
                                            Contract, Caller, Round) of
                {ok, Call} ->
                  {reply, #{ action     => <<"get">>
                           , tag        => <<"contract_call">>
                           , {int,type} => reply
                           , payload    => aect_call:serialize_for_client(Call) }};
                {error, _Reason} = Err -> Err
            end;
        {{error, _}, {ok, _}} -> {error, {broken_encoding, [contract]}};
        {{ok, _}, {error, _}} -> {error, {broken_encoding, [account]}};
        {{error, _}, {error, _}} -> {error, {broken_encoding, [contract, account]}}
    end;
process_request(#{<<"method">> := <<"channels.dry_run.call_contract">>,
                  <<"params">> := #{<<"contract">>    := ContractE,
                                    <<"abi_version">> := ABIVersion,
                                    <<"amount">>      := Amount,
                                    <<"call_data">>   := CallDataE}}, FsmPid) ->
    assert_integer(Amount),
    assert_integer(ABIVersion),
    case {aeser_api_encoder:safe_decode(contract_pubkey, ContractE),
          bytearray_decode(CallDataE)} of
        {{ok, Contract}, {ok, CallData}} ->
            case aesc_fsm:dry_run_contract(FsmPid,
                                           #{contract     => Contract,
                                             abi_version  => ABIVersion,
                                             amount       => Amount,
                                             call_data    => CallData}) of
                {ok, Call} ->
                  {reply, #{ action     => <<"dry_run">>
                           , tag        => <<"call_contract">>
                           , {int,type} => reply
                           , payload    => aect_call:serialize_for_client(Call) }};
                {error, _Reason} = Err -> Err
            end;
        {{error, _}, {ok, _}} -> {error, {broken_encoding, [contract]}};
        {{ok, _}, {error, _}} -> {error, {broken_encoding, [bytearray]}};
        {{error, _}, {error, _}} -> {error, {broken_encoding, [contract, bytearray]}}
    end;
process_request(#{<<"method">> := <<"channels.clean_contract_calls">>}, FsmPid) ->
    ok = aesc_fsm:prune_local_calls(FsmPid),
    {reply, ok_response(calls_pruned)};
process_request(#{<<"method">> := <<"channels.get.balances">>,
                  <<"params">> := #{<<"accounts">> := Accounts}}, FsmPid) ->
    case safe_decode_account_keys(Accounts) of
        {ok, AccountKeys} ->
            case aesc_fsm:get_balances(
                   FsmPid, [K || {_,K} <- AccountKeys]) of
                {ok, Balances} ->
                    Resp = [#{<<"account">> => AcctExt,
                              <<"balance">> => Bal}
                            || {AcctExt, AcctInt} <- AccountKeys,
                               {Acct, Bal} <- Balances,
                               AcctInt =:= Acct],
                    {reply, #{action     => <<"get">>
                            , tag        => <<"balances">>
                            , {int,type} => reply
                            , payload    => Resp }};
                {error, _Reason} = Err -> Err
            end;
        {error, _} ->
            {error, {broken_encoding, [account]}}
    end;
process_request(#{<<"method">> := <<"channels.get.offchain_state">>,
                  <<"params">> := #{}}, FsmPid) ->
    case aesc_fsm:get_offchain_state(FsmPid) of
        {ok, State} ->
            {reply, #{action => <<"get">>
                    , tag    => <<"offchain_state">>
                    , {int, type} => reply
                    , payload => aesc_offchain_state:serialize_for_client(State) }}
    end;
process_request(#{<<"method">> := <<"channels.get.contract">>,
                  <<"params">> := #{<<"pubkey">> := PubkeyE}}, FsmPid) ->
    case aeser_api_encoder:safe_decode(contract_pubkey, PubkeyE) of
        {ok, Pubkey} ->
            case aesc_fsm:get_contract(FsmPid, Pubkey) of
                {ok, Contract} ->
                    ContractState = aect_contracts:state(Contract),
                    SerializedContract = aect_contracts:serialize_for_client(Contract),
                    Resp = #{<<"contract">> => SerializedContract,
                             <<"contract_state">> => aect_contracts_store:serialize_for_client(ContractState)},
                    {reply, #{action => <<"get">>
                            , tag    => <<"contract">>
                            , {int, type} => reply
                            , payload => Resp }};
                {error, _Reason} = Err -> Err
            end;
        {error, _Reason} ->
            {error, {broken_encoding, [contract]}}
    end;
process_request(#{<<"method">> := <<"channels.get.poi">>,
                  <<"params">> := Filter}, FsmPid) ->
    AccountsE   = maps:get(<<"accounts">>, Filter, []),
    ContractsE  = maps:get(<<"contracts">>, Filter, []),
    Parse =
        fun(T, Keys) ->
            try {ok, lists:foldr(
                      fun(K, Acc) ->
                              {ok, Res} = aeser_api_encoder:safe_decode(T, K),
                              [Res | Acc]
                      end, [], Keys)}
            catch
                error:_ ->
                    {error, broken_encoding}
            end
        end,

    case {Parse(account_pubkey, AccountsE), Parse(contract_pubkey, ContractsE)} of
        {{ok, Accounts0}, {ok, Contracts0}} ->
            Accounts = [{account, A} || A <- Accounts0 ++ Contracts0],
            Contracts = [{contract, C} || C <- Contracts0],
            case aesc_fsm:get_poi(FsmPid, Accounts ++ Contracts) of
                {ok, PoI} ->
                    Resp = #{
                      action      => <<"get">>,
                      tag         => <<"poi">>,
                      {int,type}  => reply,
                      payload     => #{
                        <<"poi">> => aeser_api_encoder:encode(
                                       poi, aec_trees:serialize_poi(PoI))
                       }
                     },
                    {reply, Resp};
                {error, _Reason} = Err -> Err
            end;
        {{error, _},  {ok, _}}    -> {error, {broken_encoding, [account]}};
        {{ok, _},     {error, _}} -> {error, {broken_encoding, [contract]}};
        {{error, _},  {error, _}} -> {error, {broken_encoding, [account, contract]}}
    end;
process_request(#{<<"method">> := <<"channels.message">>,
                    <<"params">> := #{<<"to">>    := ToB,
                                      <<"info">>  := Msg}}, FsmPid) ->
    case aeser_api_encoder:safe_decode(account_pubkey, ToB) of
        {ok, To} ->
            case aesc_fsm:inband_msg(FsmPid, To, Msg) of
                ok -> no_reply;
                {error, _Reason} = Err -> Err
            end;
        _ -> {error, {broken_encoding, [account]}}
    end;
process_request(#{<<"method">> := <<"channels.deposit">>,
                    <<"params">> := #{<<"amount">>  := Amount}}, FsmPid) ->
    assert_integer(Amount),
    case aesc_fsm:upd_deposit(FsmPid, #{amount => Amount}) of
        ok -> no_reply;
        {error, _Reason} = Err -> Err
    end;
process_request(#{<<"method">> := <<"channels.withdraw">>,
                    <<"params">> := #{<<"amount">>  := Amount}}, FsmPid) ->
    assert_integer(Amount),
    case aesc_fsm:upd_withdraw(FsmPid, #{amount => Amount}) of
        ok -> no_reply;
        {error, _Reason} = Err -> Err
    end;
process_request(#{<<"method">> := Method,
                  <<"params">> := #{<<"tx">> := EncodedTx}}, FsmPid)
    when ?METHOD_SIGNED(Method) ->
    Tag = ?METHOD_TAG(Method),
    case aeser_api_encoder:safe_decode(transaction, EncodedTx) of
        {error, _} ->
            lager:warning("Channel WS: broken ~p tx ~p", [Method, EncodedTx]),
            {error, {broken_encoding,[transaction]}};
        {ok, TxBin} ->
            SignedTx = aetx_sign:deserialize_from_binary(TxBin),%TODO: check tx
            aesc_fsm:signing_response(FsmPid, Tag, SignedTx),
            no_reply
    end;
process_request(#{<<"method">> := <<"channels.leave">>}, FsmPid) ->
    lager:debug("Channel WS: leave channel message received"),
    aesc_fsm:leave(FsmPid),
    no_reply;
process_request(#{<<"method">> := <<"channels.shutdown">>}, FsmPid) ->
    lager:warning("Channel WS: closing channel message received"),
    aesc_fsm:shutdown(FsmPid),
    no_reply;
process_request(#{<<"method">> := <<"channels.close_solo">>}, FsmPid) ->
    lager:debug("Channel WS: close_solo message received"),
    aesc_fsm:close_solo(FsmPid),
    no_reply;
process_request(#{<<"method">> := <<"channels.slash">>}, FsmPid) ->
    lager:debug("Channel WS: slash message received"),
    aesc_fsm:slash(FsmPid),
    no_reply;
process_request(#{<<"method">> := <<"channels.settle">>}, FsmPid) ->
    lager:debug("Channel WS: settle message received"),
    aesc_fsm:settle(FsmPid),
    no_reply;
process_request(#{<<"method">> := _} = Unhandled, _FsmPid) ->
    lager:warning("Channel WS: unhandled action received ~p", [Unhandled]),
    {error, unhandled};
process_request(Msg, _FsmPid) ->
    lager:warning("Channel WS: missing action received ~p", [Msg]),
    {error, unhandled}.

ok_response(Action) ->
    #{ action     => Action
     , {int,type} => reply }.

safe_decode_account_keys(Keys) ->
    try {ok, lists:foldr(
               fun(K, Acc) ->
                       {ok, Res} = aeser_api_encoder:safe_decode(account_pubkey, K),
                       [{K, Res}|Acc]
               end, [], Keys)}
    catch
        error:_ ->
            {error, {broken_encoding, [account]}}
    end.

bytearray_decode(Bytearray) ->
    aeser_api_encoder:safe_decode(contract_bytearray, Bytearray).

method_out(#{action := Action, tag := none} = Msg) ->
    opt_type(Msg, <<"channels.", (bin(Action))/binary>>);
method_out(#{action := Action, tag := Tag} = Msg) ->
    opt_type(Msg, <<"channels.", (bin(Action))/binary, ".", (bin(Tag))/binary>>);
method_out(#{action := Action} = Msg) ->
    opt_type(Msg, <<"channels.", (bin(Action))/binary>>).

opt_type(#{ {int,type} := T }, Bin) ->
    <<Bin/binary, ".", (bin(T))/binary>>;
opt_type(_, Bin) ->
    Bin.

bin(A) when is_atom(A)   -> atom_to_binary(A, utf8);
bin(B) when is_binary(B) -> B.

assert_integer(Value) when is_integer(Value) -> ok;
assert_integer(_Value) -> error({validation_error, not_a_number}).

