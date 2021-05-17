-module(sc_ws_api_jsonrpc).

-behavior(sc_ws_api).

-export([unpack/1,
         error_response/3,
         error_code_to_msg/1,
         reply/3,
         notify/2,
         process_incoming/2
        ]).

-export([error_msg/1,
         error_data_msg/1]).

-export([process_request/2]).

-export([patterns/0]).

-define(JSONRPC_VERSION, <<"2.0">>).

-define(VERSION, 1).

-define(METHOD_SIGNED(Method), Method =:= <<"channels.initiator_sign">>;
                               Method =:= <<"channels.deposit_tx">>;
                               Method =:= <<"channels.deposit_ack">>;
                               Method =:= <<"channels.withdraw_tx">>;
                               Method =:= <<"channels.withdraw_ack">>;
                               Method =:= <<"channels.responder_sign">>;
                               Method =:= <<"channels.snapshot_solo_tx">>;
                               Method =:= <<"channels.snapshot_solo_sign">>;
                               Method =:= <<"channels.shutdown_sign">>;
                               Method =:= <<"channels.shutdown_sign_ack">>;
                               Method =:= <<"channels.update">>;
                               Method =:= <<"channels.update_ack">>;
                               Method =:= <<"channels.close_solo_tx">>;
                               Method =:= <<"channels.close_solo_sign">>;
                               Method =:= <<"channels.slash_tx">>;
                               Method =:= <<"channels.slash_sign">>;
                               Method =:= <<"channels.settle_tx">>;
                               Method =:= <<"channels.settle_sign">>;
                               Method =:= <<"channels.force_progress_tx">>;
                               Method =:= <<"channels.force_progress_sign">>
                               ).
-define(METHOD_TAG(Method), case Method of
                                <<"channels.initiator_sign">>     -> create_tx;
                                <<"channels.deposit_tx">>         -> deposit_tx;
                                <<"channels.deposit_ack">>        -> deposit_created;
                                <<"channels.withdraw_tx">>        -> withdraw_tx;
                                <<"channels.withdraw_ack">>       -> withdraw_created;
                                <<"channels.responder_sign">>     -> funding_created;
                                <<"channels.update">>             -> update;
                                <<"channels.update_ack">>         -> update_ack;
                                <<"channels.snapshot_solo_tx">>   -> snapshot_tx;
                                <<"channels.snapshot_solo_sign">> -> snapshot_solo_tx;
                                <<"channels.shutdown_sign">>      -> shutdown;
                                <<"channels.shutdown_sign_ack">>  -> shutdown_ack;
                                <<"channels.leave">>              -> leave;
                                <<"channels.close_solo_tx">>      -> close_solo_tx;
                                <<"channels.close_solo_sign">>    -> close_solo_tx;
                                <<"channels.slash_tx">>           -> slash_tx;
                                <<"channels.slash_sign">>         -> slash_tx;
                                <<"channels.settle_tx">>          -> settle_tx;
                                <<"channels.settle_sign">>        -> settle_tx;
                                <<"channels.force_progress_tx">>  -> force_progress_tx;
                                <<"channels.force_progress_sign">>-> force_progress_tx
                            end).

-include_lib("aeutils/include/aeu_stacktrace.hrl").
-include_lib("aechannel/src/aesc_codec.hrl").

%%%==================================================================
%%% Trace settings
%%%==================================================================

patterns() ->
    [{?MODULE, F, A, []} || {F, A} <- [ {reply, 3}
                                      , {notify, 2}
                                      , {process_request, 2}
                                      ]].

%%%===================================================================
%%% Callbacks
%%%===================================================================

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

error_code_to_msg(Code) ->
    aesc_fsm:error_code_to_msg(Code).

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

reply(no_reply, Msgs, ChannelId) ->
    case [{#{ payload => <<"ok">>}, M} || #{<<"id">> := _} = M <- Msgs] of
        [] -> no_reply;
        [_|_] = Replies ->
            reply({reply, Replies}, Msgs, ChannelId)
    end;
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
json_rpc_error_object(parse_error          , R) -> error_obj(-32700   , R);
json_rpc_error_object(invalid_request      , R) -> error_obj(-32000   , R);
json_rpc_error_object(unhandled            , R) -> error_obj(-32601   , R);

json_rpc_error_object(not_found            , R) -> error_obj(3, [100] , R);
json_rpc_error_object(broken_encoding      , R) -> error_obj(3, [104] , R);
json_rpc_error_object(broken_code          , R) -> error_obj(3, [104] , R);
json_rpc_error_object(value_too_low        , R) -> error_obj(3, [105] , R);
json_rpc_error_object(conflict             , R) -> error_obj(3, [107] , R);
json_rpc_error_object(not_ready            , R) -> error_obj(3, [108] , R);
json_rpc_error_object(insufficient_balance , R) -> error_obj(3, [1001], R);
json_rpc_error_object(negative_amount      , R) -> error_obj(3, [1002], R);
json_rpc_error_object(invalid_pubkeys      , R) -> error_obj(3, [1003], R);
json_rpc_error_object(call_not_found       , R) -> error_obj(3, [1004], R);
json_rpc_error_object(contract_init_failed , R) -> error_obj(3, [1007], R);
json_rpc_error_object(not_a_number         , R) -> error_obj(3, [1008], R);
json_rpc_error_object(participant_not_found, R) -> error_obj(3, [1011], R);
json_rpc_error_object(not_offchain_tx      , R) -> error_obj(2, [1012], R);
json_rpc_error_object(already_onchain      , R) -> error_obj(3, [1013], R);
json_rpc_error_object({meta, invalid}      , R) -> error_obj(3, [1014], R);
json_rpc_error_object(invalid_fsm_id       , R) -> error_obj(3, [1016], R);
json_rpc_error_object(bad_signature        , R) -> error_obj(3, [1017], R);
json_rpc_error_object(not_allowed_now      , R) -> error_obj(3, [1018], R);
json_rpc_error_object(wrong_abi_version    , R) -> error_obj(3, [1019], R);
json_rpc_error_object(wrong_vm_version     , R) -> error_obj(3, [1020], R);
json_rpc_error_object({broken_encoding,What}, R) ->
    error_obj(3, [broken_encoding_code(W) || W <- What], R);
json_rpc_error_object({What, missing}      , R) ->
    error_obj(3, [missing_field_code(What)], R);
json_rpc_error_object(Other                , R) ->
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
     , 108 => <<"Not ready">>
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
     , 1011 => <<"Participant not found">>
     , 1012 => <<"Offchain tx expected">>
     , 1013 => <<"Tx already on-chain">>
     , 1014 => <<"Invalid meta object">>
     , 1015 => <<"Invalid error code (expect 1...65535)">>
     , 1016 => <<"Invalid fsm id">>
     , 1017 => <<"Bad signature">>
     , 1018 => <<"Not allowed at current channel state">>
     , 1019 => <<"ABI version mismatch">>
     , 1020 => <<"VM version mismatch">>
     , 2000 => <<"Missing field: existing_fsm_id">>
     }.

broken_encoding_code(account    ) -> 1005;
broken_encoding_code(contract   ) -> 1006;
broken_encoding_code(bytearray  ) -> 1009;
broken_encoding_code(transaction) -> 1010.

%% TODO: Add more error codes and create a PT for it
missing_field_code(existing_fsm_id) -> 2000.

process_incoming(Msg, FsmPid) ->
    ResRev =
        lists:foldl(
            fun(Req, Accum) ->
                R =
                    try process_request(Req, FsmPid) of
                        {error, Err}   -> {error, Err};
                        no_reply       -> no_reply;
                        {reply, Reply} -> Reply
                    ?_catch_(error, Reason, StackTrace)
                        case Reason of
                            {validation_error, not_a_number} -> {error, not_a_number};
                            _ ->
                                lager:debug("CAUGHT E=~p / Req = ~p / ~p",
                                            [Reason, Req, StackTrace]),
                                no_reply
                        end
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
process_request(#{<<"method">> := <<"channels.system">>,
                  <<"params">> := #{<<"action">> := <<"stop">>}}, FsmPid) ->
    ok = aesc_fsm:stop(FsmPid),
    no_reply;
process_request(#{<<"method">> := <<"channels.history.fetch">> = M,
                  <<"params">> := Params}, FsmPid) ->
    apply_with_optional_params(
      M, Params,
      fun(XParams) ->
              get_history(FsmPid, XParams)
      end);
process_request(#{<<"method">> := <<"channels.update.new">> = M,
                   <<"params">> := #{<<"from">>    := FromB,
                                     <<"to">>      := ToB,
                                     <<"amount">>  := Amount} = Params}, FsmPid) ->
    assert_integer(Amount),
    case {aeser_api_encoder:safe_decode(account_pubkey, FromB),
          aeser_api_encoder:safe_decode(account_pubkey, ToB)} of
        {{ok, From}, {ok, To}} ->
            apply_with_optional_params(
              M, Params,
              fun(XParams) ->
                      aesc_fsm:upd_transfer(FsmPid, From, To, Amount, XParams)
              end);
        _ -> {error, {broken_encoding, [account]}}
    end;
process_request(#{<<"method">> := <<"channels.update.new_contract">> = M,
                  <<"params">> := #{<<"vm_version">>  := VmVersion,
                                    <<"abi_version">> := ABIVersion,
                                    <<"deposit">>     := Deposit,
                                    <<"code">>        := CodeE,
                                    <<"call_data">>   := CallDataE} = Params}, FsmPid) ->
    assert_integer(Deposit),
    assert_integer(VmVersion),
    assert_integer(ABIVersion),
    case {bytearray_decode(CodeE), bytearray_decode(CallDataE)} of
        {{ok, Code}, {ok, CallData}} ->
            MandatoryOpts =
                #{ vm_version  => VmVersion
                 , abi_version => ABIVersion
                 , deposit     => Deposit
                 , code        => Code
                 , call_data   => CallData },
            apply_with_optional_params(
              M, MandatoryOpts, Params,
              fun(XParams) ->
                      aesc_fsm:upd_create_contract(FsmPid, XParams)
              end);
        _ -> {error, {broken_encoding, [bytearray]}}
    end;
process_request(#{<<"method">> := <<"channels.update.call_contract">> = M,
                  <<"params">> := #{<<"contract_id">> := ContractE,
                                    <<"abi_version">> := ABIVersion,
                                    <<"amount">>      := Amount,
                                    <<"call_data">>   := CallDataE} = Params}, FsmPid) ->
    assert_integer(Amount),
    assert_integer(ABIVersion),
    case {aeser_api_encoder:safe_decode(contract_pubkey, ContractE),
          bytearray_decode(CallDataE)} of
        {{ok, Contract}, {ok, CallData}} ->
            MandatoryOpts =
                #{ contract    => Contract
                 , abi_version => ABIVersion
                 , amount      => Amount
                 , call_data   => CallData },
            apply_with_optional_params(
              M, MandatoryOpts, Params,
              fun(XOpts) ->
                      aesc_fsm:upd_call_contract(FsmPid, XOpts)
              end);
        {{error, _}, {ok, _}} -> {error, {broken_encoding, [contract]}};
        {{ok, _}, {error, _}} -> {error, {broken_encoding, [bytearray]}};
        {{error, _}, {error, _}} -> {error, {broken_encoding, [contract, bytearray]}}
    end;
process_request(#{<<"method">> := <<"channels.get.contract_call">>,
                  <<"params">> := #{<<"contract_id">>   := ContractE,
                                    <<"caller_id">>     := CallerE,
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
process_request(#{<<"method">> := <<"channels.dry_run.call_contract">> = M,
                  <<"params">> := #{<<"contract_id">> := ContractE,
                                    <<"abi_version">> := ABIVersion,
                                    <<"amount">>      := Amount,
                                    <<"call_data">>   := CallDataE} = Params}, FsmPid) ->
    assert_integer(Amount),
    assert_integer(ABIVersion),
    case {aeser_api_encoder:safe_decode(contract_pubkey, ContractE),
          bytearray_decode(CallDataE)} of
        {{ok, Contract}, {ok, CallData}} ->
            MandatoryOpts =
                #{ contract    => Contract
                 , abi_version => ABIVersion
                 , amount      => Amount
                 , call_data   => CallData },
            apply_with_optional_params(
              M, MandatoryOpts, Params,
              fun(XOpts) ->
                      case aesc_fsm:dry_run_contract(FsmPid, XOpts) of
                          {ok, Call} ->
                              {reply, #{ action     => <<"dry_run">>
                                       , tag        => <<"call_contract">>
                                       , {int,type} => reply
                                       , payload    => aect_call:serialize_for_client(Call) }};
                          {error, _Reason} = Err -> Err
                      end
              end);
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
                  <<"params">> := #{ <<"to">>    := ToB
                                   , <<"info">>  := Msg}}, FsmPid) ->
    case aeser_api_encoder:safe_decode(account_pubkey, ToB) of
        {ok, To} ->
            case aesc_fsm:inband_msg(FsmPid, To, Msg) of
                ok -> no_reply;
                {error, _Reason} = Err -> Err
            end;
        _ -> {error, {broken_encoding, [account]}}
    end;
process_request(#{<<"method">> := <<"channels.deposit">> = M,
                  <<"params">> := #{<<"amount">> := Amount} = Params}, FsmPid) ->
    assert_integer(Amount),
    apply_with_optional_params(
      M, #{amount => Amount}, Params,
      fun(XOpts) ->
              aesc_fsm:upd_deposit(FsmPid, XOpts)
      end);
process_request(#{<<"method">> := <<"channels.withdraw">> = M,
                  <<"params">> := #{<<"amount">> := Amount} = Params}, FsmPid) ->
    assert_integer(Amount),
    apply_with_optional_params(
      M, #{amount => Amount}, Params,
      fun(XOpts) ->
              aesc_fsm:upd_withdraw(FsmPid, XOpts)
      end);
process_request(#{<<"method">> := Method,
                  <<"params">> := #{<<"error">> := ErrorCode}}, FsmPid)
  when ?METHOD_SIGNED(Method) ->
    Tag = ?METHOD_TAG(Method),
    case valid_error_code(ErrorCode) of
        true ->
            case aesc_fsm:signing_response(FsmPid, Tag, {error, ErrorCode}) of
                ok -> no_reply;
                {error, _Reason} = Err -> Err
            end;
        false ->
            lager:warning("Recived unhandled method ~p", [Method]),
            {error, error_code}
    end;
process_request(#{<<"method">> := Method,
                  <<"params">> := #{<<"signed_tx">> := EncodedTx}}, FsmPid)
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
process_request(#{<<"method">> := <<"channels.shutdown">> = M} = Req, FsmPid) ->
    lager:warning("Channel WS: closing channel message received"),
    Params = maps:get(<<"params">>, Req, #{}),
    apply_with_optional_params(
      M, Params,
      fun(XOpts) ->
              aesc_fsm:shutdown(FsmPid, XOpts)
      end);
process_request(#{<<"method">> := <<"channels.close_solo">> = M} = Req, FsmPid) ->
    Params = maps:get(<<"params">>, Req, #{}),
    apply_with_optional_params(
      M, Params,
      fun(XOpts) ->
              aesc_fsm:close_solo(FsmPid, XOpts)
      end);
process_request(#{<<"method">> := <<"channels.snapshot_solo">> = M} = Req, FsmPid) ->
    lager:debug("Channel WS: snapshot_solo message received"),
    Params = maps:get(<<"params">>, Req, #{}),
    apply_with_optional_params(
      M, Params,
      fun(XOpts) ->
              aesc_fsm:snapshot_solo(FsmPid, XOpts)
      end);
process_request(#{<<"method">> := <<"channels.slash">> = M} = Req, FsmPid) ->
    lager:debug("Channel WS: slash message received"),
    Params = maps:get(<<"params">>, Req, #{}),
    apply_with_optional_params(
      M, Params,
      fun(XOpts) ->
              aesc_fsm:slash(FsmPid, XOpts)
      end);
process_request(#{<<"method">> := <<"channels.settle">> = M} = Req, FsmPid) ->
    lager:debug("Channel WS: settle message received"),
    Params = maps:get(<<"params">>, Req, #{}),
    apply_with_optional_params(
      M, Params,
      fun(XOpts) ->
              aesc_fsm:settle(FsmPid, XOpts)
      end);
process_request(#{<<"method">> := <<"channels.force_progress">> = M,
                  <<"params">> := #{<<"contract_id">> := ContractE,
                                    <<"abi_version">> := ABIVersion,
                                    <<"amount">>      := Amount,
                                    <<"call_data">>   := CallDataE,
                                    <<"gas_price">>   := GasPrice} = Params},
                FsmPid) ->
    Gas = maps:get(<<"gas">>, Params, 1000000),
    lager:debug("Channel WS: force_progress message received", []),
    assert_integer(Amount),
    assert_integer(ABIVersion),
    assert_integer(GasPrice),
    case {aeser_api_encoder:safe_decode(contract_pubkey, ContractE),
          bytearray_decode(CallDataE)} of
        {{ok, Contract}, {ok, CallData}} ->
            MandatoryOpts =
                #{ contract    => Contract
                 , abi_version => ABIVersion
                 , amount      => Amount
                 , call_data   => CallData
                 , gas_price   => GasPrice
                 , gas         => Gas },
            apply_with_optional_params(
              M, MandatoryOpts, Params,
              fun(XOpts) ->
                      lager:debug("force_progress params (~p)", [XOpts]),
                      aesc_fsm:force_progress(FsmPid, XOpts)
              end);
        _ -> {error, {broken_encoding, [account]}}
    end;
process_request(#{ <<"method">> := <<"channels.assume_minimum_depth">> = M
                 , <<"params">> := #{ <<"tx_hash">> := EncTxHash } = Params }, FsmPid) ->
    lager:debug("Channel WS: assume minimum depth: ~p", [EncTxHash]),
    case aeser_api_encoder:safe_decode(tx_hash, EncTxHash) of
        {ok, TxHash} ->
            apply_with_optional_params(
              M, #{tx_hash => TxHash}, Params,
              fun(XOpts) ->
                      aesc_fsm:assume_minimum_depth(FsmPid, maps:get(tx_hash, XOpts))
              end);
        _ -> {error, {broken_encoding, [tx_hash]}}
    end;
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

get_history(FsmPid, Params) ->
    History = aesc_fsm:get_history(FsmPid, Params),
    Result = lists:foldr(fun fold_history/2, [], History),
    {reply, #{ action      => <<"history">>
             , tag         => <<"fetch">>
             , {int, type} => reply
             , payload     => Result }}.

fold_history(Entry, Acc) ->
    case Entry of
        {rpt, Tag, TS, #{info := Info} = Msg} ->
            Payload = sc_ws_api:event_to_payload(Tag, Info, Msg, ?MODULE),
            acc_history(<<"report">>, Tag, TS, Payload, Acc);
        {Op, Tag, TS, Msg} when Op == snd; Op == rcv ->
            Info = case Msg of
                       {Tag, I} when is_map(I) ->
                           I;
                       {signed, TxType, I} ->
                           #{ tx_type => TxType
                            , tx => I };
                       I when is_map(I) ->
                           I
                   end,
            acc_history(snd_rcv_type(Op), Tag, TS, encode_info(Info), Acc);
        {req, Tag, TS, Msg} ->
            acc_history(<<"request">>, Tag, TS, encode_info(Msg), Acc);
        Other ->
            lager:debug("Other - ignoring: ~p", [Other]),
            Acc
    end.

-dialyzer({no_return, acc_history/5}).
acc_history(Type, Tag, TS, Info, Acc) ->
     [#{ type => Type
       , tag  => atom_to_binary(Tag, utf8)
       , time => format_date_time(TS)
       , info => encode_info(Info) } | Acc].

encode_info(Info) when is_map(Info) ->
    Enc = maps:map(fun encode_info_/2, Info),
    try_encode(Enc);
encode_info(Info) ->
    try_encode(Info).

try_encode(Enc) ->
    %% Ensure that JSX encode doesn't crash
    try  _ = jsx:encode(Enc),
         Enc
    catch
        error:E ->
            lager:debug("CANNOT encode (~p): ~p", [E, Enc]),
            <<"encode_error">>
    end.

encode_info_(K, V) ->
    try case {K, V} of
            {chain_hash, H} ->
                %% No predefined type for chain_hash - use bytearray
                aeser_api_encoder:encode(bytearray, H);
            {tx, Tx} when is_binary(Tx) ->
                aeser_api_encoder:encode(transaction, Tx);
            {tx, Tx} ->
                aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(Tx));
            {channel_id, Id} ->
                aeser_api_encoder:encode(channel, Id);
            {chan_id, Id} ->
                aeser_api_encoder:encode(channel, Id);
            {temporary_channel_id, Id} ->
                aeser_api_encoder:encode(channel, Id);
            {block_hash, H} ->
                aeser_api_encoder:encode(micro_block_hash, H);
            {tx_hash, H} ->
                aeser_api_encoder:encode(tx_hash, H);
            {_, Id} when K == initiator;
                         K == responder;
                         K == from;
                         K == to ->
                aeser_api_encoder:encode(account_pubkey, Id);
            {to, Id} ->
                aeser_api_encoder:encode(account_pubkey, Id);
            {signed_tx, Tx} ->
                aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(Tx));
            {channel, Ch} when is_map(Ch) ->
                Ch;
            {channel, Ch} ->
                aesc_channels:serialize_for_client(Ch);
            {info, I} ->
                encode_info(I);
            {data, D} when is_map(D) ->
                encode_info(D);
            {_, Bin} when is_binary(Bin) ->
                aeser_api_encoder:encode(bytearray, Bin);
            _ ->
                lager:debug("UNKNOWN K=~p, V=~p", [K, V]),
                V
        end
    catch
        error:E ->
            lager:debug("CAUGHT ~p, ~p := ~p", [E, K, V]),
            <<"encoding_error">>
    end.

snd_rcv_type(snd) -> <<"send">>;
snd_rcv_type(rcv) -> <<"receive">>.

%% In later OTP versions, there is a function in calendar for converting
%% timestamps to RFC3339. For now, use the rfc3339 lib (which we have already),
%% and stick to UTZ.
-dialyzer({nowarn_function, format_date_time/1}).
format_date_time({_,_,Us} = OSTime) ->
    {Date, Time} = calendar:now_to_universal_time(OSTime),
    {ok, Str} = rfc3339:format({Date, Time, Us, 0}),
    Str.

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

valid_error_code(ErrorCode) when is_integer(ErrorCode) ->
    ErrorCode >= 1 andalso ErrorCode =< 16#ffff;
valid_error_code(ErrorCode) when is_binary(ErrorCode) ->
    try valid_error_code(binary_to_integer(ErrorCode))
    catch
        error:_ ->
            false
    end;
valid_error_code(_) ->
    false.

apply_with_optional_params(Req, Params, F) ->
    apply_with_optional_params(Req, #{}, Params, F).

apply_with_optional_params(Req, Mandatory, Params, F) ->
    case merge_params(
           check_optional_params(optional_params(Req), Params),
           Mandatory) of
        {error, _} = Error ->
            Error;
        XParams when is_map(XParams) ->
            case F(XParams) of
                ok ->
                    no_reply;
                no_reply ->
                    no_reply;
                {reply, _} = Reply ->
                    Reply;
                {error, _} = Error1 ->
                    Error1
            end
    end.

merge_params({error, _} = Error, _) ->
    Error;
merge_params(Map1, Map2) when is_map(Map1), is_map(Map2) ->
    maps:merge(Map1, Map2).

optional_params(<<"channels.history.fetch">>        ) -> [n_param() | filter_params()];
optional_params(<<"channels.update.new">>           ) -> offchain_update_params();
optional_params(<<"channels.update.new_contract">>  ) -> offchain_update_params();
optional_params(<<"channels.update.call_contract">> ) -> offchain_update_params();
optional_params(<<"channels.dry_run.call_contract">>) -> offchain_update_params();
optional_params(<<"channels.deposit">>      ) -> [meta_param() , bh_param() | onchain_params()];
optional_params(<<"channels.withdraw">>     ) -> [meta_param() , bh_param() | onchain_params()];
optional_params(<<"channels.shutdown">>     ) -> onchain_params(); 
optional_params(<<"channels.close_solo">>   ) -> onchain_params();
optional_params(<<"channels.snapshot_solo">>) -> onchain_params();
optional_params(<<"channels.slash">>        ) -> onchain_params();
optional_params(<<"channels.settle">>       ) -> onchain_params();
optional_params(<<"channels.assume_minimum_depth">>) -> [];
optional_params(<<"channels.force_progress">>) -> [nonce_param()].

check_optional_params(OptionalKeys, Params) ->
    Read = sc_ws_utils:read_f(Params),
    sc_ws_utils:check_params(
        lists:map(
            fun({JSONKey, FSMKey, Opts}) ->
                Read(JSONKey, FSMKey, Opts#{mandatory => false})
            end,
            OptionalKeys)).

offchain_update_params() ->
    [bh_param(), meta_param()].

onchain_params() ->
    [nonce_param()] ++ fee_and_gas_price_params().

bh_param() ->
    {<<"block_hash">>, block_hash, #{ type => {hash, block_hash} }}.

meta_param() ->
    {<<"meta">>, meta, #{ type => {list, #{type => binary}} }}.

fee_and_gas_price_params() ->
    [nonce_param(), fee_param(), gas_price_param()].

fee_param() ->
    {<<"fee">>, fee, #{ type => integer }}.

gas_price_param() ->
    {<<"gas_price">>, gas_price, #{ type => integer }}.

nonce_param() ->
    {<<"nonce">>, nonce, #{ type => integer }}.

n_param() ->
    {<<"n">>, n, #{ type => integer }}.

filter_params() ->
    [ {<<"type">>, type, #{ type      => {list, #{type => atom, enum => [rpt, rcv, snd, req]} }
                          , mandatory => false }}
    , {<<"tag">>, tag, #{ type      => {list, #{type => atom, enum => tag_enums() }}
                        , mandatory => false }}
    ].

tag_enums() ->
    %% TODO: possibly add filter tags for other types than 'rpt'.
    [ info, on_chain_tx, conflict, update, leave, error, debug ].
