-module(aehttp_helpers).

-export([ process_request/2
        , when_stable/1
        , parse_map_to_atom_keys/0
        , read_required_params/1
        , read_optional_params/1
        , api_decode/1
        , api_optional_decode/1
        , api_conditional_decode/1
        , api_str_to_int/1
        , contract_bytearray_params_decode/1
        , ttl_decode/1
        , poi_decode/1
        , delegates_decode/1
        , relative_ttl_decode/1
        , nameservice_pointers_decode/1
        , get_nonce/1
        , get_nonce_from_account_id/1
        , print_state/0
        , get_contract_code/2
        , get_info_object_from_tx/3
        , verify_oracle_existence/1
        , verify_oracle_query_existence/2
        , verify_name/1
        , read_optional_param/3
        , get_poi/3
        , get_block_from_chain/1
        , get_block_hash_optionally_by_hash_or_height/1
        , safe_get_txs/1
        , do_dry_run/0
        , dry_run_results/1
        , to_int/1
        ]).

-export([ get_transaction/2
        , encode_transaction/2
        , decode_pointers/1
        , decode_transaction/1
        ]).

-export([ ok_response/1
        , unsigned_tx_response/1]).

-import(aeu_debug, [pp/1]).

-define(MODE_WAIT_TIMEOUT, 30000).

process_request(FunsList, Req) ->
    process_request(FunsList, Req, #{}).

process_request([], _Req, {Code, _, _Response} = Result)
    when is_integer(Code) ->
    Result;
process_request([Fun | T], Req, Result0) ->
    Res = Fun(Req, Result0),
    case Res of
        ok ->
            process_request(T, Req, Result0);
        {ok, Result} ->
            process_request(T, Req, Result);
        {ok, Req1, Result} ->
            process_request(T, Req1, Result);
        {error, ErrMsg}->
            ErrMsg
    end.

when_stable(F) ->
    case app_ctrl:await_stable_mode(?MODE_WAIT_TIMEOUT) of
        {ok, _} -> F();
        {timeout,_} ->
            error(timeout)
    end.

read_required_params(ParamNames) ->
    params_read_fun(ParamNames,
        fun(Name, Req, _) ->
            case maps:get(Name, Req) of
                undefined -> error;
                Val -> {ok, Val}
            end
        end,
        "Not found").

read_optional_params(Params) ->
    params_read_fun(Params,
        fun({Name, Default}, Req, _) ->
            %% swagger puts an 'undefined' value for missing not reqired
            %% params
            case maps:get(Name, Req, undefined) of
                undefined when Default == '$no_value' -> no_value;
                undefined                             -> {ok, Default};
                V                                     -> {ok, V}
            end
        end,
        "Not found").

api_optional_decode(Params) ->
    params_read_fun([ {K, K, T} || {K, T} <- Params ],
                    fun api_opt_decode_fun/3, "Invalid option").

api_opt_decode_fun({Name, Type}, _, Data) ->
    Encoded = maps:get(Name, Data),
    case aeser_api_encoder:safe_decode(Type, Encoded) of
        {ok, Val}  -> {ok, Val};
        {error, _} -> {ok, Encoded}
    end.

api_conditional_decode(Params) ->
    params_read_fun([ {K, K, T} || {K, T} <- Params ],
                    fun api_cond_decode_fun/3, "Invalid decode").

api_str_to_int(Params) ->
    params_read_fun(Params,
        fun(Name, Req, _) ->
            %% swagger puts an 'undefined' value for missing not reqired
            %% params
            case maps:get(Name, Req, undefined) of
                undefined             -> no_value;
                Str when is_list(Str) -> {ok, list_to_integer(Str)};
                I when is_integer(I)  -> {ok, I};
                B when is_binary(B)   -> {ok, binary_to_integer(B)};
                #{type := _, value := V} = TTL -> {ok, TTL#{value => to_int(V)}}
            end
        end,
        "Invalid encoding").

api_cond_decode_fun({Name, {Type, Cond}}, _, Data) ->
    Encoded = maps:get(Name, Data),
    try
        case Cond(Data) of
            true ->
                case aeser_api_encoder:safe_decode(Type, Encoded) of
                    {ok, Val}  -> {ok, Val};
                    {error, _} -> error
                end;
            false ->
                {ok, Encoded}
        end
    catch _:_ ->
        error
    end.

api_decode(Params) ->
    params_read_fun(Params, fun api_decode_read_fun/3, "Invalid hash").

api_decode_read_fun({Name, Types}, _, Data) when is_list(Types) ->
    Encoded = maps:get(Name, Data),
    FoldFun = fun(Type, Acc) ->
                      case aeser_api_encoder:safe_decode(Type, Encoded) of
                          {error, _} -> Acc;
                          {ok, Hash} -> {ok, Hash}
                      end
              end,
    lists:foldl(FoldFun, error, Types);
api_decode_read_fun({Name, {list, Type}}, _, Data) ->
    EncodedList = maps:get(Name, Data),
    lists:foldr( % we are prepending the decoded values, foldr to keep the initial order
        fun(_, error) -> error;
           (Encoded, {ok, Accum}) ->
                case aeser_api_encoder:safe_decode(Type, Encoded) of
                    {error, _} -> error;
                    {ok, Res} -> {ok, [Res | Accum]}
                end
        end,
        {ok, []},
        EncodedList);
api_decode_read_fun({Name, Type}, _, Data) ->
    Encoded = maps:get(Name, Data),
    case aeser_api_encoder:safe_decode(Type, Encoded) of
        {error, _} -> error;
        {ok, _} = Res -> Res
    end.

contract_bytearray_params_decode(ParamNames) ->
    params_read_fun(ParamNames,
        fun(Name, _, State) ->
            ByteArray = maps:get(Name, State),
            case aeser_api_encoder:safe_decode(contract_bytearray, ByteArray) of
                {ok, _} = Ok -> Ok;
                {error, _} -> error
            end
        end,
        "Not byte array").

params_read_fun(ParamNames, ReadFun, ErrMsg) ->
    fun(Req, State0) ->
        {Founds, NotFounds} =
            lists:foldl(
                fun(Key0, {F, NotF}) ->
                    {ReqKey, Name, Props} =
                        case Key0 of
                            {K, N}    -> {K, N, none};
                            {K, N, P} -> {K, N, P};
                            K         -> {K, K, none}
                        end,
                    CallParam =
                        case Props of
                            none -> ReqKey;
                            _    -> {ReqKey, Props}
                        end,
                    case ReadFun(CallParam, Req, State0) of
                        error     -> {F, [ReqKey | NotF]};
                        no_value  -> {F, NotF};
                        {ok, Val} -> {maps:put(Name, Val, F), NotF}
                    end
                end,
                {#{}, []},
                ParamNames),
        case NotFounds =/= [] of
            true ->
                Keys = string:join([atom_to_list(A) || A <- NotFounds], ","),
                Msg = ErrMsg ++ ": " ++ Keys,
                {error, {400, [], #{<<"reason">> => list_to_binary(Msg)}}};
            false ->
                {ok, maps:merge(State0, Founds)}
        end
    end.

get_nonce(AccountKey) ->
    fun(Req, State) ->
        case maps:get(nonce, Req, undefined) of
            undefined ->
                Pubkey = maps:get(AccountKey, State),
                case aec_next_nonce:pick_for_account(Pubkey) of
                    {ok, Nonce} ->
                        {ok, maps:put(nonce, Nonce, State)};
                    {error, account_not_found} ->
                        Msg = "Account of " ++ atom_to_list(AccountKey) ++ " not found",
                        {error, {404, [], #{<<"reason">> => list_to_binary(Msg)}}}
                end;
            Nonce ->
                {ok, maps:put(nonce, to_int(Nonce), State)}
        end
    end.

get_nonce_from_account_id(AccountKey) ->
    fun(Req, State) ->
        case maps:get(nonce, Req, undefined) of
            undefined ->
                Pick = fun (PK) ->
                               case aec_next_nonce:pick_for_account(PK) of
                                   {ok, Nonce} ->
                                       {ok, maps:put(nonce, Nonce, State)};
                                   {error, account_not_found} ->
                                       Msg = "Account of " ++ atom_to_list(AccountKey)
                                           ++ " not found",
                                       {error, {404, [], #{<<"reason">> =>
                                                               list_to_binary(Msg)}}}
                               end
                       end,
                case aeser_id:specialize(maps:get(AccountKey, State)) of
                    {account, A} -> Pick(A);
                    {oracle, O} -> Pick(O);
                    {name, N} ->
                        case aec_chain:resolve_namehash(<<"oracle_pubkey">>, N) of
                            {ok, OracleId} ->
                                Pick(aeser_id:specialize(OracleId, oracle));
                            {error, _} ->
                                Msg = "Could not resolve oracle from name hash",
                                {error, {404, [], #{<<"reason">> => list_to_binary(Msg)}}}
                        end
                end;
            Nonce ->
                {ok, maps:put(nonce, to_int(Nonce), State)}
        end
    end.

print_state() ->
    fun(Req, State) ->
        lager:info("Req: ~p", [Req]),
        lager:info("State: ~p", [State])
    end.

get_contract_code(ContractKey, CodeKey) ->
    fun(_Req, State) ->
        ContractId = maps:get(ContractKey, State),
        ContractPubKey = aeser_id:specialize(ContractId, contract),
        TopBlockHash = aec_chain:top_block_hash(),
        {ok, Trees} = aec_chain:get_block_state(TopBlockHash),
        Tree = aec_trees:contracts(Trees),
        case aect_state_tree:lookup_contract_with_code(ContractPubKey, Tree) of
            none ->
                Msg = "Contract address for key " ++ atom_to_list(ContractKey) ++ " not found",
                {error, {404, [], #{<<"reason">> => list_to_binary(Msg)}}};
            {value, _Contract, Code} ->
                {ok, maps:put(CodeKey, Code, State)}
        end
    end.

get_info_object_from_tx(TxKey, TypeKey, CallKey) ->
    fun(_Req, State) ->
            case maps:get(TxKey, State) of
                #{tx_block_hash := mempool} ->
                    {error, {404, [], #{<<"reason">> => <<"Tx not mined">>}}};
                #{tx_block_hash := BlockHash, tx := STx} ->
                    Tx = aetx_sign:tx(STx),
                    {TxType, _} = aetx:specialize_type(Tx),
                    case get_info_object_signed_tx(BlockHash, STx) of
                        {ok, InfoObject} ->
                            {ok, maps:put(TypeKey, TxType, maps:put(CallKey, InfoObject, State))};
                        {error, transaction_without_info} ->
                            %% Potentialluy one could return
                            %% {ok, maps:put(TypeKey, TxType, maps:put(CallKey, atom_to_binary(TxType, utf8), State))};
                            %% That is not backward compatible, but consistent with inner Txs
                            {error, {400, [], #{<<"reason">> => <<"Tx has no info">>}}};
                        {error, Why} ->
                            Msg = atom_to_binary(Why, utf8),
                            {error, {400, [], #{<<"reason">> => Msg}}}
                    end
            end
    end.

get_info_object_signed_tx(BlockHash, STx) ->
    get_info_object_signed_tx(BlockHash, STx, #{}, STx).

get_info_object_signed_tx(BlockHash, STx, GAIds, OrigTx) ->
    Tx = aetx_sign:tx(STx),
    case aetx:specialize_type(Tx) of
        {TxType, _} when TxType =:= contract_create_tx;
                         TxType =:= contract_call_tx;
                         TxType =:= ga_attach_tx ->
            {CB, CTx} = aetx:specialize_callback(Tx),
            Contract  = CB:contract_pubkey(CTx),
            CallId    = CB:call_id(CTx),
            aec_chain:get_contract_call(Contract, CallId, BlockHash);
        {ga_meta_tx, _} ->
            {CB, CTx} = aetx:specialize_callback(Tx),
            get_ga_meta_tx_info(BlockHash, CB, CTx, GAIds, OrigTx);
        {channel_force_progress_tx, FPTx} ->
            {_Contract, Caller} =
                aesc_force_progress_tx:contract_pubkey_and_caller(FPTx),
            Round = aesc_force_progress_tx:round(FPTx),
            TxHash = aetx_sign:hash(OrigTx),
            TxHashUsedInsteadOfContract = aesc_utils:tx_hash_to_contract_pubkey(TxHash),
            CallId =
                case maps:get(Caller, GAIds, not_found) of
                    not_found -> %% not GA
                        aect_call:id(Caller, Round,
                                     TxHashUsedInsteadOfContract);
                    GAId -> %% had been wrapped in a GA
                        aect_call:ga_id(GAId, TxHashUsedInsteadOfContract)
                end,
            aec_chain:get_contract_call(TxHashUsedInsteadOfContract, CallId, BlockHash);
        {_TxType, _} ->
            {error, transaction_without_info}
    end.


get_ga_meta_tx_info(BlockHash, CB, CTx, GAIds, OrigTx) ->
    Owner     = CB:origin(CTx),
    AuthId    = CB:auth_id(CTx),
    case aec_chain:get_ga_call(Owner, AuthId, BlockHash) of
        {ok, GAObject} ->
            SInnerTx = CB:tx(CTx),
            {InnerTxType, _} = aetx:specialize_type(aetx_sign:tx(SInnerTx)),
            case aega_call:return_type(GAObject) of
                ok ->
                    case get_info_object_signed_tx(BlockHash, SInnerTx,
                                                   GAIds#{Owner => AuthId},
                                                   OrigTx) of
                        {error, _} ->
                            %% Type is all we know from inner transaction
                            {ok, aega_call:set_inner(InnerTxType, GAObject)};
                        {ok, InnerObject} ->
                            {ok, aega_call:set_inner({InnerTxType, InnerObject}, GAObject)}
                    end;
                _ ->
                    {ok, aega_call:set_inner(InnerTxType, GAObject)}
            end;
        Error ->
            Error
    end.


verify_oracle_existence(OracleKey) ->
    fun(_Req, State) ->
            case aeser_id:specialize(maps:get(OracleKey, State)) of
                {oracle, OraclePubkey} ->
                    TopBlockHash = aec_chain:top_block_hash(),
                    {ok, Trees} = aec_chain:get_block_state(TopBlockHash),
                    OTree = aec_trees:oracles(Trees),
                    case aeo_state_tree:lookup_oracle(OraclePubkey, OTree) of
                        {value, _} -> ok;
                        none ->
                            Msg = "Oracle address for key " ++ atom_to_list(OracleKey) ++ " not found",
                            {error, {404, [], #{<<"reason">> => list_to_binary(Msg)}}}
                    end;
                {name, _} ->
                    ok
            end
    end.

verify_oracle_query_existence(OracleKey, QueryKey) ->
    fun(Req, State) ->
        case aeser_id:specialize(maps:get(OracleKey, State)) of
            {oracle, OraclePubKey} ->
                Lookup =
                    fun(QId, Tree) ->
                            aeo_state_tree:lookup_query(OraclePubKey, QId, Tree)
                    end,
                Fun = verify_key_in_state_tree(QueryKey, fun aec_trees:oracles/1,
                                               Lookup, "Oracle query"),
                Fun(Req, State);
            {name, _} ->
                %% We might succeed
                ok
        end
    end.

verify_key_in_state_tree(Key, StateTreeFun, Lookup, Entity) ->
    fun(_Req, State) ->
        ReceivedAddress = maps:get(Key, State),
        TopBlockHash = aec_chain:top_block_hash(),
        {ok, Trees} = aec_chain:get_block_state(TopBlockHash),
        Tree = StateTreeFun(Trees),
        case Lookup(ReceivedAddress, Tree) of
            none ->
                Msg = Entity ++ " for key " ++ atom_to_list(Key) ++ " not found",
                {error, {404, [], #{<<"reason">> => list_to_binary(Msg)}}};
            {value, _} ->
                ok
        end
    end.

verify_name(NameKey) ->
    fun(_Req, State) ->
        Name = maps:get(NameKey, State),
        case aens:get_name_hash(Name) of
            {ok, _} -> ok;
            {error, Reason} ->
                ReasonBin = atom_to_binary(Reason, utf8),
                {error, {400, [],
                        #{reason => <<"Name validation failed with a reason: ",
                                                ReasonBin/binary>>}}}
        end
    end.

nameservice_pointers_decode(PointersKey) ->
    fun(_Req, State) ->
        Pointers = maps:get(PointersKey, State),
        try
            {ok, Pointers1} = decode_pointers(Pointers),
            {ok, maps:put(PointersKey, Pointers1, State)}
        catch
            _:_ -> {error, {400, [], #{<<"reason">> => <<"Invalid pointers">>}}}
        end
    end.

decode_pointers(Pointers) ->
    %% TODO: some request decodes keys to atoms, the other to
    %% binaries. Refactoring needed.
    Pointers1 = lists:map(fun(#{key := K, id := I}) ->
                                  #{<<"key">> => K, <<"id">> => I};
                             (Other) ->
                                  Other
                          end, Pointers),
    decode_pointers(Pointers1, []).

decode_pointers([#{<<"key">> := Key, <<"id">> := IdEnc} | Rest], Acc) ->
    case decode_pointer_id(IdEnc) of
        {ok, Id} -> decode_pointers(Rest, [aens_pointer:new(Key, Id) | Acc]);
        {error, _Rsn} = Error -> Error
    end;
decode_pointers([], Acc) ->
    {ok, Acc}.

decode_pointer_id(IdEnc) ->
    AllowedTypes = [account_pubkey, channel, contract_pubkey, oracle_pubkey],
    aeser_api_encoder:safe_decode({id_hash, AllowedTypes}, IdEnc).

parse_map_to_atom_keys() ->
    fun(Req, State) ->
        Parse =
            fun P(Map) ->
                maps:from_list(
                    lists:map(
                        fun({K0, V0}) ->
                            case safe_binary_to_atom(K0) of
                                {ok, K} ->
                                    V = case is_map(V0) of
                                          true -> P(V0);
                                          false -> V0
                                        end,
                                    {K, V};
                                {error, non_existing} ->
                                    throw({unknown_key, K0})
                            end
                        end,
                        maps:to_list(Map)))
            end,
        try Parse(Req) of
            Req1 ->
              {ok, Req1, State}
        catch
            throw:{unknown_key, K} when is_binary(K) ->
                {error, {400, [], #{<<"reason">> => <<"Invalid parameter: ", K/binary>>}}}
        end
    end.

poi_decode(PoIKey) ->
    fun(_Req, State) ->
        PoIBin = maps:get(PoIKey, State),
        try aec_trees:deserialize_poi(PoIBin) of
            PoI ->
                {ok, maps:put(PoIKey, PoI, State)}
        catch
            _:_ ->
                {error, {400, [], #{<<"reason">> => <<"Invalid proof of inclusion">>}}}
        end
    end.

delegates_decode(DelegatesKey) ->
    fun(_Req, State) ->
        Type = {id_hash, [account_pubkey]},
        Decode =
            fun(Ids) ->
                lists:foldr(
                    fun(_, error) -> error;
                       (EncodedId, Accum) ->
                        case aeser_api_encoder:safe_decode(Type, EncodedId) of
                            {ok, PK} -> [PK | Accum];
                            {error, _} -> error
                        end
                    end,
                    [],
                    Ids)
            end,
        Delegates =
            case maps:get(DelegatesKey, State) of
                L when is_list(L) -> Decode(L);
                #{initiator := IIds, responder := RIds}
                    when is_list(IIds),
                         is_list(RIds) ->
                    case {Decode(IIds), Decode(RIds)} of
                        {error, _} -> error;
                        {_, error} -> error;
                        {_DecodedIIds, _DecodedRIds} = Dels -> Dels
                    end;
                _ -> error
            end,
        case Delegates of
            error ->
                {error, {400, [], #{<<"reason">> => <<"Invalid hash: delegate_ids">>}}};
            _ ->
                {ok, maps:put(DelegatesKey, Delegates, State)}
        end
    end.

ttl_decode(TTLKey) ->
    ttl_decode(TTLKey, [<<"delta">>, <<"block">>]).

relative_ttl_decode(TTLKey) ->
    ttl_decode(TTLKey, [<<"delta">>]).

ttl_decode(TTLKey, AllowedTypes) ->
    fun(Req, State) ->
        TTLObj = maps:get(TTLKey, Req),
        TTLKeyBin = atom_to_binary(TTLKey, utf8),
        ErrorBrokenTTL = {error, {400, [],
                          #{<<"reason">> => <<"Broken TTL for key ",TTLKeyBin/binary>>}}},
        case {maps:get(type, TTLObj, undefined), maps:get(value, TTLObj,
                                                          undefined)} of
            {undefined, _} -> ErrorBrokenTTL;
            {_, undefined} -> ErrorBrokenTTL;
            {Type, Value} ->
                case {lists:member(Type, AllowedTypes), safe_binary_to_atom(Type)} of
                    {true, {ok, TypeAtom}} ->
                        TTL = {TypeAtom, Value},
                        {ok, maps:put(TTLKey, TTL, State)};
                    _ ->
                        TypesBin = list_to_binary(string:join(
                            [binary_to_list(T) || T <- AllowedTypes],
                            ",")),
                        ErrMsg = <<"Invalid TTL type for key ",
                                  TTLKeyBin/binary, "; Allowed: ",
                                  TypesBin/binary>>,
                        {error, {400, [], #{<<"reason">> => ErrMsg}}}
                end
        end
    end.

ok_response(Fun) ->
    fun(Req, State) ->
        Res =
            case Fun of
                _ when is_function(Fun, 1) ->
                    Fun(State);
                _ when is_function(Fun, 2) ->
                    Fun(Req, State)
            end,
        {ok, {200, [], Res}}
    end.

unsigned_tx_response(NewTxFun) when is_function(NewTxFun, 1) ->
    RespFun =
        fun(State) ->
            {ok, Tx} = NewTxFun(State),
            #{tx => aeser_api_encoder:encode(transaction,
                                       aetx:serialize_to_binary(Tx))
            }
        end,
    ok_response(RespFun).

prepare_dry_run_params([], State) -> {ok, State};
prepare_dry_run_params([Param | Params], State) ->
    case prepare_dry_run_param(Param, State) of
        {ok, Value}      -> prepare_dry_run_params(Params, State#{ Param => Value });
        Err = {error, _} -> Err
    end.

do_dry_run() ->
    fun(_Req, State) ->
        case prepare_dry_run_params([top, txs, accounts, tx_events], State) of
            {ok, #{top := Top, accounts := As, txs := Txs, tx_events := Events}} ->
                lager:debug("tx_events = ~p", [Events]),
                case aec_dry_run:dry_run(Top, As, Txs, [{tx_events, Events}]) of
                    {ok, Res} ->
                        {Results, EventRes} = R = dry_run_results(Res),
                        lager:debug("dry_run_results: ~p", [R]),
                        ResultsObj0 = #{ results => Results },
                        ResultsObj = case Events of
                                         true ->
                                             ResultsObj0#{ tx_events => EventRes };
                                         false ->
                                             ResultsObj0
                                     end,
                        {ok, {200, [], ResultsObj}};
                    {error, Reason} -> dry_run_err(Reason)
                end;
            {error, Reason} ->
                dry_run_err(Reason)
        end
    end.

dry_run_err(Err) when is_list(Err) ->
    dry_run_err(list_to_binary(Err));
dry_run_err(Err) ->
    {ok, {403, [], #{ reason => <<"Bad request: ", Err/binary>>}}}.

prepare_dry_run_param(top, #{ top := top }) ->
    case aec_chain:top_block_hash() of
        undefined -> {error, "No top block hash"};
        TopHash   -> {ok, TopHash}
    end;
prepare_dry_run_param(top, #{ top := TopHash }) when is_binary(TopHash) ->
    case aeser_api_encoder:decode(TopHash) of
        {micro_block_hash, MBHash} -> {ok, MBHash};
        {key_block_hash, KBHash}   -> {ok, KBHash};
        _                          -> {error, "top should be micro block hash or key block hash"}
    end;
prepare_dry_run_param(accounts, #{ accounts := Accounts }) ->
    dry_run_accounts_(Accounts, []);
prepare_dry_run_param(txs, #{ txs := Txs }) ->
    dry_run_txs_(Txs, []);
prepare_dry_run_param(tx_events, State) ->
    case maps:get(tx_events, State, false) of
        Events when is_boolean(Events) ->
            {ok, Events};
        _Other ->
            lager:debug("Unexpected: tx_events => ~p", [_Other]),
            {error, "Bad parameter, tx_events"}
    end;
prepare_dry_run_param(Param, _State) ->
    {error, lists:concat(["Bad parameter ", Param])}.

dry_run_accounts_([], Accounts) ->
    {ok, lists:reverse(Accounts)};
dry_run_accounts_([Account | Accounts], Acc) ->
    case Account of
        #{ <<"pub_key">> := EPK, <<"amount">> := Amount } ->
            case aeser_api_encoder:safe_decode(account_pubkey, EPK) of
                {ok, PK} ->
                    dry_run_accounts_(Accounts, [#{ pub_key => PK, amount => to_int(Amount) } | Acc]);
                Err = {error, _Reason} ->
                    Err
            end;
        _ ->
            lager:debug("ACC: ~p", [Account]),
            {error, "Bad account"}
    end.

dry_run_txs_([], Txs) ->
    {ok, lists:reverse(Txs)};
dry_run_txs_([#{ <<"tx">> := ETx } | Txs], Acc) ->
    case aeser_api_encoder:safe_decode(transaction, ETx) of
        {ok, DTx} ->
            Tx = aetx:deserialize_from_binary(DTx),
            {Type, _} = aetx:specialize_type(Tx),
            case not lists:member(Type, [paying_for_tx, offchain_tx, ga_meta_tx]) of
                true  -> dry_run_txs_(Txs, [{tx, Tx} | Acc]);
                false -> {error, lists:concat(["Unsupported transaction type ", Type])}
            end;
        Err = {error, _Reason} ->
            Err
    end;
dry_run_txs_([#{ <<"call_req">> := CallReq } | Txs], Acc) ->
    dry_run_txs_(Txs, [{call_req, CallReq} | Acc]).


dry_run_results({Rs, Events}) ->
    {[dry_run_result(R) || R <- Rs],
     lists:foldr(fun tx_event_result/2, [], Events)}.

dry_run_result({Type, Res}) ->
    dry_run_result(Type, Res, #{ type => type(Type), result => ok_err(Res)}).

dry_run_result(_Type, {error, Reason}, Res) when is_binary(Reason) ->
    Res#{ reason => list_to_binary(lists:concat(["Error: ", Reason])) };
dry_run_result(_Type, {error, Reason}, Res) ->
    Res#{ reason => iolist_to_binary(io_lib:format("Internal error:\n  ~120p\n", [Reason])) };
dry_run_result(Type, {ok, CallObj}, Res) when Type =:= contract_call_tx;
                                              Type =:= contract_create_tx;
                                              Type =:= ga_attach_tx ->
    Res#{ call_obj => aect_call:serialize_for_client(CallObj) };
dry_run_result(_Type, ok, Res) ->
    Res.

tx_event_result({EventKey, EventVal} = E, Acc) ->
    lager:debug("TxEvent ~p", [E]),
    case serialize_event(EventKey, EventVal) of
        error ->
            lager:debug("Couldn't serialize ~p", [E]),
            Acc;
        SerEvent ->
            lager:debug("SerEvent = ~p", [SerEvent]),
            [SerEvent | Acc]
    end.

serialize_event({internal_call_tx, Key}, #{ type    := Type
                                          , tx_hash := TxHash
                                          , info    := Tx } = Evt) ->
    lager:debug("internal_call_tx (~p), Evt = ~p", [Key, Evt]),
    {CB, TxI} = aetx:specialize_callback(Tx),
    TxS = CB:for_client(TxI),
    #{ kind    => internal_call_tx
     , key     => Key
     , type    => Type
     , tx_hash => aeser_api_encoder:encode(tx_hash, TxHash)
     , info    => TxS };
serialize_event({channel, ChId}, #{ type    := Type
                                  , tx_hash := TxHash } = I) ->
    E = #{ kind => channel
         , key  => ChId
         , type => Type
         , tx_hash => TxHash },
    maps:merge(E, maps:with([info], I));
serialize_event(_, _) ->
    error.

ok_err({error, _}) -> <<"error">>;
ok_err(_)          -> <<"ok">>.

type(spend_tx)                  -> <<"spend">>;
type(oracle_register_tx)        -> <<"oracle_register">>;
type(oracle_extend_tx)          -> <<"oracle_extend">>;
type(oracle_query_tx)           -> <<"oracle_query">>;
type(oracle_response_tx)        -> <<"oracle_response">>;
type(name_preclaim_tx)          -> <<"name_preclaim">>;
type(name_claim_tx)             -> <<"name_claim">>;
type(name_transfer_tx)          -> <<"name_transfer">>;
type(name_update_tx)            -> <<"name_update">>;
type(name_revoke_tx)            -> <<"name_revoke">>;
type(contract_call_tx)          -> <<"contract_call">>;
type(contract_create_tx)        -> <<"contract_create">>;
type(ga_attach_tx)              -> <<"ga_attach">>;
type(channel_create_tx)         -> <<"channel_create">>;
type(channel_deposit_tx)        -> <<"channel_deposit">>;
type(channel_withdraw_tx)       -> <<"channel_withdraw">>;
type(channel_force_progress_tx) -> <<"channel_force_progress">>;
type(channel_close_solo_tx)     -> <<"channel_close_solo">>;
type(channel_close_mutual_tx)   -> <<"channel_close_mutual">>;
type(channel_slash_tx)          -> <<"channel_slash">>;
type(channel_settle_tx)         -> <<"channel_settle">>;
type(channel_snapshot_solo_tx)  -> <<"channel_snapshot_solo">>.

get_transaction(TxKey, TxStateKey) ->
    fun(_Req, State) ->
        TxHash = maps:get(TxKey, State),
        case aec_chain:find_tx_with_location(TxHash) of
            none ->
                {error, {404, [], #{<<"reason">> => <<"Transaction not found">>}}};
            {Tag, Tx} ->
                Val = #{tx => Tx, tx_block_hash => Tag},
                {ok, maps:put(TxStateKey, Val, State)}
        end
    end.

encode_transaction(TxKey, EncodedTxKey) ->
    fun(_Req, State) ->
        #{tx := Tx,
          tx_block_hash := BlockHash} = maps:get(TxKey, State),
        T =
            case BlockHash of
                mempool ->
                    aetx_sign:serialize_for_client_pending(Tx);
                _ when is_binary(BlockHash) ->
                    {ok, H} = aec_chain:get_header(BlockHash),
                    aetx_sign:serialize_for_client(H, Tx)
            end,
        {ok, maps:put(EncodedTxKey, #{tx => T}, State)}
    end.

-spec read_optional_param(atom(), map(), term()) -> term().
read_optional_param(Key, Req, Default) ->
    %% swagger does not take into consideration the 'default'
    %% if a query param is missing, swagger adds it to Req with a value of
    %% 'undefined'
    case maps:get(Key, Req, undefined) of
        undefined -> Default;
        Val -> Val
    end.

-spec get_block_from_chain(fun(() -> {ok, aec_blocks:block()} | error | {error, atom()}))
    -> {ok, aec_blocks:block()}| {404, [], map()}.
get_block_from_chain(Fun) when is_function(Fun, 0) ->
    case Fun() of
        {ok, _Block} = OK ->
            OK;
        error ->
            {404, [], #{reason => <<"Block not found">>}};
        {error, no_top_header} ->
            {404, [], #{reason => <<"No top header">>}};
        {error, block_not_found} ->
            {404, [], #{reason => <<"Block not found">>}};
        {error, chain_too_short} ->
            {404, [], #{reason => <<"Chain too short">>}};
        {error, miner_starting} ->
            {404, [], #{reason => <<"Starting mining, pending block not available yet">>}};
        {error, not_mining} ->
            {404, [], #{reason => <<"Not mining, no pending block">>}}
    end.

get_poi(Type, KeyName, PutKey) when Type =:= account
                             orelse Type =:= contract ->
    fun(_Req, State) ->
        PubKey = maps:get(KeyName, State),
        {ok, Trees} = aec_chain:get_top_state(),
        AddToPoI =
            fun(Subtree, PoI0) ->
                case aec_trees:add_poi(Subtree, PubKey, Trees, PoI0) of
                    {ok, PoI} ->
                        {ok, PoI};
                    {error, _} ->
                        SingularName =
                            case Subtree of
                                accounts -> "account";
                                contracts -> "contract"
                            end,
                        Msg = "Proof for " ++ SingularName ++ " not found",
                        {error, list_to_binary(Msg)}
                end
            end,
        SubTrees =
            case Type of
                account -> [accounts];
                contract -> [contracts, accounts]
            end,
        Res =
            lists:foldl(
                fun(_, {error, _} = Err) -> Err;
                   (T, {ok, PoI}) -> AddToPoI(T, PoI)
                end,
                {ok, aec_trees:new_poi(Trees)},
                SubTrees),
        case Res of
            {ok, PoI} ->
                {ok, maps:put(PutKey, PoI, State)};
            {error, Msg} ->
                {error, {404, [], #{<<"reason">> => Msg}}}
        end
    end.

-spec safe_binary_to_atom(atom() | binary()) -> {ok, atom()} | {error, non_existing}.
safe_binary_to_atom(A) when is_atom(A) -> {ok, A};
safe_binary_to_atom(Binary) when is_binary(Binary) ->
    try binary_to_existing_atom(Binary, utf8) of
        Atom -> {ok, Atom}
    catch
        error:badarg -> {error, non_existing}
    end.

get_block_hash_optionally_by_hash_or_height(PutKey) ->
    fun(Req, State) ->
        GetHashByHeight =
            fun(Height) ->
                case aehttp_logic:get_key_header_by_height(Height) of
                    {error, chain_too_short} ->
                        {error, not_found};
                    {ok, Header} ->
                        {ok, _Hash} = aec_headers:hash_header(Header)
                end
            end,
        R =
            case {maps:get('height', Req), maps:get('hash', Req)} of
                {undefined, undefined} ->
                    {ok, _} = aehttp_logic:get_top_hash();
                {undefined, EncodedHash} ->
                    case aeser_api_encoder:safe_decode(block_hash, EncodedHash) of
                        {error, _} ->
                            {error, invalid_hash};
                        {ok, Hash} ->
                            case aec_chain:has_block(Hash) of
                                false ->
                                    {error, not_found};
                                true ->
                                    {ok, Hash}
                            end
                    end;
                {Height, undefined} ->
                    GetHashByHeight(Height);
                {Height, EncodedHash} ->
                    case GetHashByHeight(Height) of
                        {error, _} = Err ->
                            Err;
                        {ok, Hash} -> % ensure it is the same hash
                            case aeser_api_encoder:safe_decode(block_hash, EncodedHash) of
                                {error, _} ->
                                    {error, invalid_hash};
                                {ok, Hash} -> % same hash
                                    {ok, Hash};
                                {ok, _OtherHash} ->
                                    {error, blocks_mismatch}
                            end
                    end
            end,
        case R of
            {error, not_found} ->
                {error, {404, [], #{<<"reason">> => <<"Block not found">>}}};
            {error, Why} ->
                Msg =
                    case Why of
                        invalid_hash -> <<"Invalid block hash">>;
                        blocks_mismatch -> <<"Invalid height and hash combination">>
                    end,
                {error, {400, [], #{<<"reason">> => Msg}}};
          {ok, BlockHash} ->
                {ok, maps:put(PutKey, BlockHash, State)}
        end
    end.

decode_transaction(ParamName) ->
    params_read_fun([ParamName],
        fun(Name, _, State) ->
            Tx = maps:get(Name, State),
            case aeser_api_encoder:safe_decode(transaction, Tx) of
                {error, _} -> error;
                {ok, TxDec} ->
                    try {ok, aetx_sign:deserialize_from_binary(TxDec)}
                    catch 
                        _:_ -> error
                    end
            end
        end,
        "Invalid inner transaction").

safe_get_txs(Block) ->
    case aec_blocks:type(Block) of
        'key' -> [];
        'micro' -> aec_blocks:txs(Block)
    end.

to_int(I) when is_integer(I) -> I;
to_int(Str) when is_list(Str) -> list_to_integer(Str);
to_int(Bin) when is_binary(Bin) -> binary_to_integer(Bin).

