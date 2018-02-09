-module(aehttp_helpers).

-export([ parse_request/2
        , read_required_params/1
        , base58_decode/1
        , hexstrings_decode/1
        , get_nonce/1
        , print_state/0
        , verify_contract_existance/1
        ]).

parse_request(FunsList, Req) ->
    parse_request(FunsList, Req, #{}).

parse_request([], _Req, Result) ->
    {ok, Result};
parse_request([Fun | T], Req, Result0) ->
    case Fun(Req, Result0) of
        ok ->
            parse_request(T, Req, Result0);
        {ok, Result} ->
            parse_request(T, Req, Result);
        {error, _ErrMsg} = Err ->
            Err
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

base58_decode(Params) ->
    params_read_fun(Params,
        fun({Name, Type}, _, Data) ->
            Encoded = maps:get(Name, Data),
            case aec_base58c:safe_decode(Type, Encoded) of
                {error, _} ->
                    error;
                {ok, Hash} ->
                    {ok, Hash}
            end
        end,
        "Invalid hash").

hexstrings_decode(ParamNames) ->
    params_read_fun(ParamNames,
        fun(Name, _, State) ->
            Hex = maps:get(Name, State),
            try {ok, aect_ring:hexstring_decode(Hex)}
            catch _:_ -> error
            end
        end,
        "Not hex string").

params_read_fun(ParamNames, ReadFun, ErrMsg) ->
    fun(Req, State0) ->
        {Founds, NotFounds} =
            lists:foldl(
                fun(Key0, {F, NotF}) -> 
                    {ReqKey, Name, Props} = 
                        case Key0 of
                            {K, N} -> {K, N, none};
                            {K, N, P} -> {K, N, P};
                            K -> {K, K, none}
                        end,
                    CallParam =
                        case Props of
                            none -> ReqKey;
                            _ -> {ReqKey, Props}
                        end,
                    case ReadFun(CallParam, Req, State0) of
                        error -> {F, [ReqKey | NotF]};
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
        case maps:get(nonce, Req) of
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
                {ok, maps:put(nonce, Nonce, State)}
        end
    end.

print_state() ->
    fun(Req, State) ->
        lager:info("Req: ~p", [Req]),
        lager:info("State: ~p", [State])
    end.

verify_contract_existance(ContractKey) ->
    fun(_Req, State) ->
        ContractAddress = maps:get(ContractKey, State),
        TopBlockHash = aec_conductor:top_block_hash(),
        {value, Trees} = aec_db:find_block_state(TopBlockHash),
        ContractsTree = aec_trees:contracts(Trees),
        case aect_state_tree:lookup_contract(ContractAddress, ContractsTree) of
            none ->
                Msg = "Contract address for key " ++ atom_to_list(ContractKey) ++ " not found",
                {error, {404, [], #{<<"reason">> => list_to_binary(Msg)}}};
            {value, _} ->
                ok
        end
    end.
