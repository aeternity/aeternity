-module(ws_int_dispatch).

-export([execute/3]).

-spec execute(Target :: atom(), Action :: atom(), Payload :: list() | map()) ->
                    {ok, Origin :: atom(),
                         Action :: atom(),
                         Payload :: list() | map()} | {error, binary()}.
execute(Target, Action, Payload) ->
    case is_valid(Target, Action, Payload) of
        {false, Reason} ->
            {error, Reason};
        true ->
            case do_execute(Target, Action, Payload) of
                {ok, _, _, _} = OK ->
                    OK;
                {error, _} = Err ->
                    Err
            end
    end.

-spec is_valid(Target :: atom(), Action :: atom(), Payload :: map()) ->
                true | {false, ErrMsg :: binary}.
is_valid(chain, get, #{<<"height">> := Height})
    when not is_integer(Height); Height < 0 ->
    {false, <<"Invalid height">>};
is_valid(_, _, _) ->
    true.

do_execute(chain, get, QueryPayload) ->
    #{<<"type">> := Type} = QueryPayload,
    {BlockFound, Query} =
        case QueryPayload of
            #{<<"height">> := Height} ->
                {aec_conductor:get_block_by_height(Height), {height, Height}};
            #{<<"hash">> := Hash0} ->
                {ok, Hash} = aec_base58c:safe_decode(block_hash, Hash0),
                {aec_conductor:get_block_by_hash(Hash), {hash, Hash0}}
        end,
    case BlockFound of
        {error, ErrMsg} ->
            {error, ErrMsg};
        {ok, Block} ->
            Val0 =
                  case Type of
                      <<"header">> ->
                          {ok, HH} = aec_headers:serialize_to_map(
                              aec_blocks:to_header(Block)),
                          HH;
                      <<"block">> ->
                          aec_blocks:serialize_to_map(Block)
                  end,
            Val = aehttp_dispatch_ext:cleanup_genesis(Val0),
            {ok, chain, requested_data, [{type, Type}, Query, {Type, Val}]}
    end;
do_execute(oracle, subscribe, SubscribeData) ->
    try
        #{<<"type">> := SubType, <<"ws_pid">> := WsPid} = SubscribeData,
        Event =
            case SubType of
                <<"query">> ->
                    #{<<"oracle_id">> := EncodedOId} = SubscribeData,
                    {ok, OId} = aec_base58c:safe_decode(oracle_pubkey, EncodedOId),
                    {query, OId};
                <<"response">> ->
                    #{<<"query_id">> := EncodedQId} = SubscribeData,
                    {ok, QId} = aec_base58c:safe_decode(oracle_query_id, EncodedQId),
                    {response, QId}
            end,
        aec_subscribe:subscribe({ws, WsPid}, {aeo, Event}),
        {ok, oracle, subscribe, [{result, ok}, {subscribed_to, maps:remove(<<"ws_pid">>, SubscribeData)}]}
    catch _:_ ->
        {error, <<"Bad subscription request">>}
    end;
do_execute(oracle, register, RegisterData) ->
    try
        case aehttp_dispatch_int:handle_request('PostOracleRegisterTx',
                                                #{'OracleRegisterTx' => RegisterData},
                                                #{}) of
            {200, _, #{oracle_id := OId}} ->
                {ok, oracle, register, [{result, ok}, {oracle_id, OId}]};
            {_, _, #{reason := Reason}} ->
                {error, Reason}
        end
    catch _:_ ->
        {error, <<"Bad Oracle register request">>}
    end;
do_execute(oracle, query, QueryData) ->
    try
        case aehttp_dispatch_int:handle_request('PostOracleQueryTx',
                                                #{'OracleQueryTx' => QueryData},
                                                #{}) of
            {200, _, #{query_id := QId}} ->
                {ok, oracle, query, [{result, ok}, {query_id, QId}]};
            {_, _, #{reason := Reason}} ->
                {error, Reason}
        end
    catch _:_ ->
        {error, <<"Bad Oracle query request">>}
    end;
do_execute(oracle, response, ResponseData) ->
    try
        case aehttp_dispatch_int:handle_request('PostOracleResponseTx',
                                                #{'OracleResponseTx' => ResponseData},
                                                #{}) of
            {200, _, #{query_id := QId}} ->
                {ok, oracle, response, [{result, ok}, {query_id, QId}]};
            {_, _, #{reason := Reason}} ->
                {error, Reason}
        end
    catch _:_ ->
        {error, <<"Bad Oracle response request">>}
    end;
do_execute(_, _, _) -> % a catch all for a prettier error when action is missing
    {error, <<"Missing action">>}.
