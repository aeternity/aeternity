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
                {aec_chain:get_key_block_by_height(Height), {height, Height}};
            #{<<"hash">> := Hash0} ->
                Hash = decode_block_hash(Hash0),
                {aec_chain:get_block(Hash), {hash, Hash0}}
        end,
    case BlockFound of
        error -> {error, block_not_found};
        {error, ErrMsg} ->
            {error, ErrMsg};
        {ok, Block} ->
            Header = aec_blocks:to_header(Block),
            Val =
                case aec_blocks:height(Block) of
                    0 ->
                        aec_headers:serialize_for_client(Header, key);
                    _ ->
                        PrevBlockHash = aec_blocks:prev_hash(Block),
                        case aec_chain:get_block(PrevBlockHash) of
                            {ok, PrevBlock} ->
                                PrevBlockType = aec_blocks:type(PrevBlock),
                                aec_headers:serialize_for_client(Header, PrevBlockType);
                            error ->
                                #{reason => <<"Block not found">>}
                        end
                end,
            {ok, chain, requested_data, [{type, Type}, Query, {Type, Val}]}
    end;
do_execute(chain, unsubscribe_all, Data) ->
    #{<<"ws_pid">> := WsPid} = Data,
    aec_subscribe:unsubscribe_all(WsPid),
    {ok, chain, unsubscribe_all, [{result, ok}]};
do_execute(chain, SubUnSub, SubscribeData)
        when SubUnSub == subscribe; SubUnSub == unsubscribe ->
    try
        #{<<"type">> := SubType, <<"ws_pid">> := WsPid} = SubscribeData,
        Event =
            case SubType of
                <<"mined_block">> ->
                    {chain, mined_block};
                <<"added_micro_block">> ->
                    {chain, added_micro_block};
                <<"new_block">> ->
                    {chain, new_block};
                <<"tx">> ->
                    #{<<"tx_hash">> := EncodedTxHash} = SubscribeData,
                    {ok, TxHash} = aec_base58c:safe_decode(tx_hash, EncodedTxHash),
                    {chain_tx, {tx, TxHash}}
            end,
        case SubUnSub of
            subscribe   -> aec_subscribe:subscribe({ws, WsPid}, Event);
            unsubscribe -> aec_subscribe:unsubscribe({ws, WsPid}, Event)
        end,
        {ok, chain, SubUnSub, [{result, ok}, {subscribed_to, maps:remove(<<"ws_pid">>, SubscribeData)}]}
    catch _:_ ->
        {error, <<"Bad subscription request">>}
    end;
do_execute(_, _, _) -> % a catch all for a prettier error when action is missing
    {error, <<"Missing action">>}.

%% TODO: add a function in base58c to find out what type the hash is?
decode_block_hash(Hash) ->
    case aec_base58c:safe_decode(key_block_hash, Hash) of
        {ok, KeyBlockHash} ->
            KeyBlockHash;
        {error, _Rsn} ->
            case aec_base58c:safe_decode(micro_block_hash, Hash) of
                {ok, MicroBlockHash} ->
                    MicroBlockHash;
                {error, _Rsn} = Err ->
                    Err
            end
    end.
