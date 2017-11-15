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
is_valid(chain, get, #{<<"height">> := Height}) when Height < 0->
    {false, <<"Invalid height">>};
is_valid(_, _, _) ->
    true.

do_execute(chain, get, QueryPayload) ->
    #{<<"type">> := Type} = QueryPayload,
    {BlockFound, Query} =
        case QueryPayload of
            #{<<"height">> := Height} ->
                {aec_chain:get_block_by_height(Height), {height, Height}};
            #{<<"hash">> := Hash0} ->
                Hash = base64:decode(Hash0),
                {aec_chain:get_block_by_hash(Hash), {hash, Hash0}}
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
do_execute(_, _, _) -> % a catch all for a prettier error when action is missing
    {error, <<"Missing action">>}.
