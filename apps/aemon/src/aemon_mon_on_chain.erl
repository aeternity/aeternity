-module(aemon_mon_on_chain).
-behaviour(gen_server).

-export([notify/1]).
-export([start_link/0]).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

notify(Height) ->
    gen_server:cast(?MODULE, {gen, Height}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callback

init(_) ->
    aemon_metrics:create(on_chain),
    PubKey = aemon_config:pubkey(),
    {ok, PubKey}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast({gen, Height}, St) ->
    handle_gen(Height, St),
    {noreply, St};
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Msg, St) ->
    {noreply, St}.

%% internals

handle_gen(Height, PubKey) ->
    try
        {ok, #{micro_blocks := Blocks}} = aec_chain:get_generation_by_height(Height, forward),
        [ handle_tx(Height, Tx, aetx_sign:hash(SignTx)) ||
          Block <- Blocks,
          SignTx <- aec_blocks:txs(Block),
          Tx <- [ aetx_sign:tx(SignTx) ],
          aetx:origin(Tx) == PubKey
        ],
        ttl_gen(Height)
    catch
        error:badarith -> ok; %% error on payload decode
        _:Reason ->
            lager:error("~p handle_gen error: ~p", [?MODULE, Reason])
    end.

handle_tx(Height, Tx, TxHash) ->
    ttl_tx(TxHash),
    PayloadData = decode_payload(Tx),
    confirmation_delay(Height, PayloadData),
    forks(Height, PayloadData).

ttl_gen(Height) ->
    aemon_mon_ttl:on_chain_height(Height).

ttl_tx(TxHash) ->
    aemon_mon_ttl:on_chain_tx(TxHash).

decode_payload(Tx) ->
    {spend_tx, Spend} = aetx:specialize_type(Tx),
    Payload = aec_spend_tx:payload(Spend),
    [Height, KeyBlock, MicroBlock, Timestamp] = binary:split(Payload, <<":">>, [global]),
    {erlang:binary_to_integer(Height),
     aeser_api_encoder:decode(KeyBlock),
     aeser_api_encoder:decode(MicroBlock),
     erlang:binary_to_integer(Timestamp)
    }.

confirmation_delay(GenHeight, {TxHeight, _, _, _}) ->
    aemon_metrics:confirmation_delay(GenHeight - TxHeight).

forks(_, {Height, {_, KeyHash}, TopBlock, _}) ->
    {ok, #{key_block := KeyBlock, micro_blocks := MicroBlocks}} = aec_chain:get_generation_by_height(Height, forward),
    {ok, KeyHash} = aec_blocks:hash_internal_representation(KeyBlock),
    fork_micro(TopBlock, MicroBlocks).

fork_micro({micro_block_hash, MicroHash}, MicroBlocks) ->
    HashList = lists:map(fun(Block) ->
            {ok, Hash} = aec_blocks:hash_internal_representation(Block),
            Hash
        end, MicroBlocks),
    case lists:member(MicroHash, HashList) of
        false -> aemon_metrics:fork_micro();
        true -> ok
    end;
fork_micro({key_block_hash, _}, _) -> ok.
