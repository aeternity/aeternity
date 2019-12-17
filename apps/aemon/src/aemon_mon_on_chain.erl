%% @doc This module implements metric worker process which calculates
%% on-chain metrics based on chain height changes.
-module(aemon_mon_on_chain).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , notify/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%% ==================================================================
%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

notify(Height) ->
    gen_server:cast(?MODULE, {gen, Height}).

%% ==================================================================
%% gen_server callbacks

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

%% ==================================================================
%% internal functions

handle_gen(Height, PubKey) ->
    try
        {ok, #{micro_blocks := Blocks}} = aec_chain:get_generation_by_height(Height, forward),
        %% Calculate metrics which require the transaction as input
        [ handle_tx(Height, Tx, aetx_sign:hash(SignTx)) ||
          Block <- Blocks,
          SignTx <- aec_blocks:txs(Block),
          Tx <- [ aetx_sign:tx(SignTx) ],
          aetx:origin(Tx) == PubKey
        ],
        %% Forward chain height info to ttl metric worker
        forward_ttl_gen(Height)
    catch
        error:badarith ->
            ok; %% error on payload decode
        _:Reason ->
            lager:error("~p handle_gen error: ~p", [?MODULE, Reason])
    end.

handle_tx(Height, Tx, TxHash) ->
    %% Forward transaction hash to ttl metric worker
    forward_ttl_tx(TxHash),
    %% Update metric: transaction confirmation delay
    PayloadData = decode_payload(Tx),
    update_confirmation_delay(Height, PayloadData),
    %% Update metric: micro fork counter
    ok = update_forks(Height, PayloadData),
    ok.

forward_ttl_gen(Height) ->
    aemon_mon_ttl:on_chain_height(Height).

forward_ttl_tx(TxHash) ->
    aemon_mon_ttl:on_chain_tx(TxHash).

decode_payload(Tx) ->
    {spend_tx, Spend} = aetx:specialize_type(Tx),
    Payload = aec_spend_tx:payload(Spend),
    [Height, KeyBlock, MicroBlock, Timestamp] = binary:split(Payload, <<":">>, [global]),
    { erlang:binary_to_integer(Height)
    , aeser_api_encoder:decode(KeyBlock)
    , aeser_api_encoder:decode(MicroBlock)
    , erlang:binary_to_integer(Timestamp)
    }.

update_confirmation_delay(GenHeight, {TxHeight, _, _, _}) ->
    ok = aemon_metrics:confirmation_delay(GenHeight - TxHeight).

update_forks(_, {Height, {_, KeyHash}, TopBlock, _}) ->
    {ok, #{key_block := KeyBlock, micro_blocks := MicroBlocks}} = aec_chain:get_generation_by_height(Height, forward),
    {ok, KeyHash} = aec_blocks:hash_internal_representation(KeyBlock),
    case is_micro_fork(TopBlock, MicroBlocks) of
        true ->
            aemon_metrics:fork_micro();
        false ->
            ok
    end.

is_micro_fork({micro_block_hash, MicroHash}, MicroBlocks) ->
    Hashes = lists:map(fun(Block) ->
                               {ok, Hash} = aec_blocks:hash_internal_representation(Block),
                               Hash
                       end, MicroBlocks),
    not lists:member(MicroHash, Hashes);
is_micro_fork({key_block_hash, _}, _) ->
    false.
