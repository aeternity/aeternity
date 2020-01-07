%% @doc This module implements metric worker process which calculates
%% on-chain metrics based on chain height changes.
-module(aemon_mon_on_chain).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , notify/3
        , notify_new_block/3
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(st, { pubkey = <<>> :: binary()
            , micro_blocks = [] :: list()
            }).

%% ==================================================================
%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

notify(Height, Type, Hash) ->
    gen_server:cast(?MODULE, {gen, Height, Type, Hash}).

notify_new_block(Height, Type, Hash) ->
    gen_server:cast(?MODULE, {block, Height, Type, Hash}).

%% ==================================================================
%% gen_server callbacks

init(_) ->
    aemon_metrics:create(on_chain),
    PubKey = aemon_config:pubkey(),
    {ok, #st{pubkey = PubKey}}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast({gen, Height, Type, Hash}, St) ->
    handle_complete_generation(Height, Type, Hash, St#st.pubkey),
    {noreply, St};
handle_cast({block, Height, Type, Hash}, St) ->
    handle_micro_block(Type, Hash),
    St1 = handle_micro_fork(Height, Type, Hash, St),
    {noreply, St1};
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Msg, St) ->
    {noreply, St}.

%% ==================================================================
%% internal functions

handle_complete_generation(Height, key, _, PubKey) ->
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
            lager:error("~p handle_complete_generation error: ~p", [?MODULE, Reason])
    end.

handle_micro_fork(_Height, micro, Hash, #st{micro_blocks = Blocks} = St) ->
    St#st{micro_blocks = [Hash | Blocks]};
handle_micro_fork(Height, key, _Hash, #st{micro_blocks = Blocks} = St) ->
    {ok, #{micro_blocks := BlocksOnChain}} = aec_chain:get_generation_by_height(Height, forward),
    BlocksOnChain1 = lists:map(
                       fun(B) ->
                               {ok, H} = aec_blocks:hash_internal_representation(B),
                               H
                       end, BlocksOnChain),
    %% Check whether we have seen micro-blocks which are not part of the new generation.
    case [B || B <- Blocks, not lists:member(B, BlocksOnChain1)] of
        [] ->
            %% All blocks are in the generation.
            ok;
        BlocksNotOnChain ->
            lager:debug("Monitoring observed micro-fork, missing blocks in generation ~p = ~p",
                        [Height, BlocksNotOnChain]),
            aemon_metrics:fork_micro(length(BlocksNotOnChain))
    end,
    St#st{micro_blocks = []}.

handle_micro_block(key, _Hash) ->
    ok;
handle_micro_block(micro, Hash) ->
    {ok, Block} = aec_chain:get_block(Hash),
    Version = aec_blocks:version(Block),
    Height = aec_blocks:height(Block),
    Txs = aec_blocks:txs(Block),

    %% Update metric: tx per block
    Count = length(Txs),
    aemon_metrics:block_tx_total(Count),

    Gas = lists:foldl(
      fun(Tx, Acc) ->
              Tx1 = aetx_sign:tx(Tx),

              %% Update metric: gas per tx
              GasTx = aetx:gas_limit(Tx1, Height, Version),
              aemon_metrics:block_gas_per_tx(GasTx),

              %% Update metric: size per tx
              SizeTx = aetx:size(Tx1),
              aemon_metrics:block_size_per_tx(SizeTx),

              Acc + GasTx
      end, 0, Txs),

    %% Update metric: gas per block
    aemon_metrics:block_gas_total(Gas),

    ok.

handle_tx(Height, Tx, TxHash) ->
    %% Forward transaction hash to ttl metric worker
    forward_ttl_tx(TxHash),
    %% Update metric: transaction confirmation delay
    PayloadData = decode_payload(Tx),
    update_confirmation_delay(Height, PayloadData),
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
    aemon_metrics:confirmation_delay(GenHeight - TxHeight).
