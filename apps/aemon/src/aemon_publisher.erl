-module(aemon_publisher).
-behaviour(gen_server).

-export([start/0]).
-export([single_tx/0]).

-export([start_link/0]).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

start() ->
    ?MODULE ! start.

single_tx() ->
    ?MODULE ! single_tx.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callback

init(_) ->
    [ self() ! start || aemon_config:autostart() ],
    {ok, []}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(start, St) ->
    aemon_metrics:create(publisher),
    self() ! post_tx,
    {noreply, St};
handle_info(post_tx, St) ->
    balance(),
    post_tx(),
    timer:send_after(aemon_config:interval(), post_tx),
    {noreply, St};
handle_info(single_tx, St) ->
    post_tx(),
    {noreply, St};
handle_info(_Msg, St) ->
    {noreply, St}.

%% internal callback

balance() ->
    PubKey = pubkey(),
    case aec_chain:get_account(PubKey) of
        {value, Account} ->
            Balance = aec_accounts:balance(Account),
            aemon_metrics:publisher_balance(Balance);
        _ ->
            aemon_metrics:publisher_balance(not_found)
    end.

post_tx() ->
    Timestamp = os:system_time(seconds),
    {Height, Protocol, HashKey, HashTop} = top_info(),
    Payload = to_payload(Height, HashKey, HashTop, Timestamp),

    Nonce = nonce(),
    Tx = create_tx(Height, Protocol, Nonce, Payload),
    post_tx(Height, Protocol, Nonce, Payload, Tx).

post_tx(Height, Protocol, Nonce, Payload, Tx) ->
    post_tx(Height, Protocol, Nonce, Payload, Tx, 5).

post_tx(_, _, _, _, _, 0) ->
    aemon_metrics:publisher_post_tx(max_adjustment);
post_tx(Height, Protocol,  Nonce, Payload, Tx, Adjustment) ->
    SignTx = sign_tx(Tx),
    case aec_tx_pool:push(SignTx) of
        {error, too_low_fee} ->
            NewTx = adjust_tx(Height, Protocol, Nonce, Payload, Tx),
            post_tx(Height, Protocol, Nonce, Payload, NewTx, Adjustment-1);
        {error, too_low_gas_price_for_miner} ->
            NewTx = adjust_tx(Height, Protocol, Nonce, Payload, Tx),
            post_tx(Height, Protocol, Nonce, Payload, NewTx, Adjustment-1);
        {error, Error} ->
            aemon_metrics:publisher_post_tx(Error);
        ok ->
            aemon_metrics:publisher_post_tx(success),
            aemon_mon_ttl:publisher_tx(Height, SignTx)
    end.

%% internal helpers

nonce() ->
    case aec_next_nonce:pick_for_account(pubkey()) of
        {error, _} -> 0;
        {ok, Nonce} -> Nonce
    end.

top_info() ->
    TopBlock = aec_chain:top_block(),
    {ok, TopKeyBlock} = aec_chain:top_key_block(),

    {aec_blocks:height(TopBlock),
     aec_blocks:version(TopBlock),
     to_hash(TopKeyBlock),
     to_hash(TopBlock)}.

to_hash(Block) ->
    {ok, Hash} = aec_blocks:hash_internal_representation(Block),
    EncType = case aec_blocks:type(Block) of
                  micro -> micro_block_hash;
                  key -> key_block_hash
              end,
    aeser_api_encoder:encode(EncType, Hash).

to_payload(Height, Key, Top, Timestamp) ->
    Format = io_lib:format("~p:~s:~s:~p", [Height, Key, Top, Timestamp]),
    iolist_to_binary(Format).

create_tx(Height, Protocol, Nonce, Payload) ->
    Tx = raw_tx(Height, Nonce, Payload, 1),
    adjust_tx(Height, Protocol, Nonce, Payload, Tx).

adjust_tx(Height, Protocol, Nonce, Payload, Tx0) ->
    GasPrice = aec_tx_pool:minimum_miner_gas_price(),
    GasLimit = aetx:gas_limit(Tx0, Height, Protocol),
    MinFee = aetx:min_fee(Tx0, Height, Protocol),
    MinGasFee = GasPrice * GasLimit,

    Fee = lists:max([MinFee, MinGasFee]),
    raw_tx(Height, Nonce, Payload, Fee).

raw_tx(Height, Nonce, Payload, Fee) ->
    Account = aeser_id:create(account, pubkey()),
    {ok, Tx} = aec_spend_tx:new(
                 #{ sender_id    => Account,
                    recipient_id => Account,
                    amount       => aemon_config:amount(),
                    nonce        => Nonce,
                    ttl          => Height + aemon_config:ttl(),
                    payload      => Payload,
                    fee          => Fee}
                ),
    Tx.

sign_tx(Tx) ->
    PrivKey = privkey(),
    Network = aec_governance:add_network_id( aetx:serialize_to_binary(Tx) ),
    aetx_sign:new(Tx, [enacl:sign_detached(Network, PrivKey)]).

%% config

pubkey() ->
    aemon_config:pubkey().

privkey() ->
    aemon_config:privkey().
