-module(aehc_connector).

-export([send_tx/1, get_block/1]).

-export([tx/3, header/2, block/2]).

-export([publish/2, subscribe/1]).

-type connector() :: atom().

-callback send_tx(Tx::aetx:tx()) -> binary().

-callback get_block(Num::integer()) -> block().

-export_type([tx/0, header/0, block/0]).

-spec connector() -> connector().
connector() ->
    ok.

%%%===================================================================
%%%  Parent chain simplified proto
%%%===================================================================

-record(tx, {
             sender :: binary(),
             recipient :: binary(),
             payload :: binary()
            }).

-type tx() :: #tx{}.

-record(header, {
                 hash :: binary(),
                 number = 0 :: integer()
                }).

-type header() :: #header{}.

-record(block, {
                header :: header(),
                txs :: [tx()]
               }).

-type block() :: #block{}.

-spec tx(Sender::binary(), Recipient::binary(), Payload::binary()) -> tx().
tx(Sender, Recipient, Payload) when 
      is_binary(Sender), is_binary(Recipient), is_binary(Payload) ->
    #tx{ sender=Sender, recipient=Recipient, payload=Payload }.

-spec header(Hash::binary(), Num::integer()) -> header().
header(Hash, Num) when 
      is_binary(Hash), is_integer(Num) ->
    #header{ hash=Hash, number=Num }.

-spec block(Header::header(), Txs::[tx()]) -> block().
block(Header, Txs) when
      is_record(Header, header), is_list(Txs) ->
    #block{ header=Header, txs=Txs }.

%%%===================================================================
%%%  Parent chain interface
%%%===================================================================

-spec send_tx(Tx::aetx:tx()) ->
                    {ok, TxHash::binary()} | {error, {term(), term()}}.
send_tx(Tx) ->
    Con = aehc_chain_sim_connector, %% TODO To ask via config;
    try
        Res = Con:send_tx(Tx), true = is_binary(Res),
        {ok, Res}
    catch E:R ->
            {error, {E, R}}
    end.

-spec get_block(Num::integer()) ->
                       {ok, block()} | {error, {term(), term()}}.
get_block(Num) ->
    Con = aehc_chain_sim_connector, %% TODO To ask via config;
    try 
        Res = Con:get_block(Num), true = is_record(Res, block),
        {ok, Res}
    catch E:R ->
            {error, {E, R}}
    end.

%%%===================================================================
%%%  Parent chain events
%%%===================================================================

-spec subscribe(aec_events:event()) -> true.
subscribe(Event) ->
    aec_events:subscribe(Event).

-spec publish(aec_events:event(), any()) -> ok.
publish(Event, Info) ->
    aec_events:publish(Event, Info).
