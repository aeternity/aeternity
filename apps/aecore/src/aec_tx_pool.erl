%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Memory pool of unconfirmed transactions.
%%%
%%% Unconfirmed transactions are transactions not included in any
%%% block in the longest chain.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aec_tx_pool).

-behaviour(gen_server).

-include("common.hrl").

-define(MEMPOOL, mempool).
-define(KEY_NONCE_PATTERN(Sender), {{'_', Sender, '$1'}, '_'}).

%% API
-export([start_link/0,
         stop/0]).
-export([push/1, push/2,
         peek/1,
         top_change/2,
         get_max_nonce/1,
         size/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-record(state, {db :: pool_db()}).

-type negated_fee() :: non_pos_integer().
-type non_pos_integer() :: neg_integer() | 0.

-type pool_db_key() ::
        {negated_fee(), pubkey(), non_neg_integer()} | undefined.
-type pool_db_value() :: aetx_sign:signed_tx().
-type pool_db() :: atom().

-type event() :: tx_created | tx_received.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    %% implies also clearing the mempool
    gen_server:stop(?SERVER).

-define(PUSH_EVENT(E), Event =:= tx_created; Event =:= tx_received).

%% INFO: Transaction from the same sender with the same nonce and fee
%%       will be overwritten
-spec push(aetx_sign:signed_tx()) -> ok.
push(Tx) ->
    push(Tx, tx_created).

-spec push(aetx_sign:signed_tx(), event()) -> ok.
push(Tx, Event) when ?PUSH_EVENT(Event) ->
    %% Verify that this is a signed transaction.
    try aetx_sign:tx(Tx)
    catch _:_ -> error({illegal_transaction, Tx})
    end,
    gen_server:call(?SERVER, {push, Tx, Event}).

-spec get_max_nonce(pubkey()) -> {ok, non_neg_integer()} | undefined.
get_max_nonce(Sender) ->
    gen_server:call(?SERVER, {get_max_nonce, Sender}).

%% The specified maximum number of transactions avoids requiring
%% building in memory the complete list of all transactions in the
%% pool.
-spec peek(pos_integer() | infinity) -> {ok, [aetx_sign:signed_tx()]}.
peek(MaxN) when is_integer(MaxN), MaxN >= 0; MaxN =:= infinity ->
    gen_server:call(?SERVER, {peek, MaxN}).

-spec top_change(binary(), binary()) -> ok.
top_change(OldHash, NewHash) ->
    gen_server:call(?SERVER, {top_change, OldHash, NewHash}).

-spec size() -> non_neg_integer() | undefined.
size() ->
    ets:info(?MEMPOOL, size).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, Db} = pool_db_open(),
    Handled  = ets:new(init_tx_pool, [private]),
    InitF  = fun(TxHash, _) ->
                     update_pool_on_tx_hash(TxHash, Db, Handled),
                     ok
             end,
    ok = aec_db:ensure_transaction(fun() -> aec_db:fold_mempool(InitF, ok) end),
    {ok, #state{db = Db}}.

handle_call({get_max_nonce, Sender}, _From, #state{db = Mempool} = State) ->
    {reply, int_get_max_nonce(Mempool, Sender), State};
handle_call({push, Tx, Event}, _From, #state{db = Mempool} = State) ->
    {reply, pool_db_put(Mempool, pool_db_key(Tx), Tx, Event), State};
handle_call({top_change, OldHash, NewHash}, _From,
            #state{db = Mempool} = State) ->
    do_top_change(OldHash, NewHash, Mempool),
    {reply, ok, State};
handle_call({peek, MaxNumberOfTxs}, _From, #state{db = Mempool} = State)
  when is_integer(MaxNumberOfTxs), MaxNumberOfTxs >= 0;
       MaxNumberOfTxs =:= infinity ->
    Txs = pool_db_peek(Mempool, MaxNumberOfTxs),
    {reply, {ok, Txs}, State};
handle_call(Request, From, State) ->
    lager:warning("Ignoring unknown call request from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

handle_info({'ETS-TRANSFER', _, _, _}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    lager:warning("Ignoring unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec int_get_max_nonce(pool_db(), pubkey()) -> {ok, non_neg_integer()} | undefined.
int_get_max_nonce(Mempool, Sender) ->
    case lists:flatten(ets:match(Mempool, ?KEY_NONCE_PATTERN(Sender))) of
        [] ->
            undefined;
        Nonces ->
            MaxNonce = lists:max(Nonces),
            {ok, MaxNonce}
    end.


pool_db_key(SignedTx) ->
    Tx = aetx_sign:tx(SignedTx),
    %% INFO: Sort by fee
    %%       TODO: sort by fee, then by origin, then by nonce

    %% INFO: * given that nonce is an index of transactions for a user,
    %%         the following key is unique for a transaction
    %%       * negative fee places high profit transactions at the beginning
    %%       * ordered_set type enables implicit overwrite of the same txs
    exclude_coinbase({-aetx:fee(Tx), aetx:origin(Tx), aetx:nonce(Tx)}).

exclude_coinbase({_, undefined, undefined}) ->
    undefined; %% Identify coinbase
exclude_coinbase({Fee, Origin, Nonce}) ->
    {Fee, Origin, Nonce}.

-spec pool_db_open() -> {ok, pool_db()}.
pool_db_open() ->
    {ok, ets:new(?MEMPOOL, [ordered_set, public, named_table])}.

-spec pool_db_peek(pool_db(), MaxNumber::pos_integer() | infinity) ->
                          [pool_db_value()].
pool_db_peek(_, 0) -> [];
pool_db_peek(Mempool, Max) ->
    sel_return(
      ets_select(Mempool, [{ {'_', '$1'}, [], ['$1'] }], Max)).

ets_select(T, P, infinity) ->
    ets:select(T, P);
ets_select(T, P, N) when is_integer(N), N >= 1 ->
    ets:select(T, P, N).

sel_return(L) when is_list(L) -> L;
sel_return('$end_of_table' ) -> [];
sel_return({Matches, _Cont}) -> Matches.

do_top_change(OldHash, NewHash, Mempool) ->
    %% Add back transactions to the pool from discarded part of the chain
    %% Mind that we don't need to add those which are incoming in the fork
    {ok, Ancestor} = aec_chain:find_common_ancestor(OldHash, NewHash),
    Handled = ets:new(foo, [private, set]),
    update_pool_from_blocks(Ancestor, OldHash, Mempool, Handled),
    update_pool_from_blocks(Ancestor, NewHash, Mempool, Handled),
    ets:delete(Handled),
    ok.

update_pool_from_blocks(Hash, Hash,_Mempool,_Handled) -> ok;
update_pool_from_blocks(Ancestor, Current, Mempool, Handled) ->
    lists:foreach(fun(TxHash) ->
                          update_pool_on_tx_hash(TxHash, Mempool, Handled)
                  end,
                  aec_db:get_block_tx_hashes(Current)),
    Prev = aec_chain:prev_hash_from_hash(Current),
    update_pool_from_blocks(Ancestor, Prev, Mempool, Handled).

update_pool_on_tx_hash(TxHash, Mempool, Handled) ->
    case ets:member(Handled, TxHash) of
        true -> ok;
        false ->
            ets:insert(Handled, {TxHash}),
            Tx = aec_db:get_signed_tx(TxHash),
            case aec_db:is_in_tx_pool(TxHash) of
                false ->
                    %% Added to chain
                    ets:delete(Mempool, pool_db_key(Tx));
                true ->
                    case pool_db_key(Tx) of
                        undefined ->
                            aec_db:remove_tx_from_mempool(TxHash);
                        Key ->
                            ets:insert(Mempool, {Key, Tx})
                    end
            end
    end.

-spec pool_db_put(pool_db(), pool_db_key(), aetx_sign:signed_tx(), event()) ->
                         'ok' | {'error', atom()}.
pool_db_put(_, undefined, _, _) ->
    %% TODO: This is probably not needed anymore
    {error, coinbase};
pool_db_put(Mempool, Key, Tx, Event) ->
    Hash = aetx_sign:hash(Tx),
    case aec_db:find_tx_location(Hash) of
        BlockHash when is_binary(BlockHash) ->
            lager:debug("Already have tx: ~p in ~p", [Hash, BlockHash]),
            {error, already_accepted};
        mempool ->
            lager:debug("Already have tx: ~p in ~p", [Hash, mempool]),
            ok;
        none ->
            case aetx_sign:verify(Tx) of
                {error, _} = E ->
                    lager:info("Failed signature check on tx: ~p, ~p\n", [E, Hash]),
                    E;
                ok ->
                    Unsigned = aetx_sign:tx(Tx),
                    Nonce = aetx:nonce(Unsigned),
                    case check_nonce(aetx:origin(Unsigned), Nonce) of
                        {error, _} = E -> E;
                        ok ->
                            lager:debug("Adding tx", [Hash]),
                            case ets:member(Mempool, Key) of
                                true ->
                                    lager:debug("Pool db key already present (~p)", [Key]),
                                    %% TODO: We should make a decision whether to switch the tx.
                                    ok;
                                false ->
                                    aec_events:publish(Event, Tx),
                                    aec_db:add_tx(Tx),
                                    ets:insert(Mempool, {Key, Tx}),
                                    ok
                            end
                    end
            end
    end.

check_nonce(Pubkey, TxNonce) ->
    %% Check is conservative and only rejects certain cases
    case aec_chain:get_account(Pubkey) of
        {value, Account} ->
            case aec_accounts:nonce(Account) < TxNonce of
                true  -> ok;
                false -> {error, too_low_nonce}
            end;
        {error, no_state_trees} ->
            ok;
        none ->
            ok
    end.
