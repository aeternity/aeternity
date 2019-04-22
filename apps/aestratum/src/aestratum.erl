-module(aestratum).

-behaviour(gen_server).

%% API
-export([start_link/0,
         submit_share/3,
         submit_solution/3]).

-export([sent_payments/0,
         pending_payments/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-include("aestratum.hrl").
-include("aestratum_log.hrl").
-include_lib("aecontract/src/aecontract.hrl").
-include_lib("aecore/include/blocks.hrl").
-include_lib("aecore/src/aec_conductor.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-import(aestratum_fn, [is_ok/1, ok/0, ok/1, ok/2, ok_err/2, ok_val_err/1, ok_val_err/2,
                       lazy_hd/3, tag_val_err/3, const/1]).

-define(KEYBLOCK_ROUNDS_DELAY, 180).

-define(CHAIN_TOP_CHECK_INTERVAL, 1000). % 1 second
-define(CHAIN_PAYMENT_TX_CHECK_INTERVAL, 10000). % 10 seconds

-define(PAYMENT_CONTRACT_BATCH_SIZE, 100).
-define(PAYMENT_CONTRACT_MIN_FEE_MULTIPLIER, 2.0).
-define(PAYMENT_CONTRACT_MIN_GAS_MULTIPLIER, 1.3).
-define(PAYMENT_CONTRACT_INIT_GAS, 250). % measured
%-define(PAYMENT_CONTRACT_INIT_GAS, 10000). %
-define(PAYMENT_CONTRACT_GAS_PER_TRANSFER, 22000). % measured
%-define(PAYMENT_CONTRACT_GAS_PER_TRANSFER, 500000). %

%%%===================================================================
%%% STATE
%%%===================================================================

-record(reward_key_block,
        {share_key :: aestratum_db:sort_key(),
         height    :: non_neg_integer(),
         target    :: non_neg_integer(),
         tokens    :: aestratum_db:amount(),
         hash      :: binary()}). %% reward block hash

-record(aestratum_state,
        {chain_keyblock_hash,
         pending_tx,
         pending_tx_hash}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

submit_share(<<"ak_", _/binary>> = Miner, MinerTarget, <<Hash/binary>>) ->
    true = is_integer(MinerTarget) andalso MinerTarget >= 0,
    gen_server:cast(?MODULE, {submit_share, Miner, MinerTarget, Hash}).

submit_solution(BlockHash, MinerNonce, Pow) ->
    gen_server:cast(?MODULE, {submit_solution, BlockHash, MinerNonce, Pow}).

sent_payments() ->
    ?TXN(select_payments(true)).

pending_payments() ->
    ?TXN(select_payments(false)).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    PoolRelShares = relative_shares(?POOL_PERCENT_SHARES, ?POOL_PERCENT_SUM),
    aestratum_util:set_env(#{pool_relative_shares => PoolRelShares}),
    ?TXN(aestratum_db:is_empty(?ROUNDS_TAB) andalso aestratum_db:store_round()),
    aec_events:subscribe(stratum_new_candidate),
    {ok, _} = timer:send_interval(?CHAIN_TOP_CHECK_INTERVAL, chain_top_check),
    {ok, #aestratum_state{chain_keyblock_hash = aec_chain:top_key_block_hash()}}.

handle_info(chain_top_check, #aestratum_state{chain_keyblock_hash = LastKB} = S) ->
    NewKB = aec_chain:top_key_block_hash(),
    NewKB == LastKB orelse (self() ! keyblock),
    {noreply, S#aestratum_state{chain_keyblock_hash = NewKB}};

handle_info(keyblock, #aestratum_state{chain_keyblock_hash = <<_/binary>>} = S) ->
    ?TXN(aestratum_db:store_round()),
    case ?TXN(maybe_compute_reward()) of
        {ok, not_our_share} ->
            ok;
        {ok, #aestratum_reward{} = R} ->
            ?TXN(store_payments(R)),
            self() ! payout_check;
        {error, Reason} ->
            ?ERROR("reward computation failed: ~p", [Reason])
    end,
    {noreply, S};

handle_info(payout_check, S) ->
    {noreply,
     case ?TXN(has_payments(true)) of
         true  -> S;
         false -> case ?TXN(oldest_unpaid_payment()) of
                      {ok, P} -> push_payment(P, S);
                      none -> S
                  end
     end};

%% TODO: store candidate to mnesia here
handle_info({gproc_ps_event, stratum_new_candidate,
             #{info := [{HeaderBin, #candidate{block = {key_block, KH}}, Target, _} = _Info]}},
            #aestratum_state{} = State) ->
    TargetInt  = aeminer_pow:scientific_to_integer(Target),
    BlockHash  = aestratum_miner:hash_data(HeaderBin),
    ChainEvent = #{event => recv_block,
                   block => #{block_target => TargetInt,
                              block_hash => aestratum_util:hex_encode(BlockHash),
                              block_version => aec_headers:version(KH)}},
    ?INFO("new candidate with target ~p", [TargetInt]),
    aestratum_user_register:notify({chain, ChainEvent}),
    {noreply, State};

handle_info(Req, State) ->
    ?ERROR("spurious info request ~p", [Req]),
    {noreply, State}.


handle_cast({submit_share, Miner, MinerTarget, Hash}, State) ->
    ?TXN(aestratum_db:store_share(Miner, MinerTarget, Hash)),
    {noreply, State};

handle_cast({submit_solution, BlockHash, MinerNonce, _Pow}, State) ->
    ?INFO("got solution for blockhash ~p (miner nonce = ~p)", [BlockHash, MinerNonce]),
    {noreply, State};

handle_cast(Req, State) ->
    ?ERROR("spurious cast request ~p", [Req]),
    {noreply, State}.


handle_call(Req, _From, State) ->
    ?ERROR("spurious call request ~p", [Req]),
    {reply, ignore, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

chain_reward_candidate_key_header() ->
    chain_reward_candidate_key_header(?KEYBLOCK_ROUNDS_DELAY).
chain_reward_candidate_key_header(KeyblockRoundsDelay) ->
    Height = aec_tx_pool:top_height() - KeyblockRoundsDelay,
    ok_val_err(aec_chain:get_key_header_by_height(Height)).


with_reward_share(Fun) ->
    RewardKH = chain_reward_candidate_key_header(),
    {ok, Hash} = aec_headers:hash_header(RewardKH),
    case aestratum_db:get_hash(Hash) of
        [#aestratum_hash{key = RewardShareKey}] ->
            Height = aec_headers:height(RewardKH),
            Target = aec_headers:target(RewardKH),
            {value, Fees} = aec_db:find_block_fees(Hash),
            Fun(#reward_key_block{share_key = RewardShareKey,
                                  height    = Height,
                                  hash      = Hash,
                                  target = aeminer_pow:scientific_to_integer(Target),
                                  tokens = aec_coinbase:coinbase_at_height(Height) + Fees});
        [] ->
            {ok, not_our_share}
    end.


maybe_compute_reward() ->
    with_reward_share(
      fun (#reward_key_block{share_key = RewardShareKey,
                             target = BlockTarget,
                             height = Height,
                             tokens = Amount,
                             hash = Hash}) ->
              case relative_miner_rewards(RewardShareKey, BlockTarget) of
                  {ok, MinerRelShares, LastRoundKey} ->
                      {ok, #aestratum_reward{height = Height,
                                             hash = Hash,
                                             pool = ?POOL_RELATIVE_SHARES,
                                             miners = MinerRelShares,
                                             amount = Amount,
                                             round_key = LastRoundKey}};
                  Error ->
                      Error
              end
      end).

%%%%%%%%%%

relative_payments(#aestratum_reward{height = Height, amount = TotalAmount,
                                    pool = PoolRelMap, miners = MinerRelMap}) ->
    PoolAmount   = round(TotalAmount * ?POOL_PERCENT_SUM / 100),
    MinersAmount = TotalAmount - PoolAmount,
    MinerBatches = idxs(map_to_batches(MinerRelMap), 1),
    if PoolAmount >  0 -> [#aestratum_payment{id = {Height, 0},
                                              total = PoolAmount,
                                              relmap = PoolRelMap}];
       PoolAmount =< 0 -> []
    end ++ batches_to_payments(MinerBatches, MinersAmount, Height).

store_payments(#aestratum_reward{} = Reward) ->
    store_payments(relative_payments(Reward));
store_payments(Ps) when is_list(Ps) ->
    [ok_val_err(aestratum_db:store_payment(P)) || P <- Ps].


payment_spec(_WasPaid = true) ->
    ets:fun2ms(fun (#aestratum_payment{tx_hash = TH} = P) when is_binary(TH) -> P end);
payment_spec(_WasPaid = false) ->
    ets:fun2ms(fun (#aestratum_payment{tx_hash = undefined} = P) -> P end).

select_payments(WasPaid) ->
    mnesia:select(aestratum_payment, payment_spec(WasPaid), read).
select_payments(WasPaid, N) ->
    {Res, _} = mnesia:select(aestratum_payment, payment_spec(WasPaid), N, read),
    Res.

has_payments(WasPaid) ->
    length(select_payments(WasPaid, 1)) > 0.

oldest_unpaid_payment() ->
    lazy_hd(select_payments(false, 1), ok(), const(none)).


send_payment(#aestratum_payment{tx_hash = undefined,
                                total = TotalAmount,
                                relmap = RelMap} = P) ->
    Balance      = balance(),
    TotalAmount  < Balance orelse error({caller_insufficient_funds, Balance, TotalAmount}),
    {Fee, Gas}   = estimate_costs(absolute_amounts(RelMap, TotalAmount)),
    TotalAmount1 = TotalAmount - Fee - Gas,
    TotalAmount1 > 0 orelse error({negative_payment, TotalAmount, Fee, Gas}),
    AbsMap       = absolute_amounts(RelMap, TotalAmount1),
    PayoutTxArgs = payout_call_tx_args(AbsMap, #{fee => Fee, gas => Gas}),
    CallTx   = ok_val_err(aect_call_tx:new(PayoutTxArgs)),
    BinaryTx = aec_governance:add_network_id(aetx:serialize_to_binary(CallTx)),
    SignedTx = aetx_sign:new(CallTx, [enacl:sign_detached(BinaryTx, ?CALLER_PRIVKEY)]),
    ok_err(aec_tx_pool:push(SignedTx), pushing_payout_call_tx),
    TxHash   = aetx_sign:hash(SignedTx),
    Nonce    = maps:get(nonce, PayoutTxArgs),
    Date     = calendar:now_to_datetime(erlang:timestamp()),
    {ok, P1} = ?TXN(aestratum_db:update_payment(P, AbsMap, Fee, Gas, TxHash, Nonce, Date)),
    P1.


push_payment(#aestratum_payment{id = Id} = P, S) ->
    try ?TXN(send_payment(P)) of
        #aestratum_payment{tx_hash = TH} = P1 ->
            log_push_tx(P1),
            S#aestratum_state{pending_tx = Id,
                              pending_tx_hash = TH}
    catch
        _:Rsn ->
            ?ERROR("payment ~p failed: ~p", [Id, Rsn]),
            S
    end.


min_gas_price() ->
    max(aec_governance:minimum_gas_price(1), % latest prototocol on height 1
        aec_tx_pool:minimum_miner_gas_price()).

contract_gas(TransfersCount) ->
    ?PAYMENT_CONTRACT_INIT_GAS + (TransfersCount * ?PAYMENT_CONTRACT_GAS_PER_TRANSFER).

estimate_costs(Transfers) ->
    estimate_costs(Transfers, aec_tx_pool:top_height()).
estimate_costs(Transfers, Height) ->
    {ok, Tx} = aect_call_tx:new(payout_call_tx_args(Transfers, Height)),
    {aetx:min_fee(Tx, Height), aetx:min_gas(Tx, Height)}.


format_payout_call_args(#{} = Transfers) ->
    Tuples = maps:fold(
               fun (Addr, Tokens, Acc) ->
                       ["(#"
                        ++ address_to_hex(Addr)
                        ++ ","
                        ++ integer_to_list(Tokens)
                        ++ ")" | Acc]
               end, [], Transfers),
    "[" ++ string:join(Tuples, ",") ++ "]".

address_to_hex(Addr) ->
    aeu_hex:bin_to_hex(aestratum_util:account_address_to_pubkey(Addr)).

%% payout_call_tx_args(Transfers) ->
%%     payout_call_tx_args(Transfers, aec_tx_pool:top_height()).

payout_call_tx_args(Transfers, Opts) when is_map(Opts) ->
    payout_call_tx_args(Transfers, aec_tx_pool:top_height(), Opts);
payout_call_tx_args(Transfers, Height) when is_integer(Height) ->
    payout_call_tx_args(Transfers, Height, #{}).
payout_call_tx_args(Transfers, Height, Opts) ->
    {value, Account} = aec_chain:get_account(?CALLER_PUBKEY),
    Amount = sum_values(Transfers),
    CallArgs = format_payout_call_args(Transfers),
    SourceCode = maps:get(contract_source, ?CONTRACT),
    {ok, CallData, _, _} = aeso_compiler:create_calldata(SourceCode, "payout", [CallArgs]),
    <<Int256:256>> = <<1:256/little-unsigned-integer-unit:1>>,
    Args = #{contract_id => aec_id:create(contract, ?CONTRACT_PUBKEY),
             caller_id   => aec_id:create(account, ?CALLER_PUBKEY),
             nonce       => aec_accounts:nonce(Account) + 1,
             call_data   => CallData,
             abi_version => ?ABI_SOPHIA_1,
             amount      => Amount,
             fee         => maps:get(fee, Opts, Int256),
             gas         => maps:get(gas, Opts, contract_gas(maps:size(Transfers))),
             gas_price   => maps:get(gas_price, Opts, min_gas_price())},
    {ok, Tx0} = aect_call_tx:new(Args),
    Args#{fee => aetx:min_fee(Tx0, Height)}.


%%%%%%%%%%

balance() ->
    Account = tag_val_err(aec_chain:get_account(?CALLER_PUBKEY), value, no_account),
    aec_accounts:balance(Account).


sum_group_shares(SliceCont, BlockTarget) ->
    sum_group_shares(SliceCont, BlockTarget, 0.0, #{}).

sum_group_shares('$end_of_table', _BlockTarget, SumScores, Groups) ->
    {SumScores, Groups};
sum_group_shares({Shares, Cont}, BlockTarget, SumScores, Groups) ->
    {_, SumScores1, Groups1} = lists:foldl(fun sum_group_share/2,
                                           {BlockTarget, SumScores, Groups},
                                           Shares),
    sum_group_shares(mnesia:select(Cont), BlockTarget, SumScores1, Groups1).

sum_group_share(#aestratum_share{miner = Miner, target = MinerTarget},
                {BlockTarget, SumScore, Groups}) ->
    Total = maps:get(Miner, Groups, 0),
    Score = MinerTarget / BlockTarget,
    {BlockTarget, SumScore + Score, Groups#{Miner => Total + Score}}.


%% relative_shares(ScoredShares) ->
%%     relative_shares(ScoredShares, sum_values(ScoredShares)).

relative_shares(ScoredShares, SumScores) ->
    maps:map(fun (_, Score) -> Score / SumScores end, ScoredShares).


absolute_amounts(NormalizedRelativeShares, Amount) ->
    {AbsoluteAmounts, _} =
        maps:fold(fun (Address, RelScore, {Res, TokensLeft}) ->
                          Tokens = min(round(RelScore * Amount), TokensLeft),
                          {Res#{Address => Tokens}, TokensLeft - Tokens}
                  end, {#{}, Amount}, NormalizedRelativeShares),
    AbsoluteAmounts.


relative_miner_rewards(RewardShareKey, BlockTarget) ->
    case aestratum_db:shares_range(RewardShareKey) of
        {ok, FirstShareKey, LastRoundShareKey} ->
            Selector = aestratum_db:shares_selector(FirstShareKey, LastRoundShareKey),
            SliceCont = aestratum_db:shares_slices(Selector),
            {SumScores, Groups} = sum_group_shares(SliceCont, BlockTarget),
            {ok, relative_shares(Groups, SumScores), LastRoundShareKey};
        {error, Reason} ->
            {error, Reason}
    end.

map_to_batches(Map) ->
    [maps:from_list(Batch) ||
        Batch <- aestratum_conv:list_to_chunks(
                   maps:to_list(Map), ?PAYMENT_CONTRACT_BATCH_SIZE)].

batches_to_payments(Batches, Amount, Height) ->
    {_, Payments} = lists:foldr(payment_distributor(Height, Amount),
                                {Amount, []}, Batches),
    Payments.


payment_distributor(Height, TotalAmount) ->
    distributor(TotalAmount,
                fun (I, BatchAmount, BatchRelMap, Acc) ->
                        [#aestratum_payment{id = {Height, I},
                                            total = BatchAmount,
                                            relmap = BatchRelMap} | Acc]
                end).

distributor(TotalAmount, F) ->
    fun ({I, BatchShares}, {TokensLeft, Acc}) ->
            BatchSharesSum = sum_values(BatchShares),
            case min(round(BatchSharesSum * TotalAmount), TokensLeft) of
                0 ->
                    {TokensLeft, Acc};
                BatchAmount ->
                    BatchRelShares = relative_shares(BatchShares, BatchSharesSum),
                    {TokensLeft - BatchAmount, F(I, BatchAmount, BatchRelShares, Acc)}
            end
    end.


log_push_tx(#aestratum_payment{id = {Height, I}, tx_hash = <<TH/binary>>, nonce = Nonce,
                               absmap = AbsMap, total = Total, fee = Fee, gas = Gas}) ->
    NetTotal = Total - Fee - Gas,
    NumTrans = maps:size(AbsMap),
    ?INFO("payment contract call tx ~p (~p) pushed to mempool (nonce = ~p, id = ~p), "
          ++ "distributing reward ~p to ~p beneficiaries using fee ~p and gas ~p",
          [aestratum_util:tx_address(TH), TH, Nonce, {Height, I}, NetTotal, NumTrans, Fee, Gas]).

sum_values(M) when is_map(M) ->
    lists:sum(maps:values(M)).

idxs(Xs, From) ->
    lists:zip(lists:seq(From, From + length(Xs) - 1), Xs).
