-module(aestratum).

-behaviour(gen_server).

-export([start_link/0,
         submit_share/3,
         submit_solution/3,
         top_height/0,
         status/0,
         status/1,
         db_keys/1,
         tab_keys/0,
         tab_keys/1,
         sent_payments/0,
         pending_payments/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-include("aestratum.hrl").
-include("aestratum_log.hrl").
-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aecore/include/blocks.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-import(aestratum_fn, [ok_err/2, ok_val_err/1, ok_val_err/2]).

-define(PAYOUT_CHECK_INTERVAL,            timer:minutes(2)).
-define(CHAIN_PAYMENT_TX_CHECK_INTERVAL,  timer:seconds(10)).
-define(CANDIDATE_CACHE_CLEANUP_INTERVAL, timer:minutes(15)).

-define(PAYMENT_CONTRACT_INIT_GAS, 250).           % measured
-define(PAYMENT_CONTRACT_GAS_PER_TRANSFER, 22000). % measured

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
        {height,
         balance,
         tx_push_pid,
         pending_tx_id,
         pending_tx_hash}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

submit_share(<<"ak_", _/binary>> = Miner, MinerTarget, <<CandidateBlockHash/binary>>) ->
    true = is_integer(MinerTarget) andalso MinerTarget >= 0,
    gen_server:cast(?MODULE, {submit_share, Miner, MinerTarget, CandidateBlockHash}).

submit_solution(CandidateBlockHash, Nonce, Pow) ->
    gen_server:cast(?MODULE, {submit_solution, CandidateBlockHash, Nonce, Pow}).

status() ->
    ConnPids = aestratum_user_register:conn_pids(),
    [aestratum_handler:status(ConnPid) || ConnPid <- ConnPids].

status(Account) when is_binary(Account) ->
    case aestratum_user_register:find(Account) of
        {ok, #{conn_pid := ConnPid}} -> aestratum_handler:status(ConnPid);
        {error, _Rsn} = Err          -> Err
    end.

db_keys(Height) ->
    ?TXN(aestratum_db:keys(Height)).

tab_keys() ->
    ?TXN(lists:foldl(fun (T, Acc) -> Acc#{T => mnesia:all_keys(T)} end, #{}, ?TABS)).

tab_keys(Tab) ->
    ?TXN(mnesia:all_keys(Tab)).

sent_payments() ->
    ?TXN(aestratum_db:select_payments(true)).

pending_payments() ->
    ?TXN(aestratum_db:select_payments(false)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    PoolRelShares = aestratum_reward:relative_shares(?POOL_PERCENT_SHARES, ?POOL_PERCENT_SUM),
    aestratum_env:set(#{pool_relative_shares => PoolRelShares}),
    ?TXN(aestratum_db:is_empty(?ROUNDS_TAB) andalso aestratum_db:store_round()),
    aec_events:subscribe(stratum_new_candidate),
    aec_events:subscribe(top_changed),
    {ok, _} = timer:send_interval(?PAYOUT_CHECK_INTERVAL, payout_check),
    {ok, _} = timer:send_interval(?CHAIN_PAYMENT_TX_CHECK_INTERVAL, chain_payment_tx_check),
    {ok, _} = timer:send_interval(?CANDIDATE_CACHE_CLEANUP_INTERVAL, candidate_cache_cleanup),
    State0  = #aestratum_state{height  = top_height(),
                               balance = balance()},
    State1  = case ?TXN(aestratum_db:select_payments(true, 1)) of
                  [#aestratum_payment{id = TxId, tx_hash = TxHash}] ->
                      State0#aestratum_state{pending_tx_id   = TxId,
                                             pending_tx_hash = TxHash};
                  [] ->
                      State0
              end,
    {ok, State1}.


handle_info({gproc_ps_event, top_changed, #{info := #{block_type := key, height := Height}}},
            #aestratum_state{balance = LastBalance} = S) ->
    self() ! keyblock,
    NewBalance = balance(),
    log_changed_balance(NewBalance, LastBalance),
    S1 = S#aestratum_state{height = Height, balance = NewBalance},
    {noreply, S1};
handle_info({gproc_ps_event, top_changed, _}, S) ->
    {noreply, S};

handle_info(keyblock, #aestratum_state{height = Height} = S) when is_integer(Height) ->
    ?TXN(aestratum_db:store_round()),
    case Height - ?REWARD_KEYBLOCK_DELAY of
        RewardHeight when RewardHeight > 0 ->
            case ?TXN(maybe_compute_reward(RewardHeight)) of
                {ok, not_our_share} ->
                    ok;
                {ok, #aestratum_reward{} = R} ->
                    ?TXN(begin
                             aestratum_db:store_payments(R),
                             aestratum_db:store_reward(
                               R#aestratum_reward{pool   = transformed,  % map is in payments now
                                                  miners = transformed}) % map is in payments now
                         end),
                    self() ! payout_check;
                {error, Reason} ->
                    ?ERROR("reward computation failed: ~p", [Reason])
            end;
        _Height ->
            ok
    end,
    {noreply, S};

handle_info(payout_check, #aestratum_state{tx_push_pid = Pid} = S) when is_pid(Pid) ->
    {noreply, S};
handle_info(payout_check, #aestratum_state{balance = undefined} = S) ->
    {noreply, S};
handle_info(payout_check, #aestratum_state{tx_push_pid = undefined,
                                           balance = Balance} = S)
  when is_integer(Balance) ->
    S1 = case ?TXN(aestratum_db:has_payments(true)) of
             true  ->
                 S;
             false ->
                 case ?TXN(aestratum_db:oldest_unpaid_payment()) of
                     {ok, #aestratum_payment{total = Total} = P} when Total =< Balance ->
                         push_payment(P, S);
                     {ok, #aestratum_payment{total = Total} = P} when Total > Balance ->
                         log_insufficient_balance(Balance, P),
                         S;
                     none ->
                         S
                 end
         end,
    {noreply, S1};

handle_info({'DOWN', _Ref, process, Pid, _Info}, #aestratum_state{tx_push_pid = Pid} = S) ->
    {noreply, S#aestratum_state{tx_push_pid = undefined}};

handle_info(chain_payment_tx_check, #aestratum_state{height = TopHeight,
                                                     tx_push_pid = undefined,
                                                     pending_tx_id = {Height, _} = TxId,
                                                     pending_tx_hash = <<TxHash/binary>>} = S) ->
    S1 = case payout_tx_persisted(TxHash, TopHeight) of
             true ->
                 ?TXN(case length(aestratum_db:payments(Height)) == 1 of
                          true  -> aestratum_db:delete_records(Height);
                          false -> aestratum_db:delete_payment(TxId)
                      end),
                 S#aestratum_state{pending_tx_id = undefined,
                                   pending_tx_hash = undefined};
             false ->
                 S
         end,
    {noreply, S1};
handle_info(chain_payment_tx_check, S) ->
    {noreply, S};

handle_info(candidate_cache_cleanup, S) ->
    ?TXN(aestratum_db:delete_candidates_older_than(
           aestratum_conv:delta_secs_to_universal_datetime(
             -trunc(?CANDIDATE_CACHE_CLEANUP_INTERVAL / 1000)))),
    {noreply, S};

handle_info({gproc_ps_event, stratum_new_candidate,
             #{info := [{HeaderBin, #candidate{block = BlockCand} = C, Target, _} = _Info]}},
            #aestratum_state{} = State) ->
    case aec_blocks:is_key_block(BlockCand) of
        true ->
            KH = aec_blocks:to_header(BlockCand),
            BlockHash  = aestratum_miner:hash_data(HeaderBin),
            {ok, _}    = ?TXN(aestratum_db:store_candidate(BlockHash, HeaderBin, C)),
            TargetInt  = aeminer_pow:scientific_to_integer(Target),
            ChainEvent = #{event => recv_block,
                           block => #{block_target  => TargetInt,
                                      block_hash    => aestratum_conv:hex_encode(BlockHash),
                                      block_version => aec_headers:version(KH)}},
            ?INFO("new candidate with target ~p", [TargetInt]),
            aestratum_user_register:notify({chain, ChainEvent});
        false ->
            ok
    end,
    {noreply, State};

handle_info(Req, State) ->
    ?ERROR("spurious info request ~p", [Req]),
    {noreply, State}.


handle_cast({submit_share, Miner, MinerTarget, CandidateBlockHash}, State) ->
    ?TXN(aestratum_db:store_share(Miner, MinerTarget, CandidateBlockHash)),
    {noreply, State};

handle_cast({submit_solution, CandidateBlockHash, Nonce, Pow}, State) ->
    case ?TXN(aestratum_db:get_candidate(CandidateBlockHash)) of
        {ok, #aestratum_candidate{header = HeaderBin,
                                  record = #candidate{block = CandidateBlock}}} ->
            KeyBlock        = aec_blocks:set_nonce_and_pow(CandidateBlock, Nonce, Pow),
            KeyBlockHeader  = aec_blocks:to_header(KeyBlock),
            {ok, BlockHash} = aec_headers:hash_header(KeyBlockHeader),
            ok = ?TXN(aestratum_db:mark_share_as_solution(CandidateBlockHash, BlockHash)),
            ?INFO("got solution for blockhash ~p (nonce = ~p)", [BlockHash, Nonce]),
            aec_conductor:stratum_reply({Nonce, Pow}, HeaderBin);
        {error, not_found} ->
            ?ERROR("candidate for blockhash ~p lost", [CandidateBlockHash])
    end,
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

push_payment(#aestratum_payment{id = Id} = P, S) ->
    try ?TXN(send_payment(P, S#aestratum_state.height)) of
        {Pid, TxHash, PayoutTxArgs, AbsMap} ->
            Date = calendar:now_to_datetime(erlang:timestamp()),
            P1   = ok_val_err(?TXN(aestratum_db:update_payment(P, AbsMap, TxHash, Date))),
            log_push_tx(P1, PayoutTxArgs),
            S#aestratum_state{tx_push_pid = Pid,
                              pending_tx_id = Id,
                              pending_tx_hash = TxHash}
    catch
        _:Rsn ->
            ?ERROR("payment ~p failed: ~p", [Id, Rsn]),
            S
    end.


with_reward_share(RewardHeight, Fun) ->
    RewardKH   = ok_val_err(aec_chain:get_key_header_by_height(RewardHeight)),
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


maybe_compute_reward(RewardHeight) ->
    case aestratum_db:get_reward(RewardHeight) of
        {error, not_found} ->
            with_reward_share(
              RewardHeight,
              fun (#reward_key_block{share_key = ShareKey,
                                     target = BlockTarget,
                                     height = Height,
                                     tokens = Amount,
                                     hash   = Hash}) ->
                      case aestratum_reward:relative_miner_rewards(ShareKey, BlockTarget) of
                          {ok, MinerRelShares, RoundKey} ->
                              {ok, #aestratum_reward{pool   = ?POOL_RELATIVE_SHARES,
                                                     miners = MinerRelShares,
                                                     amount = Amount,
                                                     hash   = Hash,
                                                     height = Height,
                                                     round_key = RoundKey}};
                          Error ->
                              Error
                      end
              end);
        {ok, _} ->
            %% we don't want to compute duplicitious reward
            {ok, not_our_share}
    end.


%%%%%%%%%%

send_payment(#aestratum_payment{tx_hash = undefined,
                                total = GrossAmount,
                                relmap = RelativeMap},
             TopHeight) ->
    Balance       = balance(),
    GrossAmount   < Balance orelse error({caller_insufficient_funds, Balance, GrossAmount}),
    AbsoluteMap0  = aestratum_reward:absolute_amounts(RelativeMap, GrossAmount),
    #{fee := Fee, run_fee := RunFee} = payout_call_tx_args(AbsoluteMap0, TopHeight),
    NetAmount     = GrossAmount - Fee - RunFee, % what is actually distributed
    NetAmount     > 0 orelse error({negative_net_amount, NetAmount, {GrossAmount, Fee, RunFee}}),
    AbsoluteMap   = aestratum_reward:absolute_amounts(RelativeMap, NetAmount),
    NetTxArgs     = payout_call_tx_args(AbsoluteMap, TopHeight),
    CallTx        = ok_val_err(aect_call_tx:new(NetTxArgs)),
    BinaryTx      = aec_governance:add_network_id(aetx:serialize_to_binary(CallTx)),
    SignedTx      = aetx_sign:new(CallTx, [enacl:sign_detached(BinaryTx, ?CALLER_PRIVKEY)]),
    TxHash        = aetx_sign:hash(SignedTx),
    {Pid, _Mon}   = spawn_monitor(fun () -> tx_pool_push(SignedTx) end),
    {Pid, TxHash, NetTxArgs, AbsoluteMap}.

tx_pool_push(SignedTx) ->
    ok_err(aec_tx_pool:push(SignedTx, tx_created, infinity), pushing_payout_call_tx).


min_gas_price() ->
    max(aec_governance:minimum_gas_price(1), % latest prototocol on height 1
        aec_tx_pool:minimum_miner_gas_price()).

estimate_consumed_gas(NumTransfers) ->
    ?PAYMENT_CONTRACT_INIT_GAS + NumTransfers * ?PAYMENT_CONTRACT_GAS_PER_TRANSFER.


create_call_args(#{} = Transfers) ->
    maps:fold(
      fun (Addr, Tokens, Acc) ->
              [{aestratum_conv:account_address_to_integer(Addr), Tokens} | Acc]
      end, [], Transfers).


payout_call_tx_args(Transfers, TopHeight) when is_integer(TopHeight) ->
    {value, Account} = aec_chain:get_account(?CALLER_PUBKEY),
    GasPrice = min_gas_price(),
    CallArgs = create_call_args(Transfers),
    CallType = [{list, {tuple, [word, word]}}],
    {ok, CallData} = aeb_aevm_abi:create_calldata("payout", [CallArgs], CallType, word),
    <<Int256:256>> = <<1:256/little-unsigned-integer-unit:1>>,
    Args = #{contract_id => aeser_id:create(contract, ?CONTRACT_PUBKEY),
             caller_id   => aeser_id:create(account, ?CALLER_PUBKEY),
             nonce       => aec_accounts:nonce(Account) + 1,
             call_data   => CallData,
             abi_version => ?ABI_AEVM_SOPHIA_1,
             amount      => aestratum_fn:sum_values(Transfers),
             fee         => Int256,
             gas         => Int256,
             gas_price   => Int256},
    Tx0    = ok_val_err(aect_call_tx:new(Args)),
    MinGas = aetx:min_gas(Tx0, TopHeight),
    MinFee = GasPrice * MinGas,
    RunGas = estimate_consumed_gas(maps:size(Transfers)),
    RunFee = GasPrice * RunGas,
    Args#{gas_price => GasPrice,
          gas => MinGas, fee => MinFee,          % minimal for tx to pass mempool validation
          run_gas => RunGas, run_fee => RunFee}. % estimation for contract code to finish


payout_tx_persisted(TxHash, TopHeight) ->
    payout_tx_persisted(TxHash, TopHeight, ?PAYOUT_KEYBLOCK_DELAY).
payout_tx_persisted(TxHash, TopHeight, KeyBlocksDelay) ->
    case aec_chain:find_tx_location(TxHash) of
        <<BlockHash/binary>> ->
            {ok, Block} = aec_chain:get_block(BlockHash),
            (TopHeight - aec_blocks:height(Block)) >= KeyBlocksDelay;
        _ ->
            false
    end.

%%%%%%%%%%

top_height() ->
    case aec_chain:top_key_block() of
        {ok, KB} -> aec_blocks:height(KB);
        _ -> 0
    end.


balance() ->
    case aec_chain:get_account(?CALLER_PUBKEY) of
        {value, Account} -> aec_accounts:balance(Account);
        none -> undefined
    end.


log_push_tx(#aestratum_payment{id = {Height, I}, tx_hash = <<TH/binary>>,
                               absmap = AbsMap, total = Total},
            #{nonce := Nonce, fee := Fee, run_fee := RunFee}) ->
    NetTotal  = Total - Fee - RunFee,
    NumTrans  = maps:size(AbsMap),
    EncTxHash = aeser_api_encoder:encode(tx_hash, TH),
    ?INFO("payment contract call tx ~p (~p) pushed to mempool (nonce = ~p, id = ~p), "
          ++ "distributing reward ~p to ~p beneficiaries (tx fee = ~p, ct fee = ~p)",
          [EncTxHash, TH, Nonce, {Height, I}, NetTotal, NumTrans, Fee, RunFee]).

log_changed_balance(NewBalance, undefined) when is_integer(NewBalance) ->
    ?INFO("balance changed: received ~p tokens (total = ~p), stratum account created",
          [NewBalance, NewBalance]);
log_changed_balance(NewBalance, LastBalance) when NewBalance > LastBalance ->
    ?INFO("balance changed: received ~p tokens (total = ~p)",
          [NewBalance - LastBalance, NewBalance]);
log_changed_balance(NewBalance, LastBalance) when NewBalance < LastBalance ->
    ?INFO("balance changed: subtracted ~p tokens (total = ~p)",
          [LastBalance - NewBalance, NewBalance]);
log_changed_balance(Balance, Balance) ->
    ok.

log_insufficient_balance(Balance, #aestratum_payment{total = Total, id = Id})
  when Balance < Total ->
    ?INFO("insufficient balance (~p) for payment ~p (needs ~p more tokens), delaying...",
          [Balance, Id, Total - Balance]).
