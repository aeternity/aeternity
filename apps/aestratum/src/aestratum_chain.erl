%%%-------------------------------------------------------------------
%%% @copyright (C) 2019,
%%% @doc
%%%
%%% Interface to AE node
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aestratum_chain).

-behaviour(gen_server).

%% TODO: eunit
%% TODO: type specs

%% API.
-export([start_link/3,
         payout_rewards/4,
         get_reward_key_header/1,
         hash_header/1,
         header_info/1]).

-ifdef(COMMON_TEST).
-export([calculate_rewards/2]).
-endif.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(CHAIN_TOP_CHECK_INTERVAL, 1000). % 1 second
-define(CHAIN_PAYMENT_TX_CHECK_INTERVAL, 10000). % 10 seconds

-define(PAYMENT_CONTRACT_BATCH_SIZE, 100).
-define(PAYMENT_CONTRACT_MIN_FEE_MULTIPLIER, 2.0).
-define(PAYMENT_CONTRACT_MIN_GAS_MULTIPLIER, 1.3).
-define(PAYMENT_CONTRACT_INIT_GAS, 250). % measured
-define(PAYMENT_CONTRACT_GAS_PER_TRANSFER, 22000). % measured

-include("aestratum.hrl").
-include_lib("aecore/src/aec_conductor.hrl").
-include_lib("aecontract/src/aecontract.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%%===================================================================
%%% STATE
%%%===================================================================

-record(chain_state,
        {last_keyblock,
         new_keyblock_candidate,
         benef_sum_pcts,
         caller_nonce,
         caller_pub_key,
         caller_priv_key,
         contract}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(BenefSumPcts, {_, _} = Keys, ContractPubKey) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [BenefSumPcts, Keys, ContractPubKey], []).

-spec payout_rewards(non_neg_integer(), non_neg_integer(), map(), map()) -> ok.
payout_rewards(Height, BlockReward, PoolRewards, MinersRewards) ->
    gen_server:cast(?MODULE, {payout_rewards, Height, BlockReward, PoolRewards, MinersRewards}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([BenefSumPcts, {CallerPubKey, CallerPrivKey}, ContractPubKey]) ->
    ContractPath = filename:join(code:priv_dir(aestratum), "Payout.aes"),
    {ok, _} = timer:send_interval(?CHAIN_TOP_CHECK_INTERVAL, chain_top_check),
    {ok, _} = timer:send_interval(?CHAIN_PAYMENT_TX_CHECK_INTERVAL, chain_payment_tx_check),
    {ok, Contract} = aeso_compiler:file(ContractPath),
    {value, CallerAcc} = aec_chain:get_account(CallerPubKey),
    CallerAddress = aehttp_api_encoder:encode(account_pubkey, CallerPubKey),
    ?info("using Stratum operator account ~s (balance = ~p, nonce = ~p)",
          [CallerAddress, aec_accounts:balance(CallerAcc), aec_accounts:nonce(CallerAcc)]),
    aec_events:subscribe(stratum_new_candidate),
    {ok, #chain_state{last_keyblock = aec_chain:top_key_block_hash(),
                      benef_sum_pcts = BenefSumPcts,
                      caller_nonce = aec_accounts:nonce(CallerAcc),
                      caller_pub_key = CallerPubKey,
                      caller_priv_key = CallerPrivKey,
                      contract = Contract#{contract_pk => ContractPubKey}}}.


handle_call(_Req, _From, State) ->
    {reply, ok, State}.


handle_cast({payout_rewards, Height, BlockReward, PoolRewards, MinersRewards}, State) ->
    State1 = send_payments(Height, BlockReward, PoolRewards, MinersRewards, State),
    {noreply, State1};

handle_cast(_Req, State) ->
    {noreply, State}.


handle_info(chain_top_check, #chain_state{} = State) ->
    NewKB = aec_chain:top_key_block_hash(),
    NewKB == State#chain_state.last_keyblock orelse aestratum_reward:keyblock(),
    {noreply, State#chain_state{last_keyblock = NewKB}};
handle_info(chain_payment_tx_check, #chain_state{} = State) ->
    delete_complete_payments(),
    {noreply, State};
handle_info({gproc_ps_event, stratum_new_candidate, #{info := Info}}, State) ->
    ?info("new candidate info: ~p", [Info]),
    State1 = case Info of
                 [{HeaderBin, #candidate{block = {key_block, KeyHeader}}, TargetSCI}] ->
                     Target = aeminer_pow:scientific_to_integer(TargetSCI),
                     BlockHash = aestratum_miner:hash_data(HeaderBin),
                     ChainEvent = #{event => recv_block,
                                    block => #{block_target => Target,
                                               block_hash => hex_encode(BlockHash),
                                               block_version => aec_headers:version(KeyHeader)}},
                     aestratum_user_register:notify({chain, ChainEvent}),
                     State#chain_state{new_keyblock_candidate = Info};
                 _ ->
                     lager:info("Malformed Candidate for Stratum ~p", [Info]),
                     State
             end,
    {noreply, State1};
handle_info(_Req, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.


%%%===================================================================
%%% Internal functions
%%%===================================================================

get_reward_key_header(RoundsDelay) ->
    case aec_chain:top_key_block() of
        {ok, {key_block, TopKeyHeader}} ->
            RewardHeight = aec_headers:height(TopKeyHeader) - RoundsDelay,
            if RewardHeight > 0 ->
                    aec_chain:get_key_header_by_height(RewardHeight);
               true ->
                    {error, cant_get_top_key_block}
            end;
        error ->
            {error, cant_get_top_key_block}
    end.


hash_header(KeyHeader) ->
    aec_headers:hash_header(KeyHeader).


header_info({KeyHeader, Hash}) ->
    {value, Fees} = aec_db:find_block_fees(Hash),
    Height = aec_headers:height(KeyHeader),
    Target = aeminer_pow:scientific_to_integer(aec_headers:target(KeyHeader)),
    Tokens = aec_coinbase:coinbase_at_height(Height) + Fees,
    {ok, Height, Target, Tokens}.


send_payments(Height, BlockReward, PoolRewards, MinersRewards, State) ->
    Payments = create_payments(Height, BlockReward, PoolRewards, MinersRewards, State),
    LastNonce = lists:foldl(fun (#aestratum_payment{nonce = Nonce} = P, MaxNonce) ->
                                    send_payment(P, State),
                                    max(Nonce, MaxNonce)
                            end, State#chain_state.caller_nonce, Payments),
    State#chain_state{caller_nonce = LastNonce + 1}.


send_payment(#aestratum_payment{nonce = Nonce,
                                fee = Fee,
                                gas = Gas,
                                rewards = Transfers,
                                tx_hash = undefined} = P,
             #chain_state{caller_pub_key = CallerPK, caller_priv_key = CallerSK,
                          contract = #{contract_pk := ContractPK, contract_source := Source}}) ->
    Opts = #{fee => Fee, gas => Gas},
    CallerData = {CallerPK, Nonce},
    ContractData = {ContractPK, Source},
    {ok, CallTx} = create_payout_call_tx(Transfers, CallerData, ContractData, Opts),
    SerializedTx = aec_governance:add_network_id(aetx:serialize_to_binary(CallTx)),
    SignedTx = aetx_sign:new(CallTx, [enacl:sign_detached(SerializedTx, CallerSK)]),
    TxHash = aetx_sign:hash(SignedTx),
    aec_tx_pool:push(SignedTx),
    ?info("payment contract call tx ~p (caller nonce = ~p) pushed to mempool, rewarding ~p beneficiaries using fee ~p and gas ~p",
          [TxHash, Nonce, maps:size(Transfers), Fee, Gas]),
    transaction(fun () -> mnesia:write(P#aestratum_payment{tx_hash = TxHash}) end),
    TxHash.


create_payments(Height, BlockReward, PoolRewards, MinersRewards,
                #chain_state{caller_pub_key = CallerPK, caller_nonce = Nonce,
                             contract = Contract} = State) ->
    BenefTokens = round(BlockReward * (State#chain_state.benef_sum_pcts / 100)),
    MinerTokens = BlockReward - BenefTokens,
    P0 = create_payment(Height, 0, BenefTokens, PoolRewards, {CallerPK, Nonce}, Contract),
    Ps = create_payments(Height, 1, MinerTokens, MinersRewards, {CallerPK, Nonce + 1}, Contract),
    ?info("created payments at block height ~p, for ~p pool operators and ~p miners, distributing ~p tokens",
          [Height, maps:size(PoolRewards), maps:size(MinersRewards), BlockReward]),
    [P0 | Ps].


create_payments(Height, StartBatchIdx, Amount, RelativeRewards,
                {CallerPK, CallerNonce}, Contract) ->
    Batches = split_list(maps:to_list(RelativeRewards), ?PAYMENT_CONTRACT_BATCH_SIZE),
    {_, _, Payments} =
        lists:foldl(
          fun (Batch, {BatchIdx, Nonce, Acc}) ->
                  RelShare = lists:foldl(fun ({_, X}, Sum) -> Sum + X end, 0, Batch),
                  BatchAmount = trunc(RelShare * Amount),
                  Batch1 = maps:from_list(Batch),
                  Payment = create_payment(Height, BatchIdx, BatchAmount, Batch1,
                                           {CallerPK, Nonce}, Contract),
                  {BatchIdx + 1, Nonce + 1, [Payment | Acc]}
          end, {StartBatchIdx, CallerNonce, []}, Batches),
    lists:reverse(Payments).


create_payment(Height, BatchIdx, Amount, RelativeRewards, {CallerPK, CallerNonce},
               #{contract_pk := ContractPK, contract_source := ContractSource}) ->
    ContractData = {ContractPK, ContractSource},
    Transfers0 = calculate_rewards(RelativeRewards, Amount),
    {MinFee, MinGas} = estimate_costs(Transfers0, {CallerPK, CallerNonce}, ContractData),
    Fee = round(MinFee * ?PAYMENT_CONTRACT_MIN_FEE_MULTIPLIER),
    Gas = round(MinGas * ?PAYMENT_CONTRACT_MIN_GAS_MULTIPLIER),
    RewardAmount = Amount - Fee - Gas,
    true = RewardAmount > 0,
    Transfers = calculate_rewards(RelativeRewards, RewardAmount),
    #aestratum_payment{height = Height,
                       index = BatchIdx,
                       nonce = CallerNonce,
                       fee = Fee,
                       gas = Gas,
                       rewards = Transfers}.


calculate_rewards(Transfers, Amount) ->
    {ResTransfers, _} =
        maps:fold(fun (Account, RelScore, {Res, TokensLeft}) ->
                          Tokens = min(round(RelScore * Amount), TokensLeft),
                          {Res#{Account => Tokens}, TokensLeft - Tokens}
                  end, {#{}, Amount}, Transfers),
    ResTransfers.


estimate_costs(Transfers, {CallerPK, CallerNonce}, {ContractPK, ContractSource}) ->
    estimate_costs(Transfers, {CallerPK, CallerNonce}, {ContractPK, ContractSource},
                   #{height => aec_tx_pool:top_height()}).
estimate_costs(Transfers, {CallerPK, CallerNonce}, {ContractPK, ContractSource}, Opts) ->
    Height = maps:get(height, Opts, aec_tx_pool:top_height()),
    {ok, Tx} = create_payout_call_tx(Transfers,
                                     {CallerPK, CallerNonce},
                                     {ContractPK, ContractSource},
                                     #{height => Height}),
    {aetx:min_fee(Tx, Height), aetx:min_gas(Tx, Height)}.


min_gas_price() ->
    max(aec_governance:minimum_gas_price(1), % latest prototocol on height 1
        aec_tx_pool:minimum_miner_gas_price()).


create_payout_call_tx(Transfers, {CallerPK, CallerNonce}, {ContractPK, ContractSrc}, Opts) ->
    Amount   = maps:fold(fun (_, Tokens, Total) -> Tokens + Total end, 0, Transfers),
    CallArgs = format_payout_call_args(Transfers),
    {ok, CallData, _, _} = aeso_compiler:create_calldata(ContractSrc, "payout", [CallArgs]),
    <<Int256:256>> = <<1:256/little-unsigned-integer-unit:1>>,
    Args = #{contract_id => aec_id:create(contract, ContractPK),
             caller_id   => aec_id:create(account, CallerPK),
             nonce       => maps:get(caller_nonce, Opts, CallerNonce),
             call_data   => CallData,
             abi_version => ?ABI_SOPHIA_1,
             amount      => Amount,
             fee         => maps:get(fee, Opts, Int256),
             gas         => maps:get(gas, Opts, contract_gas(maps:size(Transfers))),
             gas_price   => maps:get(gas_price, Opts, min_gas_price())},
    {ok, Tx0} = aect_call_tx:new(Args),
    Height    = maps:get(height, Opts, aec_tx_pool:top_height()),
    MinFee    = aetx:min_fee(Tx0, Height),
    aect_call_tx:new(Args#{fee => MinFee}).


contract_gas(TransfersCount) ->
    ?PAYMENT_CONTRACT_INIT_GAS + (TransfersCount * ?PAYMENT_CONTRACT_GAS_PER_TRANSFER).

format_payout_call_args(#{} = Transfers) ->
    Tuples = maps:fold(fun (Address, Tokens, Acc) ->
                               {account_pubkey, PK} = aehttp_api_encoder:decode(Address),
                               HexPK = aeu_hex:bin_to_hex(PK),
                               ["(#" ++ HexPK ++ "," ++ integer_to_list(Tokens) ++ ")" | Acc]
                       end, [], Transfers),
    "[" ++ string:join(Tuples, ",") ++ "]".


delete_complete_payments() ->
    Spec = ets:fun2ms(fun (#aestratum_payment{_ = '_'} = X) -> X end),
    Payments = transaction(fun () -> mnesia:select(?PAYMENTS_TAB, Spec) end),
    Checks = lists:foldl(
               fun (#aestratum_payment{tx_hash = TxHash, height = Height}, Acc) ->
                       {AllSeen, SeenTxs} = maps:get(Height, Acc, {true, []}),
                       Acc#{Height => case aec_chain:find_tx_with_location(TxHash) of
                                          {<<_/binary>>, _} -> {AllSeen, [TxHash | SeenTxs]};
                                          _ -> {false, SeenTxs}
                                      end}
               end, #{}, Payments),
    maps:fold(fun (Height, {AllSeen, SeenTxs}, _) ->
                      SeenTxs /= [] andalso delete_payment_records(SeenTxs),
                      AllSeen andalso aestratum_reward:confirm_payout(Height)
              end, ok, Checks).


delete_payment_records(TxHashes) ->
    transaction(fun () -> aestratum_db:delete_payment_records(TxHashes) end).

transaction(Fun) when is_function(Fun, 0) ->
    mnesia:activity(transaction, Fun).


split_list(Xs, BatchSize) ->
    split_list(Xs, BatchSize, []).

split_list([], _BatchSize, Res) ->
    Res;
split_list(Xs, BatchSize, Res) ->
    try lists:split(BatchSize, Xs) of
        {Batch, Rest} ->
            split_list(Rest, BatchSize, [Batch | Res])
    catch
        error:badarg -> [Xs | Res]
    end.

hex_encode(Data) ->
    list_to_binary(string:to_lower(aeu_hex:bin_to_hex(Data))).
