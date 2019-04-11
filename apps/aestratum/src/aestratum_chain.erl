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
-export([start_link/2,
         payout_rewards/3,
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

-define(PAYMENT_CONTRACT, <<"ct_2vPp9VhAH3S7u4UAW9Xsj2WhUo3bN5UiUNbJY1YAjRZ7HAsSB3">>).
-define(PAYMENT_CONTRACT_BATCH_SIZE, 100).
-define(PAYMENT_CONTRACT_MIN_FEE_MULTIPLIER, 2.0).
-define(PAYMENT_CONTRACT_MIN_GAS_MULTIPLIER, 1.3).

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
         caller_account,
         sign_priv_key,
         contract}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(BenefSumPcts, {_, _} = Keys) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [BenefSumPcts, Keys], []).

payout_rewards(Height, PoolRewards, MinersRewards) ->
    gen_server:cast(?MODULE, {payout_rewards, Height, PoolRewards, MinersRewards}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([BenefSumPcts, {SignPubKey, SignPrivKey}]) ->
    {ok, _} = timer:send_interval(?CHAIN_TOP_CHECK_INTERVAL, chain_top_check),
    {ok, _} = timer:send_interval(?CHAIN_PAYMENT_TX_CHECK_INTERVAL, chain_payment_tx_check),
    {ok, #{} = Contract} =
        aeso_compiler:file(filename:join(code:priv_dir(aestratum), "Payout.aes")),

    %% TODO - get balance from chain
    CallerAccount = aec_accounts:new(SignPubKey, 1000000000000000000000),
    %% TODO - get address of contract from deployment
    {contract_pubkey, ContractPK} = aehttp_api_encoder:decode(?PAYMENT_CONTRACT),
    Contract1 = Contract#{contract_pk => ContractPK},

    aec_events:subscribe(stratum_new_candidate),

    {ok, #chain_state{last_keyblock = aec_chain:top_key_block_hash(),
                      benef_sum_pcts = BenefSumPcts,
                      caller_account = CallerAccount,
                      sign_priv_key = SignPrivKey,
                      contract = Contract1}}. %% TODO


handle_call(_Req, _From, State) ->
    {reply, ok, State}.


handle_cast({payout_rewards, Height, BlockReward, PoolRewards, MinersRewards}, State) ->
    spawn(fun () -> send_payments(Height, BlockReward, PoolRewards, MinersRewards, State) end),
    {noreply, State};
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
    ?info("new candidate: ~p", [Info]),
    State1 = case Info of
                 [{HeaderBin, #candidate{block = Block}, Target}] ->
                     Payload = #{event => recv_block,
                                 block => #{block_target => Target,
                                            block_hash => aeu_hex:bin_to_hex(HeaderBin),
                                            block_version => aec_blocks:version(Block)}},
                     aestratum_user_register:notify({chain, Payload}),
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
    Target = aec_headers:target(KeyHeader),
    Tokens = aec_coinbase:coinbase_at_height(Height) + Fees,
    {ok, Height, Target, Tokens}.


send_payments(Height, BlockReward, PoolRewards, MinersRewards, State) ->
    Payments = create_payments(Height, BlockReward, PoolRewards, MinersRewards, State),
    maps:map(fun (_, #aestratum_payment{} = P) -> send_payment(P, State) end, Payments).


send_payment(#aestratum_payment{fee = Fee,
                                gas = Gas,
                                rewards = Transfers,
                                tx_hash = undefined} = P,
             #chain_state{caller_account = Caller, contract = Contract, sign_priv_key = SK}) ->
    Opts = #{fee => Fee, gas => Gas},
    ContractData = {maps:get(contract_pk, Contract), maps:get(contract_source, Contract)},
    {ok, CallTx} = create_payout_call_tx(Transfers, Caller, ContractData, Opts),
    SerializedTx = aec_governance:add_network_id(aetx:serialize_to_binary(CallTx)),
    SignedTx = aetx_sign:new(CallTx, [enacl:sign_detached(SerializedTx, SK)]),
    TxHash = aetx_sign:hash(SignedTx),
    aec_tx_pool:push(SignedTx),
    ?info("payment contract call tx ~p pushed to mempool, rewarding ~p beneficiaries using fee ~p and gas ~p",
          [TxHash, maps:size(Transfers), Fee, Gas]),
    transaction(fun () -> mnesia:write(P#aestratum_payment{tx_hash = TxHash}) end),
    TxHash.


create_payments(Height, BlockReward, PoolRewards, MinersRewards,
                #chain_state{caller_account = Caller, contract = Contract} = State) ->
    BenefRewards = round(BlockReward * (State#chain_state.benef_sum_pcts / 100)),
    MinerRewards = BlockReward - BenefRewards,
    P0 = create_payment(Height, 0, BenefRewards, PoolRewards, Caller, Contract),
    Ps = create_payments(Height, 1, MinerRewards, MinersRewards, Caller, Contract),
    ?info("created payments at block height ~p, for ~p pool operators and ~p miners, distributing ~p tokens",
          [Height, maps:size(PoolRewards), maps:size(MinerRewards), BlockReward]),
    Ps#{0 => P0}.


create_payments(Height, StartBatchIdx, Amount, RelativeRewards, CallerAcc, Contract) ->
    Batches = split_list(maps:to_list(RelativeRewards), ?PAYMENT_CONTRACT_BATCH_SIZE),
    {_, Payments} =
        lists:foldl(
          fun (Batch, {I, Acc}) ->
                  RelShare = lists:foldl(fun ({_, X}, Sum) -> Sum + X end, 0, Batch),
                  BatchAmount = trunc(RelShare * Amount),
                  Batch1 = maps:from_list(Batch),
                  Payment = create_payment(Height, I, BatchAmount, Batch1, CallerAcc, Contract),
                  {I + 1, Acc#{I => Payment}}
          end, {StartBatchIdx, #{}}, Batches),
    Payments.


create_payment(Height, BatchIdx, Amount, RelativeRewards, CallerAcc,
               #{contract_pk := ContractPK, contract_source := ContractSource}) ->
    ContractData = {ContractPK, ContractSource},
    Transfers0 = calculate_rewards(RelativeRewards, Amount),
    {MinFee, MinGas} = estimate_costs(Transfers0, CallerAcc, ContractData),
    Fee = round(MinFee * ?PAYMENT_CONTRACT_MIN_FEE_MULTIPLIER),
    Gas = round(MinGas * ?PAYMENT_CONTRACT_MIN_GAS_MULTIPLIER),
    RewardAmount = Amount - Fee - Gas,
    true = RewardAmount > 0,
    Transfers = calculate_rewards(RelativeRewards, RewardAmount),
    #aestratum_payment{height = Height,
                       index = BatchIdx,
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


estimate_costs(Transfers, CallerAcc, {ContractPK, ContractSource}) ->
    estimate_costs(Transfers, CallerAcc, {ContractPK, ContractSource},
                   #{height => aec_tx_pool:top_height()}).
estimate_costs(Transfers, CallerAcc, {ContractPK, ContractSource}, Opts) ->
    Height = maps:get(height, Opts, aec_tx_pool:top_height()),
    {ok, Tx} = create_payout_call_tx(Transfers, CallerAcc, {ContractPK, ContractSource},
                                     #{height => Height}),
    {aetx:min_fee(Tx, Height), aetx:min_gas(Tx, Height)}.


min_gas_price() ->
    max(aec_governance:minimum_gas_price(1), % latest prototocol on height 1
        aec_tx_pool:minimum_miner_gas_price()).


create_payout_call_tx(Transfers, CallerAcc, {ContractPK, ContractSource}, Opts) ->
    Amount   = maps:fold(fun (_, Tokens, Total) -> Tokens + Total end, 0, Transfers),
    CallArgs = format_payout_call_args(Transfers),
    {ok, CallData, _, _} = aeso_compiler:create_calldata(ContractSource, "payout", [CallArgs]),
    SufficientGas  = 250 + (maps:size(Transfers) * 22000),
    <<Int256:256>> = <<1:256/little-unsigned-integer-unit:1>>,
    Args = #{contract_id => aec_id:create(contract, ContractPK),
             caller_id   => aec_accounts:id(CallerAcc),
             nonce       => aec_accounts:nonce(CallerAcc) + 1,
             call_data   => CallData,
             abi_version => ?ABI_SOPHIA_1,
             amount      => Amount,
             fee         => maps:get(fee, Opts, Int256),
             gas         => maps:get(gas, Opts, SufficientGas),
             gas_price   => maps:get(gas_price, Opts, min_gas_price())},
    {ok, Tx0} = aect_call_tx:new(Args),
    Height    = maps:get(height, Opts, aec_tx_pool:top_height()),
    MinFee    = aetx:min_fee(Tx0, Height),
    aect_call_tx:new(Args#{fee => MinFee}).


format_payout_call_args(#{} = Transfers) ->
    Tuples = maps:fold(fun (PK, Tokens, Acc) ->
                               ["(#" ++ aeu_hex:bin_to_hex(PK) ++ "," ++
                                    integer_to_list(Tokens) ++ ")" | Acc]
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







%% x_spend() ->
%%     PK = fun (X) -> <<X/binary, (crypto:strong_rand_bytes(32 - size(X)))/binary>> end,

%%     SndPK = PK(<<"SND">>),
%%     RcvPK = PK(<<"RCV">>),

%%     io:format("////////// SND PK = ~w~n////////// RCV PK = ~w~n", [SndPK, RcvPK]),

%%     SndAcc = aec_accounts:new(SndPK, 1000000000000000),
%%     RcvAcc = aec_accounts:new(RcvPK, 0),

%%     {ok, SpendTx} =
%%         aec_spend_tx:new(#{sender_id => aec_id:create(account, SndPK),
%%                            recipient_id => aec_id:create(account, RcvPK),
%%                            amount => 200000000000,
%%                            fee => 50000000000,
%%                            nonce => 1,
%%                            payload => <<>>}),

%%     SignedTx = aetx_sign:new(SpendTx, [<<0:(64*8)>>]),

%%     {ok, {key_block, KH}} = aec_chain:get_key_block_by_height(0),
%%     {ok, TopHash} = aec_headers:hash_header(KH),

%%     {Env, Trees} = aetx_env:tx_env_and_trees_from_hash('aetx_transaction', TopHash),

%%     AccountsTree = lists:foldl(fun aec_accounts_trees:enter/2,
%%                                aec_trees:accounts(Trees),
%%                                [SndAcc, RcvAcc]),

%%     {ok, _, _, TreesOut, _} =
%%         aec_trees:apply_txs_on_state_trees([SignedTx],
%%                                            aec_trees:set_accounts(Trees, AccountsTree),
%%                                            Env,
%%                                            [strict, dont_verify_signature]),

%%     {aec_accounts_trees:lookup(SndPK, aec_trees:accounts(TreesOut)),
%%      aec_accounts_trees:lookup(RcvPK, aec_trees:accounts(TreesOut))}.


%% x_contract() ->
%%     PK = fun (X) -> <<X/binary, (crypto:strong_rand_bytes(32 - size(X)))/binary>> end,

%%     CreatorPK = PK(<<"SND">>),
%%     CreatorAcc = aec_accounts:new(CreatorPK, 10000000000000000000000),

%%     %% {ok, ContractSource} =
%%     %%     file:read_file("/home/x/work/aeternity/aeternity/test/contracts/identity.aes"),
%%     %% {ok, ContractCode} = aect_sophia:compile(ContractSource, <<>>),

%%     {ok, #{contract_source := ContractSource} = ContractMap} =
%%         aeso_compiler:file("/home/x/work/aeternity/aeternity/apps/aestratum/contract/Payout.aes"),

%%     io:format("////////// COMPILED CONTRACT = ~p~n", [ContractMap]),

%%         %%aeso_compiler:file("/home/x/work/aeternity/aeternity/test/contracts/identity.aes"),

%%     {ok, CreateCallData, _, _} = aeso_compiler:create_calldata(ContractSource, "init", []),

%%     {ok, CreateTx} =
%%         aect_create_tx:new(#{owner_id    => aec_id:create(account, CreatorPK),
%%                              nonce       => 1,
%%                              code        => aect_sophia:serialize(ContractMap),
%%                              vm_version  => 16#03,
%%                              abi_version => 16#01,
%%                              deposit     => 0,
%%                              amount      => 1000000000000,
%%                              gas         => 10000000000,
%%                              gas_price   => 1000000,
%%                              call_data   => CreateCallData,
%%                              fee         => 100000000000}),

%%     ContractPK = aect_contracts:compute_contract_pubkey(CreatorPK, aetx:nonce(CreateTx)),
%%     io:format("////////// CONTRACT PK = ~p~n", [ContractPK]),

%%     %% io:format("////////// MIN FEE = ~p~n", [aetx:min_fee(CreateTx, 50000)]),

%%     SignedCreateTx = aetx_sign:new(CreateTx, [<<0:(64*8)>>]),

%%     {ok, {key_block, KH}} = aec_chain:get_key_block_by_height(50000),
%%     {ok, TopHash} = aec_headers:hash_header(KH),

%%     {Env, Trees} = aetx_env:tx_env_and_trees_from_hash(aetx_contract, TopHash),

%%     io:format("////////// ENV = ~p~n", [Env]),

%%     CallerPK = PK(<<"SND">>),
%%     CallerAcc = aec_accounts:new(CallerPK, 10000000000000000000000),

%%     {RcvPKs, RcvAccs} = lists:unzip(
%%                           [begin
%%                                RPK = PK(<<"BENEF">>),
%%                                {RPK, aec_accounts:new(RPK, 0)}
%%                            end || _ <- lists:seq(1, 20)]),


%%     AccountsTree = lists:foldl(fun aec_accounts_trees:enter/2,
%%                                aec_trees:accounts(Trees),
%%                                [CreatorAcc, CallerAcc | RcvAccs]),

%%     {ok, _, _, TreesOut1, _Events1} =
%%         aec_trees:apply_txs_on_state_trees([SignedCreateTx],
%%                                            aec_trees:set_accounts(Trees, AccountsTree),
%%                                            Env,
%%                                            [strict, dont_verify_signature]),

%%     %% io:format("////////// SENDER ACCOUNT = ~p~n",
%%     %%           [aec_accounts_trees:lookup(CreatorPK, aec_trees:accounts(TreesOut1))]).


%%     {ok, CallTx} = create_payout_call_tx([{RPK, 100} || RPK <- RcvPKs],
%%                                          CallerAcc,
%%                                          {ContractPK, ContractSource}),


%%     io:format("////////// CALL TX = ~p~n", [CallTx]),


%%     SignedCallTx = aetx_sign:new(CallTx, [<<0:(64*8)>>]),

%%     %% aec_tx_pool:push(SignedCallTx).


%%     CallEnv = aetx_env:set_context(Env, aetx_transaction), %% nasty


%%     %% dbg:tracer(),
%%     %% dbg:p(all, call),
%%     %% dbg:tpl(aevm_eeevm, eval, [{'_', [], [{return_trace}]}]),

%%     {ok, _, _, TreesOut2, _Events2} =
%%         aec_trees:apply_txs_on_state_trees([SignedCallTx],
%%                                            TreesOut1,
%%                                            CallEnv,
%%                                            [strict, dont_verify_signature]),

%%     %% dbg:stop_clear(),

%%     {CallCB, CallCTx} = aetx:specialize_callback(CallTx),

%%     io:format("////////// CallCB = ~p~n////////// CallCTx = ~p~n", [CallCB, CallCTx]),

%%     {value, CallObj} =
%%         aect_call_state_tree:lookup_call(CallCB:contract_pubkey(CallCTx),
%%                                          CallCB:call_id(CallCTx),
%%                                          aec_trees:calls(TreesOut2)),

%%     io:format("////////// RESULT = ~p [~p]~n",
%%               [aect_call:return_value(CallObj), aect_call:return_type(CallObj)]),

%%     {[aec_accounts_trees:lookup(CreatorPK, aec_trees:accounts(TreesOut2)),
%%       aec_accounts_trees:lookup(CallerPK, aec_trees:accounts(TreesOut2))],
%%      [aec_accounts_trees:lookup(RPK, aec_trees:accounts(TreesOut2)) || RPK <- RcvPKs]}.
