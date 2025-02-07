-module(hctest).

-import(aecore_suite_utils, [http_request/4, external_address/0, rpc/3, rpc/4]).

-export([
    account_balance/1,
    call_info/1,
    config/2,
    config/3,
    contract_call/6,
    contract_call/7,
    contract_call_spec/7,
    contract_create_spec/6,
    create_ae_spend_tx/4,
    decode_consensus_result/3,
    encoded_pubkey/1,
    epoch_info/1,
    epoch_info/2,
    external_address/1,
    format/2,
    get_block_producer/2,
    get_block_producer_name/2,
    get_entropy/2,
    get_height/1,
    get_generations/3,
    get_nodes/1,
    inspect_election_contract/3,
    inspect_staking_contract/3,
    inspect_validator/4,
    key_reward_provided/0,
    leaders_at_height/3,
    mine_cc_blocks/2,
    mine_key_blocks/2,
    mine_to_last_block_in_epoch/2,
    mine_to_next_epoch/2,
    next_nonce/2,
    privkey/1,
    produce_cc_blocks/2,
    produce_n_epochs/2,
    produce_pc_blocks/1,
    pubkey/1,
    read_last_blocks/4,
    seed_account/3,
    sign_and_push/4,
    sign_tx/3,
    spread/3,
    src/2,
    wait_and_sync/1,
    wait_same_top/1,
    wait_same_top/2,
    who_by_pubkey/1,
    with_saved_keys/2
]).

-include_lib("stdlib/include/assert.hrl").
-include("./hctest_defaults.hrl").

config(Key, CTConfig, Default) ->
    proplists:get_value(Key, CTConfig, Default).

%% Common Test documentation discourages use of ?config macro, so here we are
config(Key, CTConfig) ->
    Value = proplists:get_value(Key, CTConfig, '$undefined'),
    ?assertNotEqual(Value, '$undefined'),
    Value.

format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

%% FIXME: Possible improvement: use a nice map instead of a proplist of 4-tuples
% -type node_record() :: #{
%     node => node(), long_name => node(),
%     public_key => binary(), private_key => binary()
% }.
%-spec get_nodes(CTConfig :: proplists:proplist()) -> list(node_record()).
get_nodes(CTConfig) ->
    config(nodes, CTConfig, []).

pubkey({Pubkey, _, _}) -> Pubkey.

privkey({_, Privkey, _}) -> Privkey.

who_by_pubkey(Pubkey) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    BobSign = pubkey(?BOB_SIGN),
    Lisa = pubkey(?LISA),
    Dwight = pubkey(?DWIGHT),
    Edwin = pubkey(?EDWIN),
    Genesis = ?GENESIS_BENFICIARY,
    case Pubkey of
        Alice -> ?ALICE;
        Bob -> ?BOB;
        BobSign -> ?BOB_SIGN;
        Lisa -> ?LISA;
        Dwight -> ?DWIGHT;
        Edwin -> ?EDWIN;
        Genesis -> genesis;
        _  -> error(unknown_beneficiary)
    end.

next_nonce(Node, Pubkey) ->
    case rpc(Node, aec_next_nonce, pick_for_account, [Pubkey, max]) of
        {ok, NextNonce} -> NextNonce;
        {error, account_not_found} -> 1
    end.

sign_and_push(NodeName, Tx, Who, NetworkId) ->
    SignedTx = sign_tx(Tx, privkey(Who), NetworkId),
    ok = rpc:call(NodeName, aec_tx_pool, push, [SignedTx, tx_received]),
    SignedTx.

%% usually we would use aec_test_utils:sign_tx/3. This function is being
%% executed in the context of the CT test and uses the corresponding
%% network_id. Since the network_id of the HC node is different, we must sign
%% the tx using the test-specific network_id
sign_tx(Tx, Privkey, NetworkId) ->
    Bin0 = aetx:serialize_to_binary(Tx),
    Bin = aec_hash:hash(signed_tx, Bin0), %% since we are in CERES context, we sign th hash
    BinForNetwork = <<NetworkId/binary, Bin/binary>>,
    Signatures = [ enacl:sign_detached(BinForNetwork, Privkey)],
    aetx_sign:new(Tx, Signatures).

seed_account(RecipientPubkey, Amount, NetworkId) ->
    seed_account(?NODE1, RecipientPubkey, Amount, NetworkId).

seed_account(Node, RecipientPubkey, Amount, NetworkId) ->
    NodeName = aecore_suite_utils:node_name(Node),
    {PatronPriv, PatronPub} = aecore_suite_utils:sign_keys(Node),
    Nonce = next_nonce(Node, PatronPub),
    Params =
        #{sender_id    => aeser_id:create(account, PatronPub),
          recipient_id => aeser_id:create(account, RecipientPubkey),
          amount       => Amount,
          fee          => 30000 * ?DEFAULT_GAS_PRICE,
          nonce        => Nonce,
          payload      => <<>>},
    ct:log("Preparing a spend tx: ~p", [Params]),
    {ok, Tx} = aec_spend_tx:new(Params),
    SignedTx = sign_tx(Tx, PatronPriv, NetworkId),
    ok = rpc:call(NodeName, aec_tx_pool, push, [SignedTx, tx_received]),
    {ok, SignedTx}.

account_balance(Pubkey) ->
    case rpc(?NODE1, aec_chain, get_account, [Pubkey]) of
        {value, Account} -> aec_accounts:balance(Account);
        none -> no_such_account
    end.

inspect_validator(CtPubkey, Origin, What, Config) ->
    TopHash = rpc(?NODE1, aec_chain, top_block_hash, []),
    {Fun, Args} =
        case What of
            get_available_balance -> {"get_available_balance", []};
            get_total_balance     -> {"get_total_balance", []}
        end,
    do_contract_call(CtPubkey, src(?STAKING_VALIDATOR_CONTRACT, Config), Fun, Args, Origin, TopHash).

inspect_staking_contract(OriginWho, WhatToInspect, Config) ->
    TopHash = rpc(?NODE1, aec_chain, top_block_hash, []),
    inspect_staking_contract(OriginWho, WhatToInspect, Config, TopHash).

inspect_staking_contract(OriginWho, WhatToInspect, Config, TopHash) ->
    {Fun, Args} =
        case WhatToInspect of
            {staking_power, Who} ->
                {"staking_power", [binary_to_list(encoded_pubkey(Who))]};
            {get_validator_state, Who} ->
                {"get_validator_state", [binary_to_list(encoded_pubkey(Who))]};
            {get_validator_contract, Who} ->
                {"get_validator_contract", [binary_to_list(encoded_pubkey(Who))]};
            get_current_epoch ->
                {"get_current_epoch", []};
            get_state ->
                {"get_state", []};
            leaders ->
                {"sorted_validators", []}

        end,
    ContractPubkey = config(staking_contract, Config),
    do_contract_call(ContractPubkey, src(?MAIN_STAKING_CONTRACT, Config), Fun, Args, OriginWho, TopHash).

inspect_election_contract(OriginWho, WhatToInspect, Config) ->
    TopHash = rpc(?NODE1, aec_chain, top_block_hash, []),
    inspect_election_contract(OriginWho, WhatToInspect, Config, TopHash).

inspect_election_contract(OriginWho, WhatToInspect, Config, TopHash) ->
    {Fun, Args} =
        case WhatToInspect of
            current_added_staking_power -> {"added_stake", []};
            _ -> {WhatToInspect, []}
        end,
    ContractPubkey = config(election_contract, Config),
    do_contract_call(ContractPubkey, src(?HC_CONTRACT, Config), Fun, Args, OriginWho, TopHash).

do_contract_call(CtPubkey, CtSrc, Fun, Args, Who, TopHash) ->
    F = fun() -> do_contract_call_(CtPubkey, CtSrc, Fun, Args, Who, TopHash) end,
    {T, Res} = timer:tc(F),
    ct:log("Calling contract took ~.2f ms", [T / 1000]),
    Res.

do_contract_call_(CtPubkey, CtSrc, Fun, Args, Who, TopHash) ->
    Tx = contract_call(CtPubkey, CtSrc, Fun, Args, 0, pubkey(Who)),
    {ok, Call} = dry_run(TopHash, Tx),
    decode_consensus_result(Call, Fun, CtSrc).

dry_run(TopHash, Tx) ->
    case rpc(?NODE1, aec_dry_run, dry_run, [TopHash, [], [{tx, Tx}]]) of
        {error, _} = Err -> Err;
        {ok, {[{contract_call_tx, {ok, Call}}], _Events}} -> {ok, Call}
    end.

call_info(SignedTx) ->
    Hash = aetx_sign:hash(SignedTx),
    case rpc:call(?NODE1_NAME, aec_chain, find_tx_location, [Hash]) of
        not_found ->  {error, unknown_tx};
        none -> {error, gced_tx};
        mempool -> {error, tx_in_pool};
        MBHash when is_binary(MBHash) ->
            case rpc:call(?NODE1_NAME, aehttp_helpers, get_info_object_signed_tx,
                          [MBHash, SignedTx]) of
                {ok, Call} -> {ok, Call};
                {error, Reason} -> {error, Reason}
            end
    end.

create_ae_spend_tx(SenderId, RecipientId, Nonce, Payload) ->
    Params = #{sender_id => aeser_id:create(account, SenderId),
               recipient_id => aeser_id:create(account, RecipientId),
               amount => 1,
               nonce => Nonce,
               fee => 40000 * ?DEFAULT_GAS_PRICE,
               payload => Payload},
    ct:log("Preparing a spend tx: ~p", [Params]),
    aec_spend_tx:new(Params).

external_address(Node) ->
    {ok, Port} = rpc(Node, aeu_env, user_config_or_env,
                     [[<<"http">>, <<"external">>, <<"port">>], aehttp, [external, port]]),
   "http://127.0.0.1:" ++ integer_to_list(Port).


decode_consensus_result(Call, Fun, Src) ->
    ReturnType = aect_call:return_type(Call),
    ReturnValue = aect_call:return_value(Call),
    Res = aect_test_utils:decode_call_result(Src, Fun, ReturnType, ReturnValue),
    {ReturnType, Res}.

src(ContractName, Config) ->
    Srcs = config(contract_src, Config),
    maps:get(ContractName, Srcs).

produce_n_epochs(Config, N) ->
    [{Node1, _, _, _}|_] = get_nodes(Config),
    %% produce blocks
    {ok, Bs} = produce_cc_blocks(Config, #{count => N * ?CHILD_EPOCH_LENGTH}),
    %% check producers
    Producers = [ aec_blocks:miner(B) || B <- Bs, aec_blocks:is_key_block(B) ],
    ChildTopHeight = get_height(Node1),
    Leaders = leaders_at_height(Node1, ChildTopHeight, Config),
    ct:log("Bs: ~p  Leaders ~p", [Bs, Leaders]),
    %% Check that all producers are valid leaders
    ?assertEqual([], lists:usort(Producers) -- Leaders),
    %% If we have more than 1 leader, then we should see more than one producer
    %% at least for larger EPOCHs
    ?assert(length(Leaders) > 1, length(Producers) > 1),
    ParentTopHeight = get_height(?PARENT_CHAIN_NODE),
    {ok, ParentBlocks} = get_generations(?PARENT_CHAIN_NODE, 0, ParentTopHeight),
    ct:log("Parent chain blocks ~p", [ParentBlocks]),
    {ok, ChildBlocks} = get_generations(Node1, 0, ChildTopHeight),
    ct:log("Child chain blocks ~p", [ChildBlocks]),
    ok.

epoch_info(Node) ->
    rpc(Node, aec_chain_hc, epoch_info, []).

epoch_info(Node, Height) when is_integer(Height) ->
    rpc(Node, aec_chain_hc, epoch_info, [Height]).

%% Wait until child chain grows by a number of key blocks. Do not try to start or stop anything just observe.
%% `parent_produce` hints the production on parent chain, is a list of tuples {Height, ParentKeyBlocksCount}
-type produce_cc_args() :: #{
    count := non_neg_integer(), % optional
    target_height := non_neg_integer(), % optional, pass either this or count
    node => node(), % optional
    %% Skipping this value will create a default schedule for parent producing to interleave with child chain
    %% If you want parent chain to halt while child chain is producing, pass an empty list
    parent_produce => list({non_neg_integer(), non_neg_integer()}) % optional
}.
-spec produce_cc_blocks(Config :: proplists:proplist(), pos_integer() | produce_cc_args()) -> {ok, list(aec_blocks:block())}.
produce_cc_blocks(Config, BlocksCnt) when is_integer(BlocksCnt) ->
    produce_cc_blocks(Config, #{count => BlocksCnt});
%% Returns 2x BlocksCnt blocks because each production is 2 blocks: key and micro
produce_cc_blocks(Config, Args) ->
    %% Skip the node argument to pick the first node
    Node = case maps:get(node, Args, undefined) of
        undefined ->
            [{N1, _, _, _} | _] = get_nodes(Config),
            N1;
        N2 ->
            N2
    end,
    TopHeight = get_height(Node),
    {ok, #{
        epoch := Epoch,
        first := EpochFirst,
        last := EpochLast,
        length := EpochLength} = Info} = epoch_info(Node, TopHeight),
    ct:log("EpochInfo ~p", [Info]),

    %% Either count or target_height should be passed. Calculate one from another
    TargetHeight = case maps:get(count, Args, undefined) of
        undefined -> maps:get(target_height, Args);
        Count -> TopHeight + Count
    end,
    BlocksCnt = TargetHeight - TopHeight,

    %% At end of BlocksCnt child epoch approaches approx:
    CBAfterEpoch = BlocksCnt - (EpochLast - TopHeight),
    ScheduleUpto = Epoch + 1 + (CBAfterEpoch div EpochLength),
    ParentTopHeight = get_height(?PARENT_CHAIN_NODE),
    ct:log("parent_height=~p child_height=~p for next ~p child blocks", [ParentTopHeight, TopHeight,  BlocksCnt]),

    %% Pairs of {height, BlockCount} to produce on parent
    ParentProduce = case maps:get(parent_produce, Args, undefined) of
        undefined ->
            %% Spread parent blocks over BlocksCnt
            lists:append([ spread(?PARENT_EPOCH_LENGTH, TopHeight,
                              [ {CH, 0} || CH <- lists:seq(EpochFirst + E * EpochLength, EpochLast + E * EpochLength)]) ||
                       E <- lists:seq(0, ScheduleUpto - Epoch) ]);
        PP ->
            PP
    end,
    %% Last parameter steers where in Child epoch parent block is produced
    % produce_cc_blocks(Config, BlocksCnt, ParentProduce).

    %% Wait max 3 child_block_times (plus BlockCnt block times) for the chain top to progress
    rpc(Node, aec_conductor, start_mining, []),
    WaitBlockIntervals = BlocksCnt + 3, % wait for count + a little extra (given that the CC sometimes stops)
    ok = produce_cc_wait_until(
        Node, ParentProduce, TargetHeight, 5 * WaitBlockIntervals, ?CHILD_BLOCK_TIME div 5),

    %% Read last BlocksCnt blocks
    ?assert(get_height(Node) >= BlocksCnt,
        format("Chain must have at least ~w blocks (has ~w)", [BlocksCnt, get_height(Node)])),
    ProducedCount = get_height(Node) - TopHeight,
    ct:log("Produced till height=~w (requested_count=~w), reading last_produced=~w blocks",
        [TargetHeight, BlocksCnt, ProducedCount]),
    LastBlocks = read_last_blocks(
        Node, rpc(Node, aec_chain, top_block_hash, []), ProducedCount, []),
    {ok, LastBlocks}.

%% Read remotely the last BlocksCnt key blocks from the top
read_last_blocks(_Node, _AtHash, 0, Accum) -> Accum;
read_last_blocks(Node, AtHash, N, Accum) ->
    {ok, Block} = rpc(Node, aec_chain, get_block, [AtHash]),
    AtHash1 = rpc(Node, aec_blocks, prev_hash, [Block]),
    read_last_blocks(Node, AtHash1, N - 1, [Block | Accum]).

produce_cc_wait_until(_Node, _ParentProduce, _TargetHeight, 0, _) ->
    erlang:error(timeout_waiting_for_block);
produce_cc_wait_until(Node, ParentProduce, TargetHeight, Attempts, SleepTime) ->
    case get_height(Node) of
        H1 when H1 > TargetHeight ->
            ok;
        H1 when H1 =:= TargetHeight ->
            %% Sleep a little extra to allow sync to happen when this is produced on another node
            timer:sleep(SleepTime),
            ok;
        H2 ->
            timer:sleep(SleepTime),
            NewParentProduce = case ParentProduce of
                [{CH, PBs} | PRest] when CH == H2+1 ->
                    ct:log("parent_produce: Producing on parent height=~w count=~w", [CH, PBs]),
                    produce_pc_blocks(PBs),
                    PRest;
                PP -> PP
            end,

            produce_cc_wait_until(Node, NewParentProduce, TargetHeight, Attempts - 1, SleepTime)
    end.

produce_pc_blocks(Count) ->
    mine_key_blocks(?PARENT_CHAIN_NODE_NAME, Count).

% produce_cc_blocks(Config, BlocksCnt, ParentProduce) ->
%     [{Node1, _, _, _} | _] = get_nodes(Config),
%     %% The previous production ended with wait_same_top, so asking first node is sufficient
%     TopHeight = get_height(Node1),
%     %% assert that the parent chain is not mining
%     ?assertEqual(stopped, rpc:call(?PARENT_CHAIN_NODE_NAME, aec_conductor, get_mining_state, [])),
%     ct:log("parent produce ~p", [ParentProduce]),
%     NewTopHeight = produce_to_cc_height(Config, TopHeight, TopHeight + BlocksCnt, ParentProduce),
%     wait_same_top([ Node || {Node, _, _, _} <- get_nodes(Config)]),
%     get_generations(Node1, TopHeight + 1, NewTopHeight).

wait_same_top(Nodes) ->
    wait_same_top(Nodes, 3).

wait_same_top(_Nodes, Attempts) when Attempts < 1 ->
    %% {error, run_out_of_attempts};
    throw({error, run_out_of_attempts});
wait_same_top(Nodes, Attempts) ->
    KBs = [ rpc(Node, aec_chain, top_block, []) || Node <- Nodes ],
    case lists:usort(KBs) of
        [KB] -> {ok, KB};
        Diffs ->
            ct:log("Nodes differ: ~p", [Diffs]),
            timer:sleep(?CHILD_BLOCK_TIME div 2),
            wait_same_top(Nodes, Attempts - 1)
    end.

wait_and_sync(Config) ->
    Nodes = get_nodes(Config),
    {ok, _KB} = wait_same_top(Nodes, 3),
    ok.

%% It seems we automatically produce child chain blocks in the background
% produce_to_cc_height(Config, TopHeight, GoalHeight, ParentProduce) ->
%     NodeNames = [ Name || {_, Name, _, _} <- get_nodes(Config) ],
%     BlocksNeeded = GoalHeight - TopHeight,
%     case BlocksNeeded > 0 of
%         false ->
%             TopHeight;
%         true ->
%             NewParentProduce =
%                 case ParentProduce of
%                     [{CH, PBs} | PRest ] when CH == TopHeight+1 ->
%                         mine_key_blocks(?PARENT_CHAIN_NODE_NAME, PBs),
%                         PRest;
%                     PP -> PP
%                 end,

%             %% TODO: add some assertions when we expect an MB (and not)!
%             {ok, _Txs} = rpc:call(hd(NodeNames), aec_tx_pool, peek, [infinity]),

%             %% This will mine 1 key-block (and 0 or 1 micro-blocks)
%             {ok, Blocks} = mine_cc_blocks(NodeNames, 1),

%             {Node, KeyBlock} = lists:last(Blocks),
%             case Blocks of
%                 [{Node, MB}, _] ->
%                     ?assertEqual(micro, aec_blocks:type(MB)),
%                     ct:log("CC ~p produced micro-block: ~p", [Node, MB]);
%                 [_] ->
%                     ok
%             end,
%             ?assertEqual(key, aec_blocks:type(KeyBlock)),
%             ct:log("CC ~p produced key-block: ~p", [Node, KeyBlock]),

%             Producer = get_block_producer_name(config(staker_names, Config), KeyBlock),
%             ct:log("~p produced CC block at height ~p", [Producer, aec_blocks:height(KeyBlock)]),
%             produce_to_cc_height(Config, TopHeight + 1, GoalHeight, NewParentProduce)
%       end.

mine_cc_blocks(NodeNames, N) ->
    aecore_suite_utils:hc_mine_blocks(NodeNames, N).

get_generations(Node, FromHeight, ToHeight) ->
    ReversedBlocks =
        lists:foldl(
            fun(Height, Accum) ->
                case rpc(Node, aec_chain, get_generation_by_height, [Height, backward]) of
                    {ok, #{key_block := KB, micro_blocks := MBs}} ->
                        ReversedGeneration = lists:reverse(MBs) ++ [KB],
                        ReversedGeneration ++ Accum;
                    error -> error({failed_to_fetch_generation, Height})
                end
            end,
            [],
            lists:seq(FromHeight, ToHeight)),
    {ok, lists:reverse(ReversedBlocks)}.

mine_key_blocks(ParentNodeName, NumParentBlocks) ->
    {ok, _} = aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(ParentNodeName),
    {ok, KBs} = aecore_suite_utils:mine_key_blocks(ParentNodeName, NumParentBlocks),
    ct:log("Parent block mined ~p ~p number: ~p", [KBs, ParentNodeName, NumParentBlocks]),
    {ok, KBs}.

get_block_producer_name(Parties, Block) ->
    Producer = aec_blocks:miner(Block),
    case lists:keyfind(Producer, 1, Parties) of
        false -> Producer;
        {_, _, Name} -> Name
    end.

get_block_producer(Node, Height) ->
    {ok, KeyHeader} = rpc(Node, aec_chain, get_key_header_by_height, [Height]),
    aec_headers:miner(KeyHeader).

get_height(Node) ->
    aecore_suite_utils:rpc(Node, aec_chain, top_height, []).

leaders_at_height(Node, Height, Config) ->
    {ok, Hash} = rpc(Node, aec_chain_state, get_key_block_hash_at_height, [Height]),
    {ok, Return} = inspect_staking_contract(?ALICE, leaders, Config, Hash),
    [ begin
        {account_pubkey, K} = aeser_api_encoder:decode(LeaderKey), K
      end || [ LeaderKey, _LeaderStake] <- Return ].

key_reward_provided() ->
    TopHeight = get_height(?NODE1),
    RewardHeight = TopHeight - ?REWARD_DELAY,
    key_reward_provided(RewardHeight).

key_reward_provided(RewardHeight) ->
  {get_block_producer(?NODE1, RewardHeight),
   rpc(?NODE1, aec_governance, block_mine_reward, [RewardHeight])}.

spread(_, _, []) ->
    [];
spread(0, TopHeight, Spread) ->
    [ {CH, N} || {CH, N} <- Spread, N /= 0, CH > TopHeight ];
%spread(N, TopHeight, [{CH, K} | Spread]) when length(Spread) < N ->
%    %% Take speed first (not realistic), then fill rest
%    spread(0, TopHeight, [{CH, K + N - length(Spread)} | [ {CH2, X+1} || {CH2, X} <- Spread]]);
spread(N, TopHeight, Spread) when N rem 2 == 0 ->
    {Left, Right} = lists:split(length(Spread) div 2, Spread),
    spread(N div 2, TopHeight, Left) ++ spread(N div 2, TopHeight, Right);
spread(N, TopHeight, Spread) when N rem 2 == 1 ->
    {Left, [{Middle, K} | Right]} = lists:split(length(Spread) div 2, Spread),
    spread(N div 2, TopHeight, Left) ++ [{Middle, K+1} || Middle > TopHeight] ++ spread(N div 2, TopHeight, Right).

get_entropy(Node, Epoch) ->
    ParentHeight = rpc(Node, aec_consensus_hc, entropy_height, [Epoch]),
    {ok, WPHdr}  = rpc(?PARENT_CHAIN_NODE, aec_chain, get_key_header_by_height, [ParentHeight]),
    {ok, WPHash0} = aec_headers:hash_header(WPHdr),
    {ParentHeight, aeser_api_encoder:encode(key_block_hash, WPHash0)}.

encoded_pubkey(Who) ->
    aeser_api_encoder:encode(account_pubkey, pubkey(Who)).

contract_call_spec(ContractPubkey, Src, Fun, Args, Amount, From, Nonce) ->
    {contract_call_tx, CallTx} =
        aetx:specialize_type(contract_call(ContractPubkey, Src, Fun, Args, Amount, From, Nonce)),
    %% Don't allow named contracts!?
    {contract, ContractPubKey} =
        aeser_id:specialize(aect_call_tx:contract_id(CallTx)),
    Spec =
        #{  <<"caller">>          => aeser_api_encoder:encode(account_pubkey,
                                                                aect_call_tx:caller_pubkey(CallTx))
            , <<"nonce">>           => aect_call_tx:nonce(CallTx)
            , <<"contract_pubkey">> => aeser_api_encoder:encode(contract_pubkey, ContractPubKey)
            , <<"abi_version">>     => aect_call_tx:abi_version(CallTx)
            , <<"fee">>             => aect_call_tx:fee(CallTx)
            , <<"amount">>          => aect_call_tx:amount(CallTx)
            , <<"gas">>             => aect_call_tx:gas(CallTx)
            , <<"gas_price">>       => aect_call_tx:gas_price(CallTx)
            , <<"call_data">>       => aeser_api_encoder:encode(contract_bytearray,
                                                                aect_call_tx:call_data(CallTx))},
    Spec.

contract_call(ContractPubkey, Src, Fun, Args, Amount, From) ->
    Nonce = next_nonce(?NODE1, From), %% no contract calls support for parent chain
    contract_call(ContractPubkey, Src, Fun, Args, Amount, From, Nonce).

contract_call(ContractPubkey, Src, Fun, Args, Amount, From, Nonce) ->
    {ok, CallData} = aect_test_utils:encode_call_data(Src, Fun, Args),
    ABI = aect_test_utils:abi_version(),
    TxSpec =
        #{  caller_id   => aeser_id:create(account, From)
            , nonce       => Nonce
            , contract_id => aeser_id:create(contract, ContractPubkey)
            , abi_version => ABI
            , fee         => 1000000 * ?DEFAULT_GAS_PRICE
            , amount      => Amount
            , gas         => 1000000
            , gas_price   => ?DEFAULT_GAS_PRICE
            , call_data   => CallData},
    {ok, Tx} = aect_call_tx:new(TxSpec),
    Tx.

with_saved_keys(Keys, Config) ->
    {_TC, SavedConfig} = config(saved_config, Config),
    lists:foldl(fun(Key, Conf) ->
                    case proplists:get_value(Key, SavedConfig) of
                        undefined -> Conf;
                        Val -> [{Key, Val} | Conf]
                    end
                end,
                lists:keydelete(saved_config, 1, Config), Keys).

mine_to_next_epoch(Node, Config) ->
    Height1 = get_height(Node),
    {ok, #{last := Last1, length := _Len}} = epoch_info(Node),
    {ok, Bs} = produce_cc_blocks(Config, #{count => Last1 - Height1 + 1}),
    ct:log("Block last epoch: ~p", [Bs]).

mine_to_last_block_in_epoch(Node, Config) ->
    {ok, #{epoch  := _Epoch,
            first  := _First,
            last   := Last,
            length := _Length}} = epoch_info(Node),
    CH = get_height(Node),
    DistToBeforeLast = Last - CH - 1,
    {ok, _} = produce_cc_blocks(Config, #{count => DistToBeforeLast}).

contract_create_spec(Name, Src, Args, Amount, Nonce, Owner) ->
    {ok, Code}   = aect_test_utils:compile_contract(aect_test_utils:sophia_version(), Name),
    Pubkey = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    EncodedPubkey   = aeser_api_encoder:encode(contract_pubkey, Pubkey),
    EncodedOwner    = aeser_api_encoder:encode(account_pubkey, Owner),
    EncodedCode     = aeser_api_encoder:encode(contract_bytearray, Code),
    {ok, CallData}  = aect_test_utils:encode_call_data(Src, "init", Args),
    EncodedCallData = aeser_api_encoder:encode(contract_bytearray, CallData),
    VM = aect_test_utils:vm_version(),
    ABI = aect_test_utils:abi_version(),
    Spec = #{ <<"amount">> => Amount
            , <<"vm_version">> => VM
            , <<"abi_version">> => ABI
            , <<"nonce">> => Nonce
            , <<"code">> => EncodedCode
            , <<"call_data">> => EncodedCallData
            , <<"pubkey">> => EncodedPubkey
            , <<"owner_pubkey">> => EncodedOwner },
    Spec.
