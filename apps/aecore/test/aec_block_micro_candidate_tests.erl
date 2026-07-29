%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_block_{key,micro}_candidate module
%%% @end
%%%=============================================================================
-module(aec_block_micro_candidate_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-define(TEST_PUB, <<181,14,1,149,98,104,64,190,242,52,152,159,190,216,30,49,
                    94,251,20,75,9,85,29,82,35,178,98,75,188,72,242,141>>).
-define(TEST_PRIV, <<220,220,95,208,13,103,52,54,220,60,93,149,153,25,95,67,
                     178,143,191,176,251,107,170,15,223,140,13,57,96,171,79,
                     43,181,14,1,149,98,104,64,190,242,52,152,159,190,216,30,
                     49,94,251,20,75,9,85,29,82,35,178,98,75,188,72,242,141>>).
-define(TEST_ID, aeser_id:create(account, ?TEST_PUB)).

%% What the pool reports when it stopped because it ran out of transactions to
%% offer, rather than because it hit the deadline it was given.
-define(DRAINED, #{expired => false}).

%% Mirrors the schema default of mining.micro_block_candidate_timeout, so that
%% changing a documented default has to be a deliberate edit here too.
-define(DEFAULT_CANDIDATE_TIMEOUT, 1000).

-define(PAYER_PUB, <<177,181,119,188,211,39,203,57,229,94,108,2,107,214, 167,74,27,
                    53,222,108,6,80,196,174,81,239,171,117,158,65,91,102>>).
-define(PAYER_PRIV, <<145,69,14,254,5,22,194,68,118,57,0,134,66,96,8,20,124,253,238,
                    207,230,147,95,173,161,192,86,195,165,186,115,251,177,181,119,
                    188,211,39,203,57,229,94,108,2,107,214,167,74,27,53,222,108,6,
                    80,196,174,81,239,171,117,158,65,91,102>>).

block_extension_test_() ->
    {foreach,
      fun() ->
        meck:new(aeu_time, [passthrough]),
        meck:new(aec_chain, [passthrough]),
        meck:new(aec_db, [passthrough]),
        meck:new(aec_keys, [passthrough]),
        meck:new(aec_tx_pool, [passthrough]),
        meck:new(aec_trees, [passthrough])
      end,
      fun(_) ->
        %% Unset here rather than in the test bodies: eunit kills the test
        %% process on a {timeout, _} expiry, which is exactly the failure mode
        %% the deadline tests below guard against, and an `after` clause in a
        %% test body would not run - leaking the setting into every later test.
        application:unset_env(aecore, micro_block_candidate_timeout),
        application:unset_env(aeutils, '$user_map'),
        meck:unload(aec_trees),
        meck:unload(aec_tx_pool),
        meck:unload(aec_keys),
        meck:unload(aec_chain),
        meck:unload(aec_db),
        meck:unload(aeu_time)
      end,
      [{"Generate a block in one step, compared with two steps, with a spend tx",
        fun() ->
          Height = 10,
          Protocol = aec_hard_forks:protocol_effective_at_height(Height),
          AccMap = #{ preset_accounts => [{?TEST_PUB, 100000 * aec_test_utils:min_gas_price()}] },
          {Block0, Trees0} = aec_block_genesis:genesis_block_with_state(AccMap),
          STx = aec_test_utils:sign_tx(spend_tx(#{}, Trees0), ?TEST_PRIV),


          meck:expect(aeu_time, now_in_msecs, 0, 1234567890),
          meck:expect(aec_chain, get_block_state, 1, {ok, Trees0}),
          meck_tx_pool_get_candidate([STx]),
          meck:expect(aec_keys, get_pubkey, 0, {ok, ?TEST_PUB}),
          meck:expect(aec_db, find_discovered_pof, 1, none),
          {ok, Block1A, #{ trees := Trees1A }} = aec_block_micro_candidate:create(Block0),

          meck_tx_pool_get_candidate([]),
          {ok, Block1B0, BInfo} = aec_block_micro_candidate:create(Block0),

          {ok, Block1B, #{ trees := Trees1A }} =
                aec_block_micro_candidate:update(Block1B0, [STx], BInfo),

          ?assertEqual(Block1A, Block1B),
          16660 = aetx:used_gas(aetx_sign:tx(STx), Height, Protocol, Trees1A),
          ok
        end},
       {"Generate a block in one step, compared with two steps, with contract calls (and a spend tx)",
        fun() ->
          GasPrice = 3,
          GasUsed = 7,
          Caller = <<"caller_address........(32 bytes)">>,
          Contract = <<"contract_address......(32 bytes)">>,
          Call = aect_call:set_gas_used(
                   GasUsed,
                   aect_call:new(
                     aeser_id:create(account, Caller),
                     _CallerNonce = 1,
                     aeser_id:create(contract, Contract),
                     _BlockHeight = 42,
                     GasPrice)),

          AccMap = #{ preset_accounts => [{?TEST_PUB, 100000 * aec_test_utils:min_gas_price()}] },
          {Block0, Trees0} = aec_block_genesis:genesis_block_with_state(AccMap),

          STx = aec_test_utils:sign_tx(spend_tx(#{}, Trees0), ?TEST_PRIV),


          meck:expect(aeu_time, now_in_msecs, 0, 1234567890),
          meck:expect(aec_chain, get_block_state, 1, {ok, Trees0}),
          meck_tx_pool_get_candidate([STx]),
          meck:expect(aec_keys, get_pubkey, 0, {ok, ?TEST_PUB}),
          meck:expect(aec_db, find_discovered_pof, 1, none),

          {ok,_Block1A, #{ trees := Trees1A }} = aec_block_micro_candidate:create(Block0),

          %% Amend call state tree, in order not to require calling
          %% actual contract that would make this unit test
          %% unnecessary complex.
          meck:expect(aec_trees, apply_txs_on_state_trees,
                      fun(STxs, Trees, Env) ->
                              {ok, STxs, [], NewTrees, Events} =
                                  meck:passthrough([STxs, Trees, Env]),
                              case lists:member(STx, STxs) of
                                  false -> {ok, [], STxs, NewTrees, Events};
                                  true ->
                                      NewTreesWithCall =
                                          aec_trees:set_calls(
                                            NewTrees,
                                            aect_call_state_tree:insert_call(
                                              Call,
                                              aec_trees:calls(NewTrees))),
                                      {ok, STxs, [], NewTreesWithCall, Events}
                              end
                      end),

          {ok, Block1B, #{ trees := Trees1B }} = aec_block_micro_candidate:create(Block0),

          ?assertEqual(get_miner_account_balance(Trees1A), %% NG: Not yet + GasUsed * GasPrice,
                       get_miner_account_balance(Trees1B)),

          meck_tx_pool_get_candidate([]),
          {ok, Block1C0, BInfo} = aec_block_micro_candidate:create(Block0),

          {ok, Block1C, #{ trees := _Trees1B }} =
                aec_block_micro_candidate:update(Block1C0, [STx], BInfo),

          ?assertEqual(Block1B, Block1C)
        end},
       {"Updating a block does not exceed microblock gas limit",
        fun() ->
            Height = 10,
            Protocol = aec_hard_forks:protocol_effective_at_height(Height),
            SpendTx = fun(Data) -> aec_test_utils:sign_tx(spend_tx(Data), ?TEST_PRIV) end,
            Gas = fun(Txs) -> lists:sum(
                                lists:map(
                                  fun(T) -> aetx:gas_limit(aetx_sign:tx(T), Height, Protocol) end, Txs))
                  end,

            %% Reduce number of spend txs necessary for filling up block
            %% (hence reduce time necessary for running test)
            %% by computing spend tx payload roughly as expensive as 10 spend txs.
            SmallSpendTx = SpendTx(#{nonce => 0, payload => <<>>}),
            PayloadSize = 10 * trunc(Gas([SmallSpendTx]) / aec_governance:byte_gas()),
            Payload = << <<0>> || _ <- lists:seq(1, PayloadSize) >>,

            Tx = fun(N) -> SpendTx(#{nonce   => N,
                                     payload => Payload,
                                     fee     => 20 * aetx:min_fee(aetx_sign:tx(SmallSpendTx), Height, Protocol)}) end,

            %% Compute txs for filling up block.
            MaxGas = aec_governance:block_gas_limit(),
            {ExceedingTx, FillingTxs = [_|_]} =
                (fun F(Nonce, TxsAccIn) ->
                    {true, _} = {Gas(TxsAccIn) =< MaxGas, {TxsAccIn, MaxGas}}, %% Hardcoded expectation - for readability.
                    ThisTx = Tx(Nonce),
                    NextNonce = 1 + Nonce,
                    TxsAccOutTmp = TxsAccIn ++ [ThisTx],
                    case Gas(TxsAccOutTmp) =< MaxGas of
                        false ->
                            {ThisTx, TxsAccIn};
                        true ->
                            F(NextNonce, TxsAccOutTmp)
                    end
                 end)(1, []),
            ?assertMatch(X when X =< MaxGas, Gas(FillingTxs)), %% Hardcoded expectation - for readability.
            ?assertMatch(X when X  > MaxGas, Gas(FillingTxs ++ [ExceedingTx])), %% Hardcoded expectation - for readability.

            AccMap = #{ preset_accounts => [{?TEST_PUB, 100000000000000000000000000000000}] },
            {Block0, Trees0} = aec_block_genesis:genesis_block_with_state(AccMap),
            meck:expect(aec_chain, get_block_state, 1, {ok, Trees0}),
            meck:expect(aec_keys, get_pubkey, 0, {ok, ?TEST_PUB}),
            meck:expect(aec_db, find_discovered_pof, 1, none),

            %% Create full block, then check that attempting to add one tx fails.
            meck_tx_pool_get_candidate(FillingTxs),
            {ok, FullBlock, FullBlockInfo} = aec_block_micro_candidate:create(Block0),
            ?assertEqual(FillingTxs, aec_blocks:txs(FullBlock)), %% Hardcoded expectation - in case any txs discarded for e.g. insufficient funds.
            ?assertEqual(
                %% The total gas of the filling txs is deterministic.
                %% If it equals block gas limit,
                %% this assertion will fail with other error.
                %% If it happens, tune test
                %% in order to misalign total gas of txs with block gas limit
                %% e.g. put additional payload to all spend txs.
                {error, no_update_to_block_candidate},
                aec_block_micro_candidate:update(FullBlock, [ExceedingTx], FullBlockInfo)),
            ok
        end},
       {"Packing gives up at its deadline when no selected tx can be applied",
        {timeout, 30,
         fun() ->
            Block0 = setup_deadline_mecks(),

            %% A transaction the pool keeps offering but that can never be
            %% applied. It consumes no gas, so the refill loop never reduces
            %% the budget and would only stop once the pool stopped offering.
            BadTx = aec_test_utils:sign_tx(spend_tx(#{nonce => 100}), ?TEST_PRIV),

            meck:expect(aec_tx_pool, get_candidate,
                        fun(_, _, _, _) -> {ok, [BadTx], ?DRAINED} end),
            meck:expect(aec_tx_pool, failed_txs, 1, ok),

            %% Long enough that a stalled CI box cannot spend it inside the
            %% first pass, which has to complete for the assertions below.
            Timeout = 200,
            application:set_env(aecore, micro_block_candidate_timeout, Timeout),
            Before = erlang:monotonic_time(millisecond),
            {ok, Block, _BlockInfo} = aec_block_micro_candidate:create(Block0),
            Elapsed = erlang:monotonic_time(millisecond) - Before,

            %% Nothing applied, so there is nothing to publish.
            ?assertEqual([], aec_blocks:txs(Block)),
            %% The loop really did run until the deadline, and really did make
            %% more than one attempt - a build that gave up before the first
            %% pass would satisfy the assertion above just as well.
            ?assert(Elapsed >= Timeout),
            ?assert(meck:num_calls(aec_tx_pool, get_candidate,
                                   ['_', '_', '_', '_']) > 1),

            %% Every pass is handed the same deadline, computed once per build.
            Deadlines = candidate_deadlines(),
            ?assertMatch([_|_], Deadlines),
            [ ?assert(is_integer(D)) || D <- Deadlines ],
            ?assertEqual(1, length(lists:usort(Deadlines)))
         end}},
       {"A candidate timeout of 0 restores unbounded packing",
        {timeout, 30,
         fun() ->
            Block0 = setup_deadline_mecks(),
            BadTx = aec_test_utils:sign_tx(spend_tx(#{nonce => 100}), ?TEST_PRIV),

            meck:expect(aec_tx_pool, failed_txs, 1, ok),

            %% With no deadline the pool alone decides when to stop, so make it
            %% offer the unapplicable tx a fixed number of times. Each of those
            %% passes applies nothing and consumes no gas, so a deadline - or a
            %% cap on passes - is the only thing that could cut the loop short.
            Passes = 25,
            Counter = ets:new(candidate_passes, [public]),
            ets:insert(Counter, {passes, 0}),
            meck:expect(aec_tx_pool, get_candidate,
                        fun(_, _, _, _) ->
                            case ets:update_counter(Counter, passes, 1) of
                                N when N =< Passes -> {ok, [BadTx], ?DRAINED};
                                _                  -> {ok, [], ?DRAINED}
                            end
                        end),

            application:set_env(aecore, micro_block_candidate_timeout, 0),
            {ok, Block, _BlockInfo} = aec_block_micro_candidate:create(Block0),

            ?assertEqual([], aec_blocks:txs(Block)),
            ?assertEqual(Passes + 1, ets:lookup_element(Counter, passes, 2)),
            ets:delete(Counter)
         end}},
       {"A build stopped by its deadline publishes what it packed",
        {timeout, 30,
         fun() ->
            Block0 = setup_deadline_mecks(),
            GoodTx = aec_test_utils:sign_tx(spend_tx(#{nonce => 1}), ?TEST_PRIV),
            BadTx  = aec_test_utils:sign_tx(spend_tx(#{nonce => 100}), ?TEST_PRIV),

            meck:expect(aec_tx_pool, failed_txs, 1, ok),
            %% The first pass offers something applicable, every later pass
            %% does not - so the build is truncated with one tx already packed.
            meck:expect(aec_tx_pool, get_candidate,
                        fun(_, Packed, _, _) when map_size(Packed) =:= 0 ->
                                {ok, [GoodTx], ?DRAINED};
                           (_, _, _, _) ->
                                {ok, [BadTx], ?DRAINED}
                        end),

            application:set_env(aecore, micro_block_candidate_timeout, 200),
            {ok, Block, BlockInfo} = aec_block_micro_candidate:create(Block0),

            %% What was packed before the deadline survives...
            ?assertEqual([GoodTx], aec_blocks:txs(Block)),
            %% ...and the truncated candidate is consistent: its state hash is
            %% the hash of the trees that packing actually produced, and the
            %% block info it returns can still be extended.
            ?assertEqual(aec_trees:hash(maps:get(trees, BlockInfo)),
                         aec_blocks:root_hash(Block)),
            NextTx = aec_test_utils:sign_tx(spend_tx(#{nonce => 2}), ?TEST_PRIV),
            ?assertMatch({ok, _, _},
                         aec_block_micro_candidate:update(Block, [NextTx], BlockInfo))
         end}},
       {"Transactions and events from several packing passes keep their order",
        fun() ->
            Block0 = setup_deadline_mecks(),
            [STx1, STx2, STx3] =
                [ aec_test_utils:sign_tx(spend_tx(#{nonce => N}), ?TEST_PRIV)
                  || N <- [1, 2, 3] ],

            %% One transaction per pass, so the block is assembled out of three
            %% separate batches rather than in a single pass.
            meck:expect(aec_tx_pool, get_candidate,
                        fun(_, Packed, _, _) ->
                            case map_size(Packed) of
                                0 -> {ok, [STx1], ?DRAINED};
                                1 -> {ok, [STx2], ?DRAINED};
                                2 -> {ok, [STx3], ?DRAINED};
                                _ -> {ok, [], ?DRAINED}
                            end
                        end),
            %% Tag each applied tx with an event, so the event accumulator is
            %% held to the same ordering as the transaction accumulator. Both
            %% are collected per pass in reverse and joined once at the end.
            meck:expect(aec_trees, apply_txs_on_state_trees,
                        fun(Txs, Trees, _Env) ->
                            {ok, Txs, [], Trees, [ {applied, T} || T <- Txs ]}
                        end),

            {ok, Block, #{tx_env := TxEnv}} = aec_block_micro_candidate:create(Block0),

            ?assertEqual([STx1, STx2, STx3], aec_blocks:txs(Block)),
            ?assertEqual([{applied, STx1}, {applied, STx2}, {applied, STx3}],
                         aetx_env:events(TxEnv))
        end},
       {"The candidate timeout is read from configuration, and defaults",
        fun() ->
            Block0 = setup_deadline_mecks(),
            %% Nothing to pack, so a build is one pass long and the deadline it
            %% handed the pool is all that is under test.
            meck_tx_pool_get_candidate([]),

            %% Unset. This is what the mining loop runs with, because
            %% aec_block_generator builds its candidates through create/1.
            Before = erlang:monotonic_time(millisecond),
            {ok, _, _} = aec_block_micro_candidate:create(Block0),
            [Default] = candidate_deadlines(),
            ?assert(Default - Before >= ?DEFAULT_CANDIDATE_TIMEOUT),
            ?assert(Default - Before < ?DEFAULT_CANDIDATE_TIMEOUT * 2),

            %% Set, through the user configuration rather than the application
            %% environment: this is the key the schema defines, and reading a
            %% mistyped path would fall back to the default unnoticed.
            Configured = ?DEFAULT_CANDIDATE_TIMEOUT div 4,
            application:set_env(
              aeutils, '$user_map',
              #{<<"mining">> => #{<<"micro_block_candidate_timeout">> => Configured}}),
            meck:reset(aec_tx_pool),
            Before1 = erlang:monotonic_time(millisecond),
            {ok, _, _} = aec_block_micro_candidate:create(Block0),
            [FromConfig] = candidate_deadlines(),
            ?assert(FromConfig - Before1 >= Configured),
            ?assert(FromConfig - Before1 < ?DEFAULT_CANDIDATE_TIMEOUT)
        end},
       {"Only a build the deadline actually truncated is reported",
        %% The metrics mock belongs to this test alone: every mock costs a
        %% load-and-purge of a cover-compiled module, and the fixture above is
        %% instantiated once per test.
        {setup,
         fun() -> meck:new(aec_metrics, [passthrough]) end,
         fun(_) -> meck:unload(aec_metrics) end,
         fun() ->
            Block0 = setup_deadline_mecks(),
            Expired = [ae,epoch,aecore,tx_pool,candidate,expired],
            %% Far enough ahead that packing cannot reach it: whether the build
            %% counts as truncated is then entirely the pool's answer.
            application:set_env(aecore, micro_block_candidate_timeout, 60000),

            %% Selection can stop between select chunks with nothing selected,
            %% which is indistinguishable from an empty pool by the block alone.
            meck:expect(aec_tx_pool, get_candidate,
                        fun(_, _, _, _) -> {ok, [], #{expired => true}} end),
            {ok, Truncated, _} = aec_block_micro_candidate:create(Block0),
            ?assertEqual([], aec_blocks:txs(Truncated)),
            ?assert(meck:called(aec_metrics, try_update, [Expired, 1])),

            %% A pool that simply has nothing left is how every healthy build
            %% ends, and must not be reported as a truncated one.
            meck:reset(aec_metrics),
            meck:expect(aec_tx_pool, get_candidate,
                        fun(_, _, _, _) -> {ok, [], ?DRAINED} end),
            {ok, Drained, _} = aec_block_micro_candidate:create(Block0),
            ?assertEqual([], aec_blocks:txs(Drained)),
            ?assertNot(meck:called(aec_metrics, try_update, [Expired, 1]))
         end}}
      ]}.

%% A genesis chain plus everything a candidate build needs, except the pool:
%% each test below installs its own get_candidate expectation.
setup_deadline_mecks() ->
    AccMap = #{ preset_accounts => [{?TEST_PUB, 100000 * aec_test_utils:min_gas_price()}] },
    {Block0, Trees0} = aec_block_genesis:genesis_block_with_state(AccMap),
    meck:expect(aeu_time, now_in_msecs, 0, 1234567890),
    meck:expect(aec_chain, get_block_state, 1, {ok, Trees0}),
    meck:expect(aec_keys, get_pubkey, 0, {ok, ?TEST_PUB}),
    meck:expect(aec_db, find_discovered_pof, 1, none),
    Block0.

%% The deadline each packing pass was given, in call order.
candidate_deadlines() ->
    [ maps:get(deadline, Opts)
      || {_Pid, {aec_tx_pool, get_candidate, [_, _, _, Opts]}, _}
             <- meck:history(aec_tx_pool) ].

used_gas_test_() ->
    {foreach,
      fun() ->
        meck:new(aeu_time, [passthrough]),
        meck:new(aec_chain, [passthrough]),
        meck:new(aec_db, [passthrough]),
        meck:new(aec_keys, [passthrough]),
        meck:new(aec_tx_pool, [passthrough]),
        meck:new(aec_trees, [passthrough])
      end,
      fun(_) ->
        meck:unload(aec_trees),
        meck:unload(aec_tx_pool),
        meck:unload(aec_keys),
        meck:unload(aec_chain),
        meck:unload(aec_db),
        meck:unload(aeu_time)
      end,
      [{"Check consumed gas by a contract create tx",
        fun() ->
          Height = 10,
          Protocol = aec_hard_forks:protocol_effective_at_height(Height),
          AccMap = #{ preset_accounts => [{?TEST_PUB, 1000000000000000000000000000000000000000}] },
          {Block00, Trees0} = aec_block_genesis:genesis_block_with_state(AccMap),
          %% since the block is genesis one, it does not have the protocol
          %% enabled, so we set it
          Block0 = set_protocol_and_version(Block00, Protocol, Height),
          ContractName = identity,
          VM =
              case Protocol >= ?IRIS_PROTOCOL_VSN of
                  true -> fate;
                  false -> aevm
              end,
          case aect_test_utils:sophia_version(VM, Protocol) of
              {error, _} -> skip;
              SophiaVersion ->
                GasLimit = 1000000,
                CallOpts0 =
                    #{vm_version  => aect_test_utils:vm_version(VM, Protocol),
                      abi_version => aect_test_utils:abi_version(VM,Protocol),
                      gas => GasLimit},
                CreateTx = sign(contract_create_tx(?TEST_PUB,
                                                    SophiaVersion,
                                                    ContractName, "init", [],
                                                    Trees0, CallOpts0), ?TEST_PRIV),

                meck_expect_candidate_prerequisites(1234567890, Trees0, [CreateTx]),
                {ok, _Block1, #{ trees := Trees1 }} = aec_block_micro_candidate:create(Block0),
                %% actual gas used is the consumed gas + the gas
                %% required for the contract call tx itself (~98480)
                Used = aetx:used_gas(aetx_sign:tx(CreateTx), Height, Protocol, Trees1),
                TxLimit = aetx:gas_limit(aetx_sign:tx(CreateTx), Height, Protocol),
                {true, Used} = {Used < GasLimit, Used},
                {true, TxLimit} = {GasLimit < TxLimit, TxLimit}
            end
        end
      },
      {"Check consumed gas by a contract call tx",
        fun() ->
          Height = 10,
          Protocol = aec_hard_forks:protocol_effective_at_height(Height),
          AccMap = #{ preset_accounts => [{?TEST_PUB, 1000000000000000000000000000000000000000}] },
          {Block00, Trees0} = aec_block_genesis:genesis_block_with_state(AccMap),
          %% since the block is genesis one, it does not have the protocol
          %% enabled, so we set it
          Block0 = set_protocol_and_version(Block00, Protocol, Height),
          ContractName = identity,
          VM =
              case Protocol >= ?IRIS_PROTOCOL_VSN of
                  true -> fate;
                  false -> aevm
              end,
          case aect_test_utils:sophia_version(VM, Protocol) of
              {error, _} -> skip;
              SophiaVersion ->
                GasLimit = 1000000,
                CallOpts0 =
                    #{vm_version  => aect_test_utils:vm_version(VM, Protocol),
                      abi_version => aect_test_utils:abi_version(VM,Protocol),
                      gas => GasLimit},
                CreateTx = sign(contract_create_tx(?TEST_PUB,
                                                    SophiaVersion,
                                                    ContractName, "init", [],
                                                    Trees0, CallOpts0), ?TEST_PRIV),
                Time1 = 1234567890,
                meck_expect_candidate_prerequisites(Time1, Trees0, [CreateTx]),
                {ok, Block1, #{ trees := Trees1 }} = aec_block_micro_candidate:create(Block0),
                [CreateTx] = aec_blocks:txs(Block1),



                {aect_create_tx, CrTx} = aetx:specialize_callback(aetx_sign:tx(CreateTx)),
                ContractPubkey = aect_create_tx:contract_pubkey(CrTx),
                CallTx =
                    sign(contract_call_tx(?TEST_PUB, SophiaVersion, ContractPubkey,
                                          ContractName, "main_", ["42"], Trees1, CallOpts0),
                          ?TEST_PRIV),
                meck_expect_candidate_prerequisites(Time1 + 3 * 1000, Trees1, [CallTx]),
                %% since the previous block - Block1 - is a
                %% microblock, we must meck also this call:
                PrevKeyHash = aec_blocks:prev_key_hash(Block1),
                meck:expect(aec_chain, get_block, fun(Hash) -> if Hash == PrevKeyHash -> {ok, Block0} end end),
                {ok, Block2, #{ trees := Trees2 }} = aec_block_micro_candidate:create(Block1),

                [CallTx] = aec_blocks:txs(Block2),
                Used = aetx:used_gas(aetx_sign:tx(CallTx), Height, Protocol, Trees2),
                TxLimit = aetx:gas_limit(aetx_sign:tx(CallTx), Height, Protocol),
                {true, Used} = {Used < GasLimit, Used},
                {true, TxLimit} = {GasLimit < TxLimit, TxLimit},
                ok
            end
        end
      },
      {"Check consumed gas by a paying for",
        fun() ->
            Height = 10,
            Protocol = aec_hard_forks:protocol_effective_at_height(Height),
            case Protocol >= ?IRIS_PROTOCOL_VSN of
                true ->
                    AccMap = #{ preset_accounts => [{?TEST_PUB, 1000000000000000000000000000000000000000},
                                                    {?PAYER_PUB, 1000000000000000000000000000000000000000}
                                                    ] },
                    {Block00, Trees0} = aec_block_genesis:genesis_block_with_state(AccMap),
                    %% since the block is genesis one, it does not have the protocol
                    %% enabled, so we set it
                    Block0 = set_protocol_and_version(Block00, Protocol, Height),
                    Time = 1234567890,
                    STx = sign_inner(spend_tx(#{}, Trees0), ?TEST_PRIV),
                    PNonce = next_nonce(?PAYER_PUB, Trees0),
                    PayingFor =
                        sign(paying_for_tx(?PAYER_PUB, 20000 * aec_test_utils:min_gas_price(),
                                           STx, PNonce),
                             ?PAYER_PRIV),
                    meck_expect_candidate_prerequisites(Time, Trees0, [PayingFor]),
                    {ok, Block1, #{ trees := Trees1 }} = aec_block_micro_candidate:create(Block0),
                    [PayingFor] = aec_blocks:txs(Block1),
                    STUsed = aetx:used_gas(aetx_sign:tx(STx), Height, Protocol, Trees1),
                    PUsed = aetx:used_gas(aetx_sign:tx(PayingFor), Height, Protocol, Trees1),
                    true = STUsed < PUsed;
                false -> pass
            end
        end
      },
      {"Check consumed gas by a paying for wrapped around create tx",
        fun() ->
            Height = 10,
            Protocol = aec_hard_forks:protocol_effective_at_height(Height),
            case Protocol >= ?IRIS_PROTOCOL_VSN of
                true ->
                    AccMap = #{ preset_accounts => [{?TEST_PUB, 1000000000000000000000000000000000000000},
                                                    {?PAYER_PUB, 1000000000000000000000000000000000000000}
                                                    ] },
                    {Block00, Trees0} = aec_block_genesis:genesis_block_with_state(AccMap),
                    %% since the block is genesis one, it does not have the protocol
                    %% enabled, so we set it
                    Block0 = set_protocol_and_version(Block00, Protocol, Height),
                    Time = 1234567890,
                    ContractName = identity,
                    VM = fate, %% only FATE after iris
                    case aect_test_utils:sophia_version(VM, Protocol) of
                        {error, _} -> skip;
                        SophiaVersion ->
                          GasLimit = 1000000,
                          CallOpts0 =
                              #{vm_version  => aect_test_utils:vm_version(VM, Protocol),
                                abi_version => aect_test_utils:abi_version(VM,Protocol),
                                gas => GasLimit},
                          CreateTx = sign_inner(contract_create_tx(?TEST_PUB,
                                                                  SophiaVersion,
                                                                  ContractName, "init", [],
                                                                  Trees0, CallOpts0), ?TEST_PRIV),
                          PNonce = next_nonce(?PAYER_PUB, Trees0),
                          PayingFor = sign(paying_for_tx(?PAYER_PUB,
                                                         20000 * aec_test_utils:min_gas_price(),
                                                         CreateTx, PNonce),
                                           ?PAYER_PRIV),
                          meck_expect_candidate_prerequisites(Time, Trees0, [PayingFor]),
                          {ok, Block1, #{ trees := Trees1 }} = aec_block_micro_candidate:create(Block0),
                          [PayingFor] = aec_blocks:txs(Block1),
                          %% actual gas used is the consumed gas + the gas
                          %% required for the contract call tx itself (~98480)
                          Used = aetx:used_gas(aetx_sign:tx(PayingFor), Height, Protocol, Trees1),
                          TxLimit = aetx:gas_limit(aetx_sign:tx(PayingFor), Height, Protocol),
                          {true, Used} = {Used < GasLimit, Used},
                          {true, TxLimit} = {GasLimit < TxLimit, TxLimit}
                    end;
                false -> pass
            end
        end
      },
      {"Check consumed gas by a meta tx",
        fun() ->
            Height = 10,
            Protocol = aec_hard_forks:protocol_effective_at_height(Height),
            Protocol = aec_hard_forks:protocol_effective_at_height(Height),
            case Protocol >= ?FORTUNA_PROTOCOL_VSN of
                true ->
                    AccMap = #{ preset_accounts => [{?TEST_PUB, 1000000000000000000000000000000000000000}
                                                    ] },
                    {Block00, Trees0} = aec_block_genesis:genesis_block_with_state(AccMap),
                    %% since the block is genesis one, it does not have the protocol
                    %% enabled, so we set it
                    Block0 = set_protocol_and_version(Block00, Protocol, Height),
                    Time = 1234567890,
                    GasLimit = 1000000,
                    VM =
                        case Protocol >= ?IRIS_PROTOCOL_VSN of
                            true -> fate;
                            false -> aevm
                        end,
                    case aect_test_utils:sophia_version(VM, Protocol) of
                        {error, _} -> skip;
                        SophiaVersion ->
                            VMVersion = aect_test_utils:vm_version(VM, Protocol),
                            CallOpts0 =
                                #{vm_version  => VMVersion,
                                  abi_version => aect_test_utils:abi_version(VM,Protocol),
                                  gas => GasLimit},
                            AttachTx = sign(attach_tx(?TEST_PUB, SophiaVersion, "basic_auth", "init", [],
                                                      CallOpts0, Trees0, VMVersion), ?TEST_PRIV),
                            meck_expect_candidate_prerequisites(Time, Trees0, [AttachTx]),
                            {ok, Block1, #{ trees := Trees1 }} = aec_block_micro_candidate:create(Block0),
                            [AttachTx] = aec_blocks:txs(Block1),
                            Used = aetx:used_gas(aetx_sign:tx(AttachTx), Height, Protocol, Trees1),
                            TxLimit = aetx:gas_limit(aetx_sign:tx(AttachTx), Height, Protocol),
                            {true, Used} = {Used < GasLimit, Used},
                            {true, TxLimit} = {GasLimit < TxLimit, TxLimit},

                            %% use meta tx
                            Spend = spend_tx(#{}, Trees0),
                            GANonce = "1",
                            Fee      = 20000 * aec_test_utils:min_gas_price(),
                            GasPrice = 1000 * aec_test_utils:min_gas_price(),
                            TxHash    = aega_test_utils:auth_data_hash(Fee, GasPrice,
                                                                       aec_governance:add_network_id(aetx:serialize_to_binary(Spend))),
                            Signature = aega_test_utils:basic_auth_sign(list_to_integer(GANonce), TxHash, ?TEST_PRIV),
                            AuthData  = aega_test_utils:make_calldata(SophiaVersion, "basic_auth", "authorize",
                                [GANonce, aega_test_utils:to_hex_lit(64, Signature)]),

                            PrevKeyHash = aec_blocks:prev_key_hash(Block1),
                            meck:expect(aec_chain, get_block, fun(Hash) -> if Hash == PrevKeyHash -> {ok, Block0} end end),
                            STx = aetx_sign:new(meta_tx(Spend, ?TEST_PUB, AuthData, CallOpts0#{fee => Fee, gas_price => GasPrice}), []),
                            meck_expect_candidate_prerequisites(Time, Trees1, [STx]),
                            {ok, Block2, #{ trees := Trees2 }} = aec_block_micro_candidate:create(Block1),
                            [STx] = aec_blocks:txs(Block2),

                            SpendUsed = aetx:used_gas(Spend, Height, Protocol, Trees2),
                            MetaUsed = aetx:used_gas(aetx_sign:tx(STx), Height, Protocol, Trees2),
                            _MetaLimit = aetx:gas_limit(aetx_sign:tx(STx), Height, Protocol),
                            {true, MetaUsed} = {MetaUsed < GasLimit + SpendUsed, MetaUsed},
                            ok
                    end,
                    ok;
                false -> pass
            end
        end
      },
      {"Check consumed gas by a meta tx wrapping a contract create",
        fun() ->
            Height = 10,
            Protocol = aec_hard_forks:protocol_effective_at_height(Height),
            Protocol = aec_hard_forks:protocol_effective_at_height(Height),
            case Protocol >= ?FORTUNA_PROTOCOL_VSN of
                true ->
                    AccMap = #{ preset_accounts => [{?TEST_PUB, 1000000000000000000000000000000000000000}
                                                    ] },
                    {Block00, Trees0} = aec_block_genesis:genesis_block_with_state(AccMap),
                    %% since the block is genesis one, it does not have the protocol
                    %% enabled, so we set it
                    Block0 = set_protocol_and_version(Block00, Protocol, Height),
                    Time = 1234567890,
                    GasLimit = 100000,
                    VM =
                        case Protocol >= ?IRIS_PROTOCOL_VSN of
                            true -> fate;
                            false -> aevm
                        end,
                    case aect_test_utils:sophia_version(VM, Protocol) of
                        {error, _} -> skip;
                        SophiaVersion ->
                            VMVersion = aect_test_utils:vm_version(VM, Protocol),
                            CallOpts0 =
                                #{vm_version  => VMVersion,
                                  abi_version => aect_test_utils:abi_version(VM,Protocol),
                                  gas => GasLimit},
                            AttachTx = sign(attach_tx(?TEST_PUB, SophiaVersion, "basic_auth", "init", [],
                                                      CallOpts0, Trees0, VMVersion), ?TEST_PRIV),
                            meck_expect_candidate_prerequisites(Time, Trees0, [AttachTx]),
                            {ok, Block1, #{ trees := Trees1 }} = aec_block_micro_candidate:create(Block0),
                            [AttachTx] = aec_blocks:txs(Block1),
                            Used = aetx:used_gas(aetx_sign:tx(AttachTx), Height, Protocol, Trees1),
                            TxLimit = aetx:gas_limit(aetx_sign:tx(AttachTx), Height, Protocol),
                            {true, TxLimit} = {Used < TxLimit, TxLimit},

                            %% use meta tx
                            ContractName = identity,
                            Create = contract_create_tx(?TEST_PUB, SophiaVersion, ContractName, "init", [],
                                                        Trees1, CallOpts0#{nonce => 0}),
                            GANonce = "1",
                            Fee      =  20000 * aec_test_utils:min_gas_price(),
                            GasPrice =  1000 * aec_test_utils:min_gas_price(),
                            TxHash    = aega_test_utils:auth_data_hash(Fee, GasPrice,
                                                                       aec_governance:add_network_id(aetx:serialize_to_binary(Create))),
                            Signature = aega_test_utils:basic_auth_sign(list_to_integer(GANonce), TxHash, ?TEST_PRIV),
                            AuthData  = aega_test_utils:make_calldata(SophiaVersion, "basic_auth", "authorize",
                                [GANonce, aega_test_utils:to_hex_lit(64, Signature)]),

                            PrevKeyHash = aec_blocks:prev_key_hash(Block1),
                            meck:expect(aec_chain, get_block, fun(Hash) -> if Hash == PrevKeyHash -> {ok, Block0} end end),
                            STx = aetx_sign:new(meta_tx(Create, ?TEST_PUB, AuthData, CallOpts0#{fee => Fee, gas_price => GasPrice}), []),
                            meck_expect_candidate_prerequisites(Time, Trees1, [STx]),
                            {ok, Block2, #{ trees := Trees2 }} = aec_block_micro_candidate:create(Block1),
                            [STx] = aec_blocks:txs(Block2),

                            MetaUsed = aetx:used_gas(aetx_sign:tx(STx), Height, Protocol, Trees2),
                            MetaLimit = aetx:gas_limit(aetx_sign:tx(STx), Height, Protocol),
                            {true, MetaUsed, MetaLimit} = {MetaUsed < MetaLimit, MetaUsed, MetaLimit},

                            {UnderEstimate, OverEstimate} =
                                if VM == fate ->
                                    Under = 75000 + 75000 + 6400, %% Base meta_tx + base create_tx + size(underestimated)
                                    Over  = Under + 100 + 10000,  %% extra size + used gas
                                    {Under, Over};
                                   true ->
                                    Under = 75000 + 75000 + 50000, %% Base meta_tx + base create_tx + size(underestimated)
                                    Over  = Under + 500 + 10000,   %% extra size + used gas
                                    {Under, Over}
                                end,
                            {true, true, UnderEstimate, MetaUsed, OverEstimate} =
                                {UnderEstimate < MetaUsed, MetaUsed < OverEstimate, UnderEstimate, MetaUsed, OverEstimate},

                            ok
                    end,
                    ok;
                false -> pass
            end
        end
      }
      ]}.

get_miner_account_balance(State) ->
    {ok, Miner} = aec_keys:get_pubkey(),
    aec_accounts:balance(aec_accounts_trees:get(Miner,
                                                aec_trees:accounts(State))).

spend_tx(Data, Trees) ->
    NextNonce = next_nonce(?TEST_PUB, Trees),
    spend_tx(Data#{nonce => NextNonce}).

spend_tx(Data) ->
    DefaultData =
        #{ sender_id => ?TEST_ID, recipient_id => ?TEST_ID
         , amount => 10, fee => 20000 * aec_test_utils:min_gas_price(), ttl => 100, nonce => 1, payload => <<>> },
    {ok, Tx} = aec_spend_tx:new(maps:merge(DefaultData, Data)),
    Tx.

contract_create_tx(Owner, SophiaVersion, ContractName, Fun, Args, Trees, Data) ->
    State = aect_test_utils:set_trees(Trees, #{}),
    {ok, Code} = aect_test_utils:compile_contract(SophiaVersion, ContractName),
    _Tx = aect_test_utils:create_tx(Owner, Data#{call_data => call_data(SophiaVersion, ContractName, Fun, Args),
                                                 code => Code },
                                    State).

contract_call_tx(Caller, SophiaVersion, Contract, ContractName, Fun, Args, Trees, Data) ->
    State = aect_test_utils:set_trees(Trees, #{}),
    _Tx = aect_test_utils:call_tx(Caller, Contract,
                                  Data#{call_data => call_data(SophiaVersion, ContractName, Fun, Args)}, State).

call_data(SophiaVersion, ContractName, Fun, Args) when is_atom(ContractName) ->
    {ok, BinSrc} = aect_test_utils:read_contract(SophiaVersion, ContractName),
    {ok, CallData} = aect_test_utils:encode_call_data(BinSrc, Fun, Args),
    CallData.

sign(Tx, Privkey) ->
    aec_test_utils:sign_tx(Tx, Privkey).

sign_inner(Tx, Privkey) ->
    aec_test_utils:sign_pay_for_inner_tx(Tx, Privkey).

set_protocol_and_version(KeyBlock, Protocol, Height) ->
    Header = aec_blocks:to_header(KeyBlock),
    Header1 = aec_headers:set_version_and_height(Header, Protocol, Height),
    aec_blocks:new_key_from_header(Header1).

meck_expect_candidate_prerequisites(Time, Trees, Txs) ->
    meck:expect(aeu_time, now_in_msecs, 0, Time),
    meck:expect(aec_chain, get_block_state, 1, {ok, Trees}),
    meck_tx_pool_get_candidate(Txs),
    meck:expect(aec_db, find_discovered_pof, 1, none).

%% New block packing will call get_candidate (at least) twice - meck this!
%% The set of already packed hashes is empty on the first pass only.
meck_tx_pool_get_candidate(Txs) ->
    meck:expect(aec_tx_pool, get_candidate,
                fun(_, Packed, _, _) when map_size(Packed) =:= 0 -> {ok, Txs, ?DRAINED};
                   (_, _, _, _)  -> {ok, [], ?DRAINED}
                end).

paying_for_tx(Payer, Fee, InnerTx, Nonce) ->
    {ok, Tx} =
        aec_paying_for_tx:new(#{payer_id => aeser_id:create(account, Payer),
                                nonce    => Nonce,
                                fee      => Fee,
                                tx       => InnerTx}),
    Tx.

next_nonce(Pubkey, Trees) ->
    Accounts = aec_trees:accounts(Trees),
    case aec_accounts_trees:lookup(Pubkey, Accounts) of
        none -> 1;
        {value, Acc} ->
            aec_accounts:nonce(Acc) + 1
    end.


attach_tx(Pubkey, SophiaVersion, ContractName, _Fun, _Args, Opts, Trees, VMVersion) ->
    Nonce = next_nonce(Pubkey, Trees),

    {ok, #{bytecode := Code, src := Src, map := #{type_info := TI}}} =
        aega_test_utils:get_contract(SophiaVersion, ContractName),

    CallData = aega_test_utils:make_calldata(SophiaVersion, Src, "init", []),

    {ok, AuthFun} = aega_test_utils:auth_fun_hash(<<"authorize">>, TI,
                                                  VMVersion),

    Map = Opts#{ nonce => Nonce, code => Code, auth_fun => AuthFun,
                 call_data => CallData},
    aega_test_utils:ga_attach_tx(Pubkey, Map).

meta_tx(InnerTx, Pubkey, AuthData, Opts) ->
    aega_test_utils:ga_meta_tx(Pubkey,
                               maps:merge(
                                 #{ auth_data => AuthData,
                                    tx => aetx_sign:new(InnerTx, [])}, Opts)).
-endif.
