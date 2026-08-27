%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aetx_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("aecore/include/blocks.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aecontract/include/aecontract.hrl").

-define(TEST_MODULE, aetx).

-define(RECIPIENT_PUBKEY, <<"_________recipient_pubkey_______">>).

-define(PROTOCOLS, [?ROMA_PROTOCOL_VSN, ?MINERVA_PROTOCOL_VSN, ?FORTUNA_PROTOCOL_VSN,
                    ?LIMA_PROTOCOL_VSN, ?IRIS_PROTOCOL_VSN, ?CERES_PROTOCOL_VSN,
                    ?ARCUS_PROTOCOL_VSN, ?SALUS_PROTOCOL_VSN]).

%% Probably to be moved to common tests
apply_signed_txs_test_() ->
    {setup,
     fun() ->
             ok = meck:new(aec_chain, [passthrough]),
             meck:expect(aec_chain, get_top_state, 0, {ok, aec_trees:new()}),
             ok = meck:new(aec_governance, [passthrough]),
             meck:expect(aec_governance, minimum_gas_price, 1, 1),
             ok
     end,
     fun(_) ->
             meck:unload(aec_governance),
             meck:unload(aec_chain)
     end,
     [{"Apply txs and check resulting balances",
       fun() ->
               %% Init state tree with 2 accounts
               {MinerPubkey, MinerPrivkey} = aecore_suite_utils:generate_key_pair(),

               Nonce = 10,
               SomeAmt = 40,
               Fee = 20000,
               SenderBalance = Fee + SomeAmt + 10, %% some extra, so we don't end with a balance of 0
               MinerAccount = account(MinerPubkey, SenderBalance, Nonce),
               RecipientBalance = 80000,
               AnotherAccount = account(?RECIPIENT_PUBKEY, RecipientBalance, 12),
               StateTree0 = aec_test_utils:create_state_tree_with_accounts([MinerAccount, AnotherAccount]),

               BlockHeight = 30,
               %% Create 2 signed transactions (1 valid, 1 invalid)
               {ok, SpendTx} =
                    spend_tx(#{sender_id => aeser_id:create(account, MinerPubkey),
                               nonce => Nonce + 1,
                               fee => Fee,
                               amount => SomeAmt}),
               {ok, OverBalanceTx} = spend_tx(
                                       #{sender_id => aeser_id:create(account, MinerPubkey),
                                         amount => SenderBalance + 1,
                                         fee => Fee,
                                         nonce => Nonce + 2}),
               SignedSpendTx = aec_test_utils:sign_tx(SpendTx, MinerPrivkey),
               SignedOverBalanceTx = aec_test_utils:sign_tx(OverBalanceTx, MinerPrivkey),

               SignedTxs = [SignedSpendTx, SignedOverBalanceTx],
               Env = aetx_env:tx_env(BlockHeight),
               {ok, ValidSignedTxs, StateTree, _Events} =
                  aec_block_micro_candidate:apply_block_txs(SignedTxs, StateTree0, Env),

               ?assertEqual([SignedSpendTx], ValidSignedTxs),

               ResultAccountsTree = aec_trees:accounts(StateTree),
               {value, ResultMinerAccount} = aec_accounts_trees:lookup(MinerPubkey, ResultAccountsTree),
               {value, ResultRecipientAccount} = aec_accounts_trees:lookup(?RECIPIENT_PUBKEY, ResultAccountsTree),

               %% Initial balance - spend_tx amount - spend_tx fee
               ?assertEqual(SenderBalance - SomeAmt - Fee, aec_accounts:balance(ResultMinerAccount)),
               ?assertEqual(RecipientBalance + SomeAmt, aec_accounts:balance(ResultRecipientAccount))
       end
      }]}.

spend_tx(Opts) ->
    DefaultOpts =
        #{recipient_id => aeser_id:create(account, ?RECIPIENT_PUBKEY),
          amount => 40,
          fee => 20000,
          ttl => 100,
          nonce => 11,
          payload => <<"">>},
    {ok, _SpendTx} = aec_spend_tx:new(maps:merge(DefaultOpts, Opts)).

account(Pubkey, Balance, Nonce) ->
    aec_accounts:set_nonce(aec_accounts:new(Pubkey, Balance), Nonce).

check_used_gas_test_() ->
    {setup,
     fun() ->
          ok
     end,
     fun(_) ->
         ok
     end,
     [{"Check spend used gas",
        fun() ->
            Height = 10,
            Protocol = aec_hard_forks:protocol_effective_at_height(Height),
            {Pubkey, _Privkey} = aecore_suite_utils:generate_key_pair(),
            ID = aeser_id:create(account, Pubkey),
            Account = account(Pubkey, 1000000000000000000, 1),
            Trees0 = aec_test_utils:create_state_tree_with_accounts([Account]),
            Test =
                fun(Opts, GasConsumed) ->
                    {ok, Spend} = spend_tx(maps:merge(Opts, #{sender_id => ID})),
                    GasConsumed = aetx:used_gas(Spend, Height, Protocol, Trees0)
                end,
            Test(#{}, 16580),
            Test(#{payload => <<"hello">>}, 16680),
            case Protocol > ?ROMA_PROTOCOL_VSN of
                true -> %% higher gas price, so a bit more of gas consumed for the bigger size of the transaction
                    Test(#{fee => 20000 * aec_governance:minimum_gas_price(Protocol)}, 16640);
                false -> pass
            end
       end
      }
     ]}.

%% tx_base_gas/2 has no catch-all, so every type is either priced there or on
%% no_base_gas_tx_types/0; one that is neither raises out of every arity that
%% reaches base_gas/2. Enumerated so the next new type fails here, not the node.
tx_type_base_gas_coverage_test_() ->
    NoBaseGas = ?TEST_MODULE:no_base_gas_tx_types(),
    [{"Every no-base-gas type is a real tx type",
      fun() -> ?assertEqual([], NoBaseGas -- ?TEST_MODULE:tx_types()) end}
     | [ {lists:concat([Type, " at protocol ", Protocol]),
          fun() ->
                  case lists:member(Type, NoBaseGas) of
                      false ->
                          ?assert(is_integer(base_gas_of(Type, Protocol)));
                      true ->
                          %% On the list precisely because governance cannot
                          %% price it. A tx_base_gas/2 clause added for one of
                          %% these has to take it back off the list.
                          ?assertError(function_clause, base_gas_of(Type, Protocol))
                  end
          end}
         || Type <- ?TEST_MODULE:tx_types(), Protocol <- ?PROTOCOLS ]].

base_gas_of(Type, Protocol) when Type =:= contract_create_tx;
                                 Type =:= contract_call_tx;
                                 Type =:= ga_attach_tx;
                                 Type =:= ga_meta_tx ->
    aec_governance:tx_base_gas(Type, Protocol, ?ABI_FATE_SOPHIA_1);
base_gas_of(Type, Protocol) ->
    aec_governance:tx_base_gas(Type, Protocol).

%% The fee and gas entry points aec_tx_pool calls on a transaction it is asked to
%% hold: check_minimum_fee/6 reaches min_fee/3 (and through it fee_gas/3 and
%% gas_limit/3), check_minimum_miner_gas_price/6 reaches min_gas_price/3. A raise
%% in any of them kills the process holding the transaction instead of rejecting
%% it, so each has to answer for a no-base-gas type too.
no_base_gas_tx_fee_functions_test_() ->
    Height = 100,
    [ {lists:concat([?TEST_MODULE:tx_type(Tx), " at protocol ", Protocol]),
       fun() ->
               ?assertEqual(0, ?TEST_MODULE:gas_limit(Tx, Height, Protocol)),
               ?assertEqual(0, ?TEST_MODULE:fee_gas(Tx, Height, Protocol)),
               ?assertEqual(0, ?TEST_MODULE:min_fee(Tx, Height, Protocol)),
               %% Both arities: min_gas_price/3 delegates to min_gas_price/4
               %% below Iris, and each divides by the fee gas computed above.
               ?assertEqual(0, ?TEST_MODULE:min_gas_price(Tx, Height, Protocol))
       end}
      || Tx <- constructible_no_base_gas_txs(), Protocol <- ?PROTOCOLS ].

constructible_no_base_gas_txs() ->
    %% channel_client_reconnect_tx is in aetx:tx_types/0 but aetx:type_to_cb/1
    %% has no clause for it, so no aetx() of that type can be built at all. The
    %% coverage test above is what keeps it accounted for.
    {ok, OffchainTx} =
        aesc_offchain_tx:new(#{channel_id => aeser_id:create(channel, <<1:32/unit:8>>),
                               state_hash => <<2:32/unit:8>>,
                               round      => 1}),
    {ok, VoteTx} =
        aec_hc_vote_tx:new(#{voter_id => aeser_id:create(account, <<3:32/unit:8>>),
                             epoch    => 1,
                             type     => 1,
                             data     => #{}}),
    [OffchainTx, VoteTx].

%% The no-base-gas types are not the only route to zero fee gas. An oracle
%% transaction whose absolute TTL is already behind the current height falls off
%% aeo_utils:ttl_delta/2 with {error, too_low_height}, and gas_limit/3's oracle
%% clauses answer 0 for it - the case aetx:check_minimum_fee/2's own comment
%% names. fee_gas/3's catch-all then delegates to gas_limit/3, so min_gas_price/3
%% divides by that zero for an ordinary, priced tx type. Kept as its own test
%% because it is a second, independent way into fee_gas_price/2's zero clause.
expired_absolute_ttl_oracle_tx_fee_functions_test_() ->
    Height = 100,
    Expired = {block, Height - 50},
    [ {lists:concat([?TEST_MODULE:tx_type(Tx), " with an expired absolute TTL",
                     " at protocol ", Protocol]),
       fun() ->
               ?assertEqual(0, ?TEST_MODULE:gas_limit(Tx, Height, Protocol)),
               ?assertEqual(0, ?TEST_MODULE:fee_gas(Tx, Height, Protocol)),
               ?assertEqual(0, ?TEST_MODULE:min_fee(Tx, Height, Protocol)),
               ?assertEqual(0, ?TEST_MODULE:min_gas_price(Tx, Height, Protocol))
       end}
      || Tx <- expired_ttl_oracle_txs(Expired), Protocol <- ?PROTOCOLS ].

%% Only two of the four oracle types can hold an absolute TTL. The other two
%% are typed relative_ttl() and pin ?ttl_delta_int in serialize/1 and
%% deserialize/2, so a {block, _} TTL cannot exist for them at all.
expired_ttl_oracle_txs(ExpiredTTL) ->
    AccountId = aeser_id:create(account, <<4:32/unit:8>>),
    OracleId  = aeser_id:create(oracle,  <<5:32/unit:8>>),
    {ok, RegisterTx} =
        aeo_register_tx:new(#{account_id      => AccountId,
                              nonce           => 1,
                              query_format    => <<"string">>,
                              abi_version     => ?ABI_NO_VM,
                              response_format => <<"string">>,
                              query_fee       => 1,
                              oracle_ttl      => ExpiredTTL,
                              fee             => 20000}),
    {ok, QueryTx} =
        aeo_query_tx:new(#{sender_id     => AccountId,
                           nonce         => 1,
                           oracle_id     => OracleId,
                           query         => <<"who?">>,
                           query_fee     => 1,
                           query_ttl     => ExpiredTTL,
                           response_ttl  => {delta, 10},
                           fee           => 20000}),
    [RegisterTx, QueryTx].
