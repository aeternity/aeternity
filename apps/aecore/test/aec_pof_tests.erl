%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_pof
%%% @end
%%%-------------------------------------------------------------------
-module(aec_pof_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("aeminer/include/aeminer.hrl").
-include("blocks.hrl").

-import(aec_test_utils,
        [ extend_block_chain_with_state/2
        , blocks_only_chain/1
        , genesis_block/0
        , genesis_block_with_state/1
        ]).


validation_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:mock_genesis_and_forks(),
             aec_test_utils:start_chain_db(),
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpDir) ->
             aec_test_utils:aec_keys_cleanup(TmpDir),
             aec_test_utils:stop_chain_db(),
             aec_test_utils:unmock_genesis_and_forks()
     end,
     [ {"Check valid pof", fun validation_pass/0}
     , {"Check invalid pof: signature", fun validation_fail_signature/0}
     , {"Check invalid pof: same header", fun validation_fail_same_header/0}
     , {"Check invalid pof: siblings", fun validation_fail_siblings/0}
     ]
    }.

validation_pass() ->
    {Header1, Header2, Pubkey,_PrivKey} = make_fraud_headers(),
    PoF = aec_pof:new(Header1, Header2, Pubkey),
    ?assertEqual(ok, aec_pof:validate(PoF)),
    ok.

validation_fail_signature() ->
    {Header1, Header2,_Pubkey,_PrivKey} = make_fraud_headers(),
    #{ public := PubKey1 } = enacl:sign_keypair(),
    PoF = aec_pof:new(Header1, Header2, PubKey1),
    ?assertEqual({error, fraud_header_dont_match_leader_key}, aec_pof:validate(PoF)),
    ok.

validation_fail_same_header() ->
    {Header1,_Header2, PubKey,_PrivKey} = make_fraud_headers(),
    PoF = aec_pof:new(Header1, Header1, PubKey),
    ?assertEqual({error, same_header}, aec_pof:validate(PoF)),
    ok.

validation_fail_siblings() ->
    {Header1, Header2, PubKey,_PrivKey} = make_fraud_headers(),
    Hash = aec_headers:hash_header(Header1),

    PoF = aec_pof:new(Header1,
                      aec_headers:set_prev_hash(Header2, Hash),
                      PubKey),
    ?assertEqual({error, not_siblings}, aec_pof:validate(PoF)).

make_fraud_headers() ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    PresetAccounts = [{PubKey, 1000000 * aec_test_utils:min_gas_price()}],
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),

    %% Create main chain
    TxsFun = fun(1) ->
                     Tx1 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 1, PubKey, 20000 * aec_test_utils:min_gas_price(), 2), PrivKey),
                     [Tx1];
                (_) ->
                     []
             end,
    Chain0 = [B0, B1|_] = gen_block_chain_with_state_by_target(
               PresetAccounts, [?HIGHEST_TARGET_SCI], 1, TxsFun),
    [_KB0,_KB1, MB] = blocks_only_chain(Chain0),

    CommonChain = [B0, B1],
    Txs = [aec_test_utils:sign_tx(make_spend_tx(PubKey, 1, PubKey, 20000 * aec_test_utils:min_gas_price(), 3), PrivKey)],
    Fork = aec_test_utils:extend_block_chain_with_micro_blocks(CommonChain, Txs),
    [_, _, MBF] = blocks_only_chain(Fork),

    {ok, MinerPubKey} = aec_keys:pubkey(),
    {ok, MinerPrivKey} = aec_keys:sign_privkey(),
    {aec_blocks:to_header(MB), aec_blocks:to_header(MBF), MinerPubKey, MinerPrivKey}.

make_spend_tx(Sender, SenderNonce, Recipient, Fee, Amount) ->
    SenderId = aeser_id:create(account, Sender),
    RecipientId = aeser_id:create(account, Recipient),
    {ok, SpendTx} = aec_spend_tx:new(#{sender_id => SenderId,
                                       recipient_id => RecipientId,
                                       amount => Amount,
                                       fee => Fee,
                                       nonce => SenderNonce,
                                       payload => <<>>}),
    SpendTx.

gen_block_chain_with_state_by_target(PresetAccounts, Targets, Nonce, TxsFun) ->
    Data = #{ targets => Targets, txs_by_height_fun => TxsFun, nonce => Nonce },
    {B0, S0} = genesis_block_with_state(PresetAccounts),
    extend_block_chain_with_state([{B0, S0}], Data).
