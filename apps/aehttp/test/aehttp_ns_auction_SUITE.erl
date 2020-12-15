%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%
%%% @end
%%% Created : 02. Sep 2019 16:15
%%%-------------------------------------------------------------------
-module(aehttp_ns_auction_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-include_lib("stdlib/include/assert.hrl").
-import(aecore_suite_utils, [http_request/4, httpc_request/4, process_http_return/1]).
-import(aecore_suite_utils, [internal_address/0, external_address/0, rpc/3, rpc/4]).

%% common_test exports
-export(
   [
    all/0, groups/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).


-define(HTTP_INT, aehttp_integration_SUITE).
-define(NODE, dev1).
-define(BID_TIMEOUT, 2).

%% API
-export([naming_system_auction/1]).

all() ->
    [{group, naming}].

groups() ->
    [{naming, [sequence], [naming_system_auction]}].

init_per_suite(Config) ->
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => false,
                     <<"hard_forks">> => Forks},
               <<"mining">> =>
                   #{<<"micro_block_cycle">> => 1,
                     <<"name_claim_bid_timeout">> => ?BID_TIMEOUT
                    }},
    {ok, StartedApps} = application:ensure_all_started(gproc),
    Config0 = [{protocol_vsn, aect_test_utils:latest_protocol_version()} | Config],
    Config1 = aecore_suite_utils:init_per_suite([?NODE], DefCfg, [{instant_mining, true},
                                                                  {symlink_name, "latest.http_endpoints"},
                                                                  {test_module, ?MODULE}] ++ Config0),
    aecore_suite_utils:start_node(?NODE, Config1),
    Node = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:connect(Node, []),
    [ {node, Node}
    , {nodes, [aecore_suite_utils:node_tuple(?NODE)]}
    , {started_apps, StartedApps} ]  ++ Config1.

end_per_suite(Config) ->
    aecore_suite_utils:stop_node(?NODE, Config),
    [application:stop(A) ||
        A <- lists:reverse(
               proplists:get_value(started_apps, Config, []))],
    ok.


init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(naming_system_auction, Config) ->
    case ?config(protocol_vsn, Config) >= ?LIMA_PROTOCOL_VSN of
        true -> Config;
        false -> {skip, requires_lima_or_newer}
    end.

end_per_testcase(_Case, Config) ->
    ?HTTP_INT:end_per_testcase_all(Config).


naming_system_auction(Config) ->
    {PubKey1, PrivKey1} = ?HTTP_INT:initialize_account(6*1000000 * aec_test_utils:min_gas_price()),
    {PubKey2, PrivKey2} = ?HTTP_INT:initialize_account(6*1000000 * aec_test_utils:min_gas_price()),

    PubKey1Enc = aeser_api_encoder:encode(account_pubkey, PubKey1),
    PubKey2Enc = aeser_api_encoder:encode(account_pubkey, PubKey2),

    Name        = <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.chain">>,
    NameSalt    = 12345,
    Fee         = 100000 * aec_test_utils:min_gas_price(),

    %% Check mempool empty
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),
    {ok, 200, #{<<"balance">> := Balance}} = ?HTTP_INT:get_accounts_by_pubkey_sut(PubKey1Enc),

    %% Get commitment hash to preclaim a name
    {ok, 200, #{<<"commitment_id">> := EncodedCHash}} = ?HTTP_INT:get_commitment_id(Name, NameSalt),
    {ok, _CHash} = aeser_api_encoder:safe_decode(commitment, EncodedCHash),

    %% Submit name preclaim tx and check it is in mempool
    PreclaimData = #{commitment_id => EncodedCHash,
                     fee           => Fee,
                     account_id    => PubKey1Enc},
    {ok, 200, #{<<"tx">> := PreclaimTxEnc}} = ?HTTP_INT:get_name_preclaim(PreclaimData),
    PreclaimTxHash = ?HTTP_INT:sign_and_post_tx(PreclaimTxEnc, PrivKey1),
    {ok, 200, #{<<"tx">> := PreclaimTx}} = ?HTTP_INT:get_transactions_by_hash_sut(PreclaimTxHash),
    ?assertEqual(EncodedCHash, maps:get(<<"commitment_id">>, PreclaimTx)),

    %% Mine a block and check mempool empty again
    ok = ?HTTP_INT:wait_for_tx_hash_on_chain(PreclaimTxHash),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check fee taken from account
    {ok, 200, #{<<"balance">> := PubKey1BalPreClaim}} = ?HTTP_INT:get_accounts_by_pubkey_sut(PubKey1Enc),
    ?assertEqual(PubKey1BalPreClaim, Balance - Fee),

    FirstNameFee = rpc(aec_governance, name_claim_fee, [Name, ?config(protocol_vsn, Config)]),

    ct:log("Balance PubKey1 before init bid: ~p", [PubKey1BalPreClaim]),
    ct:log("Price for the init bid and tx fee: ~p ~p", [FirstNameFee, Fee]),

    NameEnc = aeser_api_encoder:encode(name, Name),

    %% Submit name claim tx and check it is in mempool
    ClaimData1 = #{account_id => PubKey1Enc,
                   name       => NameEnc,
                   name_salt  => NameSalt,
                   name_fee   => FirstNameFee,
                   fee        => Fee},
    {ok, 200, #{<<"tx">> := ClaimTxEnc1}} = ?HTTP_INT:get_name_claim(ClaimData1),
    ClaimTxHash1 = ?HTTP_INT:sign_and_post_tx(ClaimTxEnc1, PrivKey1),

    %% Mine a block and check mempool empty again
    ok = ?HTTP_INT:wait_for_tx_hash_on_chain(ClaimTxHash1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check tx fee taken from account, claim fee locked,
    %% then mine reward and fee added to account
    {ok, 200, #{<<"balance">> := PubKey1BalPreAuction}} = ?HTTP_INT:get_accounts_by_pubkey_sut(PubKey1Enc),

    ct:log("Balance PubKey1 after init bid: ~p", [PubKey1BalPreAuction]),
    ?assertEqual(PubKey1BalPreAuction, PubKey1BalPreClaim - Fee - FirstNameFee),

    %% Check that name entry is absent as there is auction ongoing
    {ok, 404, #{<<"reason">> := <<"Name not found">>}} = ?HTTP_INT:get_names_entry_by_name_sut(Name),

    {ok, 200, #{<<"balance">> := PubKey2BalPreAuction}} = ?HTTP_INT:get_accounts_by_pubkey_sut(PubKey2Enc),
    ct:log("Balance PubKey2 before counter bid: ~p", [PubKey2BalPreAuction]),

    %% Figure out minimal couter bid price
    NextMinPrice = FirstNameFee + FirstNameFee *
        aec_governance:name_claim_bid_increment() div 100,

    %% Submit couter bid with name claim tx and check it is in mempool
    ClaimData2 = #{account_id => PubKey2Enc,
                   name       => NameEnc,
                   name_salt  => 0,
                   name_fee   => NextMinPrice,
                   fee        => Fee},
    {ok, 200, #{<<"tx">> := ClaimTxEnc2}} = ?HTTP_INT:get_name_claim(ClaimData2),
    ClaimTxHash2 = ?HTTP_INT:sign_and_post_tx(ClaimTxEnc2, PrivKey2),

    %% Mine a block and check mempool empty again
    ok = ?HTTP_INT:wait_for_tx_hash_on_chain(ClaimTxHash2),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check balances
    {ok, 200, #{<<"balance">> := PubKey1BalPostAuction}} = ?HTTP_INT:get_accounts_by_pubkey_sut(PubKey1Enc),
    {ok, 200, #{<<"balance">> := PubKey2BalPostAuction}} = ?HTTP_INT:get_accounts_by_pubkey_sut(PubKey2Enc),

    ct:log("Balance PubKey1 post counter bid: ~p", [PubKey1BalPostAuction]),
    ct:log("Balance PubKey2 post counter bid: ~p", [PubKey2BalPostAuction]),

    %% Return first bid to PubKey1
    ?assertEqual(PubKey1BalPostAuction, PubKey1BalPreAuction + FirstNameFee),
    %% The second bidder, PubKey2, is now charged
    ?assertEqual(PubKey2BalPostAuction, PubKey2BalPreAuction - Fee - NextMinPrice),

    {ok, 404, #{<<"reason">> := <<"Name not found">>}} = ?HTTP_INT:get_names_entry_by_name_sut(Name),
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), ?BID_TIMEOUT),

    %% Check if we get correct name from the API
    {ok, 200, RespMap} = ?HTTP_INT:get_names_entry_by_name_sut(Name),
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    NameHash = aens_hash:name_hash(NameAscii),
    ?assertEqual(aeser_api_encoder:encode(id_hash, aeser_id:create(name, NameHash)), maps:get(<<"id">>, RespMap)).
