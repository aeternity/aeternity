-module(aehttp_integration_SUITE).

%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% Endpoint calls
-export([]).

%% test case exports
%% external endpoints
-export(
   [
    % pings
    broken_pings/1,
    blocked_ping/1,
    correct_ping/1,

    % get block-s
    get_top_empty_chain/1,
    get_top_non_empty_chain/1,
    block_by_height/1,
    block_not_found_by_height/1,
    block_by_hash/1,
    block_not_found_by_broken_hash/1,
    block_not_found_by_hash/1,

    % sync gossip 
    pending_transactions/1,
    post_correct_blocks/1,
    post_broken_blocks/1,
    post_correct_tx/1,

    % balances
    miner_balance/1,
    all_accounts_balances/1,
    all_accounts_balances_empty/1,
    all_accounts_balances_disabled/1
   ]).
%%
%% test case exports
%% external endpoints
-export(
   [
    broken_spend_tx/1,
    miner_pub_key/1
   ]).

-include_lib("common_test/include/ct.hrl").
-define(NODE, dev1).

all() ->
    [
     {group, all_endpoints}
    ].

groups() ->
    [
     {all_endpoints, [sequence], [{group, external_endpoints},
                                  {group, internal_endpoints}]},
     {external_endpoints, [sequence],
      [
       % pings
       broken_pings,
       blocked_ping,
       correct_ping,

       % get block-s
       get_top_empty_chain,
       get_top_non_empty_chain,
       block_not_found_by_height,
       block_by_height,
       block_by_hash,
       block_not_found_by_broken_hash,
       block_not_found_by_hash,

       % sync APIs
       post_correct_blocks,
       post_broken_blocks,
       pending_transactions,
       post_correct_tx,
  
       % balances
       miner_balance,
       all_accounts_balances,
       all_accounts_balances_empty,
       all_accounts_balances_disabled

      ]},
     {internal_endpoints, [sequence],
      [
       broken_spend_tx,
       miner_pub_key
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    ok = application:ensure_started(erlexec),
    DataDir = ?config(data_dir, Config),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    Config1 = [{symlink_name, "latest.http_endpoints"},
               {top_dir, TopDir},
               {test_module, ?MODULE}] ++ Config,
    aecore_suite_utils:make_shortcut(Config1),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    aecore_suite_utils:create_configs(Config1, #{<<"chain">> =>
                                                 #{<<"persist">> => false}}),
    aecore_suite_utils:make_multi(Config1, [?NODE]),
    Config1.

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]} | Config].
    

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(?NODE)),
    ct:log("testcase pid: ~p", [self()]),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    aecore_suite_utils:stop_node(?NODE, Config),
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================
correct_ping(_Config) ->
    PingObj = rpc(aec_sync, local_ping_object, []),
    {ok, 200, _} = post_ping(maps:put(<<"source">>, unique_peer(), PingObj)),
    ok.

broken_pings(_Config) ->
    % no 'source'
    {ok, 400, _} = post_ping(#{}),
    % no ping obj data
    {ok, 400, _} = post_ping(#{<<"source">> => unique_peer()}),
    PingObj = rpc(aec_sync, local_ping_object, []),
    % wrong genesis hash 
    WrongGenHashPing = maps:merge(PingObj, #{<<"source">> => unique_peer(),
                                             <<"genesis_hash">> => <<"foo">>}),
    {ok, 409, #{<<"reason">> := <<"Different genesis blocks">>}} =
        post_ping(WrongGenHashPing),
    ok.

blocked_ping(_Config) ->
    Peer = unique_peer(),
    PingObj = rpc(aec_sync, local_ping_object, []),
    WrongGenHashPing = maps:merge(PingObj, #{<<"source">> => Peer,
                                             <<"genesis_hash">> => <<"foo">>}),
    {ok, 409, _} = post_ping(WrongGenHashPing),
    % node is blocked now 
    {ok, 403, #{<<"reason">> := <<"Not allowed">>}} =
        post_ping(maps:put(<<"source">>, Peer, PingObj)),
    ok.

get_top_empty_chain(_Config) ->
    {ok, 200, HeaderMap} = get_top(), 
    ct:log("~p returned header = ~p", [?NODE, HeaderMap]),
    {ok, 200, GenBlockMap} = get_block_by_height(0),
    {ok, GenBlock} = aec_blocks:deserialize_from_map(
                      aehttp_dispatch_ext:add_missing_to_genesis_block(GenBlockMap)),
    ExpectedMap = header_to_endpoint_top(aec_blocks:to_header(GenBlock)),
    ct:log("Cleaned top header = ~p", [ExpectedMap]),
    HeaderMap = ExpectedMap,
    ok.

get_top_non_empty_chain(_Config) ->
    ok = aecore_suite_utils:mine_one_block(aecore_suite_utils:node_name(?NODE)),
    ExpectedH = rpc(aec_conductor, top_header, []), 
    ExpectedMap = header_to_endpoint_top(ExpectedH),
    ct:log("Cleaned top header = ~p", [ExpectedMap]),
    {ok, 200, HeaderMap} = get_top(), 
    HeaderMap = ExpectedMap,
    #{<<"height">> := Height} = HeaderMap,
    true = Height > 0,
    ok.

block_by_height(_Config) ->
    BlocksToCheck = 4,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToCheck),
    TopHeader = rpc(aec_conductor, top_header, []), 
    BlocksToCheck = aec_headers:height(TopHeader),
    lists:foreach(
        fun(Height) ->
            {ok, ExpectedBlock} = rpc(aec_conductor, get_block_by_height, [Height]),
            ExpectedBlockMap = block_to_endpoint_map(ExpectedBlock), 
            {ok, 200, BlockMap} = get_block_by_height(Height),
            ct:log("ExpectedBlockMap ~p, BlockMap: ~p", [ExpectedBlockMap,
                                                         BlockMap]),
            BlockMap = ExpectedBlockMap,
            #{<<"height">> := Height} = BlockMap
        end,
        lists:seq(0, BlocksToCheck)), % from genesis
    ok.

block_not_found_by_height(_Config) ->
    TopHeader = rpc(aec_conductor, top_header, []), 
    0 = aec_headers:height(TopHeader), %chain is empty
    NumberOfChecks = 10,
    lists:foreach(
        fun(_) ->
            Height = rand:uniform(99) + 1, % random number 1-100
            {ok, 404, #{<<"reason">> := <<"Chain too short">>}} = get_block_by_height(Height)
        end,
        lists:seq(1, NumberOfChecks)), % number
    ok.

block_not_found_by_hash(_Config) ->
    NumberOfChecks = 10,
    lists:foreach(
        fun(_) ->
            H = random_hash(),
            {error, block_not_found} = rpc(aec_conductor, get_block_by_hash, [H]), 
            Hash = base64:encode(H),
            {ok, 404, #{<<"reason">> := <<"Block not found">>}} = get_block_by_hash(Hash)
        end,
        lists:seq(1, NumberOfChecks)), % number
    ok.

block_not_found_by_broken_hash(_Config) ->
    NumberOfChecks = 10,
    lists:foreach(
        fun(_) ->
            H =
                lists:map(
                    fun(_) -> rand:uniform(26) + 96 end,
                    lists:seq(1, 32)),
            Hash = list_to_binary(H),
            {ok, 404, #{<<"reason">> := <<"Block not found">>}} = get_block_by_hash(Hash)
        end,
        lists:seq(1, NumberOfChecks)), % number
    ok.

block_by_hash(_Config) ->
    BlocksToCheck = 4,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToCheck),
    TopHeader = rpc(aec_conductor, top_header, []), 
    BlocksToCheck = aec_headers:height(TopHeader),
    lists:foreach(
        fun(Height) ->
            {ok, ExpectedBlock} = rpc(aec_conductor, get_block_by_height, [Height]),
            {ok, H} = aec_blocks:hash_internal_representation(ExpectedBlock),
            Hash = base64:encode(H),
            ExpectedBlockMap = block_to_endpoint_map(ExpectedBlock), 
            {ok, 200, BlockMap} = get_block_by_hash(Hash),
            ct:log("ExpectedBlockMap ~p, BlockMap: ~p", [ExpectedBlockMap,
                                                         BlockMap]),
            BlockMap = ExpectedBlockMap,
            #{<<"height">> := Height} = BlockMap
        end,
        lists:seq(0, BlocksToCheck)), % from genesis
    ok.

%% Maybe this test should be broken into a couple of smaller tests
%% it currently tests the possitive cases for
%% GET externalAPI/transactions
%% POST internalAPI/spend-tx
%% GET externalAPI/account/balance
pending_transactions(_Config) ->
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty 
    {ok, 200, []} = get_transactions(), 
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} = get_balance(),
    BlocksToMine = 10,
    AmountToSpent = 7,
    Fee = 2,
    MineReward = rpc(aec_governance, block_mine_reward, []),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    {ok, 200, #{<<"balance">> := Bal0}} = get_balance(),  

    Bal0 = BlocksToMine * MineReward,
    true = (is_integer(Bal0) andalso Bal0 > AmountToSpent + Fee),

    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % still empty 
    {ok, 200, []} = get_transactions(), 

    %{ok, SenderPubKey} = rpc:call(?NODE, aec_keys, pubkey, [], 5000),
    ReceiverPubKey = random_hash(),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
                  get_balance(base64:encode(ReceiverPubKey)),

    {ok, 200, _} = post_spend_tx(ReceiverPubKey, AmountToSpent, Fee),
    {ok, NodeTxs} = rpc(aec_tx_pool, peek, [infinity]),
    true = length(NodeTxs) =:= 1, % not empty anymore
    {ok, 200, ReturnedTxs} = get_transactions(), 
    ExpectedTxs = [#{<<"tx">> => base64:encode(aec_tx_sign:serialize_to_binary(T))}
           || T <- NodeTxs],
    true = length(ExpectedTxs) =:= length(ReturnedTxs),
    true = lists:all(fun(Tx) -> lists:member(Tx, ExpectedTxs) end, ReturnedTxs),

    {ok, 200, #{<<"balance">> := Bal0}} = get_balance(),  
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
                  get_balance(base64:encode(ReceiverPubKey)),


    % Currently aec_conductor:start fails to mine a new block;
    % investigate this issue and uncomment the following lines
    %ok = aecore_suite_utils:mine_one_block(aecore_suite_utils:node_name(?NODE)),
    %{ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty again
    %{ok, 200, []} = get_transactions(), 

    %{ok, 200, #{<<"balance">> := Bal1}} = get_balance(),  
    %Bal1 = Bal0 + MineReward + Fee - AmountToSpent - Fee,
    %{ok, 200, #{<<"balance">> := AmountToSpent}} = 
    %             get_balance(base64:encode(ReceiverPubKey)),

    ok.

post_correct_blocks(Config) ->
    BlocksToPost = 10,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToPost),
    Blocks =
        lists:map(
            fun(Height) ->
                {ok, B} = rpc(aec_conductor, get_block_by_height, [Height]),
                B
            end,
            lists:seq(1, BlocksToPost)),
    % consider a rpc call for cleaning the aec_chain_state's state
    aecore_suite_utils:stop_node(?NODE, Config),
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(?NODE)),
    GH = rpc(aec_conductor, top_header, []), 
    0 = aec_headers:height(GH), %chain is empty
    lists:foreach(
        fun(Block) ->
            {ok, 200, _} = post_block(Block),
            H = rpc(aec_conductor, top_header, []), 
            {ok, HH} = aec_headers:hash_header(H),
            {ok, BH} = aec_blocks:hash_internal_representation(Block),
            BH = HH % block accepted
        end,
        Blocks),
    ok.

post_broken_blocks(Config) ->
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   1),
    {ok, CorrectBlock} = rpc(aec_conductor, get_block_by_height, [1]),
    % consider a rpc call for cleaning the aec_chain_state's state
    aecore_suite_utils:stop_node(?NODE, Config),
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(?NODE)),
    GH = rpc(aec_conductor, top_header, []), 
    0 = aec_headers:height(GH), %chain is empty
    CorrectBlockMap = aec_blocks:serialize_to_map(CorrectBlock),
    BrokenBlocks =
        lists:map(
            fun({Key, Value}) ->
                BrokenBlock = maps:put(Key, Value, CorrectBlockMap),
                {Key, BrokenBlock}
            end,
        [{<<"prev_hash">>, base64:encode(<<"foo">>)},
         {<<"state_hash">>, base64:encode(<<"foo">>)},
         {<<"txs_hash">>, base64:encode(<<"foo">>)},
         {<<"target">>, 42},
         {<<"nonce">>, 42},
         {<<"time">>, 42},
         {<<"version">>, 42},
         {<<"pow">>, lists:seq(1, 42)},
         {<<"transactions">>, []}
        ]),

    lists:foreach(
        fun({BrokenField, BlockMap}) ->
            ct:log("Testing with a broken ~p", [BrokenField]),
            {ok, Block} = aec_blocks:deserialize_from_map(BlockMap),
            {ok, 400, #{<<"reason">> := <<"Block rejected">>}} = post_block(Block),
            H = rpc(aec_conductor, top_header, []), 
            0 = aec_headers:height(H) %chain is still empty
        end,
        BrokenBlocks),
    ok.

%% Even though a tx with a unknown sender pubkey would be accepted, we need
%% a valid account nonce; that's why we mine a while
post_correct_tx(_Config) ->
    Amount = 7,
    Fee = 2,
    BlocksToMine = 10,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty 
    {ok, 200, _} = get_balance(), % account present
    {ok, PubKey} = rpc(aec_keys, pubkey, []),
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [PubKey]),
    {ok, SpendTx} =
        aec_spend_tx:new(
          #{sender => PubKey,
            recipient => random_hash(),
            amount => Amount,
            fee => Fee,
            nonce => Nonce}),
    {ok, SignedTx} = rpc(aec_keys, sign, [SpendTx]),
    post_tx(SignedTx),
    {ok, [SignedTx]} = rpc(aec_tx_pool, peek, [infinity]), % same tx 
    ok.

%% TODO: POST broken tx (currently not handled in API Endpoint)

%% changing of another account's balance is checked in pending_transactions test
miner_balance(_Config) ->
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} = get_balance(),
    BlocksToMine = 10,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    {ok, Bal} = rpc(aec_mining, get_miner_account_balance, []),
    {ok, 200, #{<<"balance">> := Bal}} = get_balance(),  
    {ok, PubKey} = rpc(aec_keys, pubkey, []),
    {ok, 200, #{<<"balance">> := Bal}} = get_balance(base64:encode(PubKey)),
    ok.

all_accounts_balances(_Config) ->
    rpc(application, set_env, [aehttp, enable_debug_endpoints, true]),
    GenesisAccounts = rpc(aec_genesis_block_settings, preset_accounts, []),
    BlocksToMine = 10,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    {ok, 200, #{<<"accounts_balances">> := Balances}} = get_all_accounts_balances(),
    {ok, MinerPubKey} = rpc(aec_keys, pubkey, []),
    {ok, MinerBal} = rpc(aec_mining, get_miner_account_balance, []),
    ExpectedBalances = [{MinerPubKey, MinerBal} | GenesisAccounts],

    true = length(Balances) =:= length(ExpectedBalances),
    true =
        lists:all(
            fun(#{<<"pub_key">> := PKEncoded, <<"balance">> := Bal}) ->
                Account = {base64:decode(PKEncoded), Bal},
                lists:member(Account, ExpectedBalances) end,
            Balances),
    %% TODO: after fixing the aec_conductor mining issue, spend some txs
    %% and check all accounts
    ok.

all_accounts_balances_empty(_Config) ->
    rpc(application, set_env, [aehttp, enable_debug_endpoints, true]),
    GenesisAccounts = rpc(aec_genesis_block_settings, preset_accounts, []),
    {ok, 200, #{<<"accounts_balances">> := Balances}} = get_all_accounts_balances(),
    true = length(Balances) =:= length(GenesisAccounts),
    true =
        lists:all(
            fun(#{<<"pub_key">> := PKEncoded, <<"balance">> := Bal}) ->
                Account = {base64:decode(PKEncoded), Bal},
                lists:member(Account, GenesisAccounts) end,
            Balances),
    ok.

all_accounts_balances_disabled(_Config) ->
    rpc(application, set_env, [aehttp, enable_debug_endpoints, false]),
    {ok, 404, _} = get_all_accounts_balances(),
    ok.

%% TODO: GetInfo: should it be disabled?

%% possitive test of spend_tx is handled in pending_transactions test
broken_spend_tx(_Config) ->
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} = get_balance(),
    ReceiverPubKey = random_hash(),
    {ok, 404, _} = post_spend_tx(ReceiverPubKey, 42, 2),
    ok.

miner_pub_key(_Config) ->
    {ok, MinerPubKey} = rpc(aec_keys, pubkey, []),
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_miner_pub_key(),
    MinerPubKey = base64:decode(EncodedPubKey),
    ok.
%% ============================================================
%% HTTP Requests 
%% ============================================================

get_top() ->
    Host = external_address(),
    http_request(Host, get, "top", []).

post_ping(Body) ->
    Host = external_address(),
    http_request(Host, post, "ping", Body).

get_block_by_height(Height) ->
    Host = external_address(),
    http_request(Host, get, "block-by-height", [{height, Height}]).

get_block_by_hash(Hash) ->
    Host = external_address(),
    http_request(Host, get, "block-by-hash", [{hash, Hash}]).

get_transactions() ->
    Host = external_address(),
    http_request(Host, get, "transactions", []).

post_spend_tx(Recipient, Amount, Fee) ->
    Host = internal_address(),
    http_request(Host, post, "spend-tx",
                 #{recipient_pubkey => base64:encode(Recipient),
                   amount => Amount,
                   fee => Fee}).

get_balance() ->
    Host = external_address(),
    http_request(Host, get, "account/balance", []).

get_balance(PubKey) ->
    Host = external_address(),
    http_request(Host, get, "account/balance", [{pub_key, PubKey}]).

post_block(Block) ->
    Host = external_address(),
    BlockMap = aec_blocks:serialize_to_map(Block),
    http_request(Host, post, "block", BlockMap).

post_tx(SignedTx) ->
    Host = external_address(),
    TxSerialized = base64:encode(aec_tx_sign:serialize_to_binary(SignedTx)),
    http_request(Host, post, "tx", #{tx => TxSerialized}).

get_all_accounts_balances() ->
    Host = external_address(),
    http_request(Host, get, "balances", []).

get_miner_pub_key() ->
    Host = internal_address(),
    http_request(Host, get, "account/pub-key", []).

%% ============================================================
%% private functions
%% ============================================================
rpc(Mod, Fun, Args) ->
    rpc:call(aecore_suite_utils:node_name(?NODE), Mod, Fun, Args, 5000).

external_address() ->
    Port = rpc(aeu_env, user_config_or_env, 
              [ [<<"http">>, <<"external">>, <<"port">>], 
                aehttp, [swagger_port_external], 8043]),
    aeu_requests:pp_uri({http, "127.0.0.1", Port}). % good enough for requests

internal_address() ->
    Port = rpc(aeu_env, user_config_or_env, 
              [ [<<"http">>, <<"internal">>, <<"port">>], 
                aehttp, [internal, swagger_port], 8143]),
    aeu_requests:pp_uri({http, "127.0.0.1", Port}).

http_request(Host, get, Path, Params) ->
    URL = binary_to_list(
            iolist_to_binary(
              [Host, "v1/", Path, encode_get_params(Params)])),
    ct:log("GET ~p", [URL]),
    R = httpc:request(get, {URL, []}, [], []),
    process_http_return(R);
http_request(Host, post, Path, Params) ->
    URL = Host ++ "v1/" ++ Path,
    {Type, Body} = case Params of
                       Map when is_map(Map) ->
                           %% JSON-encoded
                           {"application/json", jsx:encode(Params)};
                       [] ->
                           {"application/x-www-form-urlencoded",
                            http_uri:encode(Path)}
                   end,
    %% lager:debug("Type = ~p; Body = ~p", [Type, Body]),
    ct:log("POST ~p, type ~p, Body ~p", [URL, Type, Body]),
    R = httpc:request(post, {URL, [], Type, Body}, [], []),
    process_http_return(R).


encode_get_params(#{} = Ps) ->
    encode_get_params(maps:to_list(Ps));
encode_get_params([{K,V}|T]) ->
    ["?", [str(K),"=",uenc(V)
           | [["&", str(K1), "=", uenc(V1)]
              || {K1, V1} <- T]]];
encode_get_params([]) ->
    [].

str(A) when is_atom(A) ->
    str(atom_to_binary(A, utf8));
str(S) when is_list(S); is_binary(S) ->
    S.

uenc(I) when is_integer(I) ->
    uenc(integer_to_list(I));
uenc(V) ->
    http_uri:encode(V).

process_http_return(R) ->
    case R of
        {ok, {{_, ReturnCode, _State}, _Head, Body}} ->
            try
                ct:log("Return code ~p, Body ~p", [ReturnCode, Body]),
                Result =
                    case iolist_to_binary(Body) of
                        <<>> ->
                            #{};
                        BodyB ->
                            jsx:decode(BodyB, [return_maps])
                    end,
                {ok, ReturnCode, Result}
            catch
                error:E ->
                    {error, {parse_error, [E, erlang:get_stacktrace()]}}
            end;
        {error, _} = Error ->
            Error
    end.

header_to_endpoint_top(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    {ok, HMap} = aec_headers:serialize_to_map(Header),
    CleanedH = aehttp_dispatch_ext:cleanup_genesis(HMap),
    maps:put(<<"hash">>, base64:encode(Hash), CleanedH).

block_to_endpoint_map(Block) ->
    BMap = aec_blocks:serialize_to_map(Block),
    aehttp_dispatch_ext:cleanup_genesis(BMap).

%% misbehaving peers get blocked so we need unique ones
%% the function bellow is not perfect: there is a slight possibility of having a
%% duplicate
%% P = NumberOfPeersRequested / AllCombinations.
%% AllCombinations = 254 * 255 * 255 * 255 * 65535  = 276 011 744 298 750 
unique_peer() ->
    IPsegments =
        lists:map(
            fun(1) ->
                integer_to_list(rand:uniform(254) + 1);
            (_) ->
                integer_to_list(rand:uniform(255))
            end,
            lists:seq(1, 4)),
    IP = string:join(IPsegments, "."),
    Port = integer_to_list(rand:uniform(65535)),
    list_to_binary(IP ++ ":" ++ Port).


random_hash() ->
    HList =
        lists:map(
            fun(_) -> rand:uniform(255) end,
            lists:seq(1, 32)),
    list_to_binary(HList).
