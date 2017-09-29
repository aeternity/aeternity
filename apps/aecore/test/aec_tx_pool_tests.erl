-module(aec_tx_pool_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("txs.hrl").

all_test_() ->
    {foreach,
     fun() ->
             {ok, _} = aec_tx_pool:start_link(),
             %% Start `aec_keys` merely for generating realistic test
             %% signed txs - as a node would do.
             TmpKeysDir = mktempd(),
             ok = application:ensure_started(crypto),
             {ok, _} = aec_keys:start_link(["mypassword", TmpKeysDir]),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = aec_keys:stop(),
             ok = application:stop(crypto),
             {ok, KeyFiles} = file:list_dir(TmpKeysDir),
             %% Expect two filenames - private and public keys.
             [_KF1, _KF2] = KeyFiles,
             lists:foreach(
               fun(F) ->
                       AbsF = filename:absname_join(TmpKeysDir, F),
                       {ok, _} = {file:delete(AbsF), {F, AbsF}}
               end,
               KeyFiles),
             ok = file:del_dir(TmpKeysDir),
             ok = aec_tx_pool:stop()
     end,
     [{"No txs in mempool",
       fun() ->
               ?assertEqual({ok, []}, aec_tx_pool:pop(1)),
               ?assertEqual({ok, []}, aec_tx_pool:pop(5)),

               ?assertEqual({ok, []}, aec_tx_pool:peek(1)),
               ?assertEqual({ok, []}, aec_tx_pool:peek(3)),

               STx1 = a_signed_tx(<<"pk1">>, 1, 1),
               ?assertEqual(ok, aec_tx_pool:delete(STx1))
       end},
      {"As a healthy network peer, the node stores in mempool txs received from peers and serves txs in mempool to peers",
       fun() ->
               %% No txs to serve to peers.
               ?assertEqual({ok, []}, aec_tx_pool:peek(1)),

               %% Tx received from a peer.
               STx1 = a_signed_tx(<<"pk1">>, 1, 1),
               ?assertEqual(ok, aec_tx_pool:push(STx1)),

               %% One tx to serve to peers.
               ?assertEqual({ok, [STx1]}, aec_tx_pool:peek(1)),

               %% Other tx received from a peer.
               STx2 = a_signed_tx(<<"pk2">>, 1, 2),
               ?assertEqual(ok, aec_tx_pool:push(STx2)),

               %% Two tx2 to serve to peers.
               ?assertEqual({ok, [STx2]}, aec_tx_pool:peek(1))
       end},
      {"Mempool follows longest chain",
       fun() ->
               %% Prepare a few txs.
               STx1 = a_signed_tx(<<"pk1">>, 1, 1),
               STx2 = a_signed_tx(<<"pk2">>, 1, 1),
               STx3 = a_signed_tx(<<"pk3">>, 1, 1),
               STx4 = a_signed_tx(<<"pk4">>, 1, 1),
               STx5 = a_signed_tx(<<"pk5">>, 1, 1),

               %% Some txs received from peers.
               ?assertEqual(ok, aec_tx_pool:push(STx2)),
               ?assertEqual(ok, aec_tx_pool:push(STx4)),
               ?assertMatch({ok, [_,_]}, aec_tx_pool:peek(10)),

               %% A block inserted in chain.
               ?assertEqual(ok, aec_tx_pool:delete(STx1)),
               ?assertEqual(ok, aec_tx_pool:delete(STx2)),
               ?assertEqual({ok, [STx4]}, aec_tx_pool:peek(10)),

               %% A tx already in chain now received from peers.
               ?assertEqual(ok, aec_tx_pool:push(STx2)),
               ?assertMatch({ok, [_,_]}, aec_tx_pool:peek(10)),

               %% A block orphaned...
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               ?assertEqual(ok, aec_tx_pool:push(STx2)), %% Tx already in pool.
               ?assertMatch({ok, [_,_,_]}, aec_tx_pool:peek(10)),
               %% ... two new blocks inserted in longest chain.
               ?assertEqual(ok, aec_tx_pool:delete(STx3)),
               ?assertEqual(ok, aec_tx_pool:delete(STx4)),
               ?assertEqual(ok, aec_tx_pool:delete(STx5)),
               ?assertMatch({ok, [_,_]}, aec_tx_pool:peek(10))
       end},
      {"As a miner, the node stores in mempool txs received from peers and includes txs from mempool in mined block",
       fun() ->
               %% Simplistic parameter representing maximum number of
               %% txs in block excluding coinbase - for easing test.
               MaxTxs = 2,

               %% No txs to include in block (apart from coinbase).
               ?assertEqual({ok, []}, aec_tx_pool:pop(MaxTxs)),

               %% Tx received from a peer ...
               STx1 = a_signed_tx(<<"pk1">>, 1, 1),
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               %% ... hence only one tx to include in block.
               ?assertEqual({ok, [STx1]}, aec_tx_pool:pop(2 = MaxTxs)),

               %% Two txs received from peers ...
               STx2 = a_signed_tx(<<"pk2">>, 1, 1),
               STx3 = a_signed_tx(<<"pk3">>, 1, 2),
               ?assertEqual(ok, aec_tx_pool:push(STx2)),
               ?assertEqual(ok, aec_tx_pool:push(STx3)),
               %% ... hence exactly two txs to include in block.
               {ok, TwoTxs} = aec_tx_pool:pop(2 = MaxTxs),
               ?assertEqual([STx3, STx2], TwoTxs),

               %% A lot of txs received from peers ...
               ALotOfTxs = [STx11 = a_signed_tx(<<"pk11">>, 1, 8),
                            STx12 = a_signed_tx(<<"pk12">>, 1, 2),
                            STx13 = a_signed_tx(<<"pk13">>, 1, 5)],
               lists:foreach(
                 fun(T) ->
                         Context = {{tx_being_pushed_to_pool, T},
                                    {all_txs_being_pushed_to_pool, ALotOfTxs}},
                         ?assertMatch({ok, _},
                                      {aec_tx_pool:push(T), Context})
                 end,
                 ALotOfTxs),
               %% ... but still only two txs to be included in block.
               {ok, StillOnlyTwoTxs} = aec_tx_pool:pop(2 = MaxTxs),
               ?assertEqual([STx11, STx13], StillOnlyTwoTxs),
               lists:foreach(
                 fun(T) ->
                         Context = {{tx_from_pool, T},
                                    {txs_in_pool, StillOnlyTwoTxs},
                                    {all_txs_in_pool, ALotOfTxs}},
                         ?assertMatch({true, _},
                                      {lists:member(T, ALotOfTxs), Context})
                 end,
                 StillOnlyTwoTxs),

               %% One tx left to be included in block...
               ?assertEqual({ok, [STx12]}, aec_tx_pool:pop(2 = MaxTxs)),
               %% ... now none.
               ?assertEqual({ok, []}, aec_tx_pool:pop(2 = MaxTxs))
      end}]}.

a_signed_tx(Account, AccountNonce, Fee) ->
    Tx = #spend_tx{from = Account, nonce = AccountNonce, fee = Fee},
    {ok, STx} = aec_keys:sign(Tx),
    STx.

mktempd() ->
    mktempd(os:type()).

mktempd({unix, _}) ->
    lib:nonl(?cmd("mktemp -d")).
