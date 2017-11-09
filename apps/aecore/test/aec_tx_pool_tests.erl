-module(aec_tx_pool_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("txs.hrl").

-define(TAB, aec_tx_pool_test_keys).

all_test_() ->
    {foreach,
     fun() ->
             application:ensure_started(gproc),
             ok = application:ensure_started(crypto),
             TmpKeysDir = mktempd(),
             {ok, _} = aec_keys:start_link(["mypassword", TmpKeysDir]),
             R2 = aec_keys:open("mypassword"),
             ok = R2,
             {ok, _} = aec_tx_pool:start_link(),
             %% Start `aec_keys` merely for generating realistic test
             %% signed txs - as a node would do.
             ets:new(?TAB, [public, ordered_set, named_table]),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = aec_keys:stop(),
             ok = application:stop(gproc),
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
             ets:delete(?TAB),
             ok = aec_tx_pool:stop()
     end,
     [{"No txs in mempool",
       fun() ->
               ?assertEqual({ok, []}, aec_tx_pool:peek(1)),
               ?assertEqual({ok, []}, aec_tx_pool:peek(3)),

               STx1 = a_signed_tx(me, new_pubkey(), 1, 1),
               ?assertEqual(ok, aec_tx_pool:delete(STx1))
       end},
      {"As a healthy network peer, the node stores in mempool txs received from peers and serves txs in mempool to peers",
       fun() ->
               %% No txs to serve to peers.
               ?assertEqual({ok, []}, aec_tx_pool:peek(1)),

               %% Tx received from a peer.
               STx1 = a_signed_tx(new_pubkey(), me, 1, 1),
               ?assertEqual(ok, aec_tx_pool:push(STx1, tx_received)),

               %% One tx to serve to peers.
               ?assertEqual({ok, [STx1]}, aec_tx_pool:peek(1)),

               %% Other tx received from a peer.
               STx2 = a_signed_tx(new_pubkey(), me, 1, 2),
               ?assertEqual(ok, aec_tx_pool:push(STx2, tx_received)),

               %% Two tx2 to serve to peers.
               ?assertEqual({ok, [STx2]}, aec_tx_pool:peek(1))
       end},
      {"Mempool follows longest chain",
       fun() ->
               %% Prepare a few txs.
               STx1 = a_signed_tx(me, new_pubkey(), 1, 1),
               STx2 = a_signed_tx(me, new_pubkey(), 2, 1),
               STx3 = a_signed_tx(me, new_pubkey(), 3, 1),
               STx4 = a_signed_tx(me, new_pubkey(), 4, 1),
               STx5 = a_signed_tx(me, new_pubkey(), 5, 1),

               %% Some txs received from peers.
               ?assertEqual(ok, aec_tx_pool:push(STx2)),
               ?assertEqual(ok, aec_tx_pool:push(STx4)),
               ?assertMatch({ok, [_,_]}, aec_tx_pool:peek(10)),

               %% A block inserted in chain.
               ?assertEqual(ok, aec_tx_pool:delete(STx1)),
               ?assertEqual(ok, aec_tx_pool:delete(STx2)),
               ?assertEqual({ok, [STx4]}, aec_tx_pool:peek(10)),

               %% A tx already in chain now received from peers.
               ?assertEqual(ok, aec_tx_pool:push(STx2, tx_received)),
               ?assertMatch({ok, [_,_]}, aec_tx_pool:peek(10)),

               %% A block orphaned...
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               %% ?assertEqual({error, already_exists},
               %%              aec_tx_pool:push(STx2)), %% Tx already in pool.
               ?assertMatch({ok, [_,_,_]}, aec_tx_pool:peek(10)),
               %% ... two new blocks inserted in longest chain.
               ?assertEqual(ok, aec_tx_pool:delete(STx3)),
               ?assertEqual(ok, aec_tx_pool:delete(STx4)),
               ?assertEqual(ok, aec_tx_pool:delete(STx5)),
               ?assertMatch({ok, [_,_]}, aec_tx_pool:peek(10))
       end},
      {"Ensure ordering",
             fun() ->
                 %% We should sort by fee, but preserve the order of nonces for each sender
                 PK1 = new_pubkey(),
                 PK2 = new_pubkey(),
                 PK3 = new_pubkey(),
                 STx1 = a_signed_tx(PK1, me, 1, 1),
                 STx2 = a_signed_tx(PK1, me, 2, 2),
                 STx3 = a_signed_tx(PK1, me, 3, 3),
                 STx4 = a_signed_tx(PK2, me, 2, 5),
                 STx5 = a_signed_tx(PK2, me, 1, 6),

                 [?assertEqual(ok, aec_tx_pool:push(Tx)) || Tx <- [STx1, STx2, STx3, STx4, STx5]],
                 {ok, CurrentMempool} = aec_tx_pool:peek(10),

                 MempoolOrder = [{Sender, Nonce} || #signed_tx{data=#spend_tx{sender=Sender, nonce=Nonce}}
                                                    <- CurrentMempool],
                 %% this is not-optimal order: transactions for PK1 are invalid in that order
                 CorrectOrder = [{PK2,1},{PK2,2},{PK1,3},{PK1,2},{PK1,1}],

                 ?assertEqual(CorrectOrder, MempoolOrder),

                 %% check if we track nonces correctly
                 MaxNonce = aec_tx_pool:get_max_nonce(PK1),
                 ?assertEqual({ok,3}, MaxNonce),

                 NotExistingSender = aec_tx_pool:get_max_nonce(PK3),
                 ?assertEqual(undefined, NotExistingSender)
             end},
      {"As a miner, the node stores in mempool txs received from peers and includes txs from mempool in mined block",
       fun() ->
               %% Simplistic parameter representing maximum number of
               %% txs in block excluding coinbase - for easing test.
               MaxTxs = 2,

               %% No txs to include in block (apart from coinbase).
               ?assertEqual({ok, []}, aec_tx_pool:peek(MaxTxs)),

               %% Tx received from a peer ...
               STx1 = a_signed_tx(new_pubkey(), me, 1, 1),
               ?assertEqual(ok, aec_tx_pool:push(STx1, tx_received)),
               %% ... hence only one tx to include in block ...
               ?assertEqual({ok, [STx1]}, aec_tx_pool:peek(2 = MaxTxs)),
               %% ... that gets successfully mined and inserted in
               %% chain.
               ?assertEqual(ok, aec_tx_pool:delete(STx1)),

               %% Two txs received from peers ...
               STx2 = a_signed_tx(new_pubkey(), me, 1, 1),
               STx3 = a_signed_tx(new_pubkey(), me, 1, 2),
               ?assertEqual(ok, aec_tx_pool:push(STx2, tx_received)),
               ?assertEqual(ok, aec_tx_pool:push(STx3, tx_received)),
               %% ... hence exactly two txs to include in block ...
               ?assertEqual({ok, [STx3, STx2]}, aec_tx_pool:peek(2 = MaxTxs)),
               %% ... that gets successfully mined and inserted in
               %% chain.
               ?assertEqual(ok, aec_tx_pool:delete(STx3)),
               ?assertEqual(ok, aec_tx_pool:delete(STx2)),

               %% A lot of txs received from peers ...
               STx11 = a_signed_tx(new_pubkey(), me, 1, 8),
               STx12 = a_signed_tx(new_pubkey(), me, 2, 2),
               STx13 = a_signed_tx(new_pubkey(), me, 3, 5),
               ?assertEqual(ok, aec_tx_pool:push(STx11, tx_received)),
               ?assertEqual(ok, aec_tx_pool:push(STx12, tx_received)),
               ?assertEqual(ok, aec_tx_pool:push(STx13, tx_received)),
               %% ... but still only two txs to be included in block ...
               ?assertEqual({ok, [STx11, STx13]}, aec_tx_pool:peek(2 = MaxTxs)),
               %% ... that gets successfully mined and inserted in
               %% chain.
               ?assertEqual(ok, aec_tx_pool:delete(STx11)),
               ?assertEqual(ok, aec_tx_pool:delete(STx13)),

               %% One tx left to be included in block...
               ?assertEqual({ok, [STx12]}, aec_tx_pool:peek(2 = MaxTxs)),
               %% ... that gets successfully mined and inserted in
               %% chain ...
               ?assertEqual(ok, aec_tx_pool:delete(STx12)),
               %% ... so now none.
               ?assertEqual({ok, []}, aec_tx_pool:peek(2 = MaxTxs))
      end}]}.

a_signed_tx(Sender, Recipient, Nonce, Fee) ->
    Tx = #spend_tx{sender = acct(Sender),
                   recipient = acct(Recipient),
                   nonce = Nonce, fee = Fee},
    {ok, STx} = sign(Sender, Tx),
    STx.

sign(me, Tx) ->
    aec_keys:sign(Tx);
sign(PubKey, Tx) ->
    try
        [{_, PrivKey}] = ets:lookup(?TAB, PubKey),
        Signers = aec_tx:signers(Tx),
        true = lists:member(PubKey, Signers),
        Bin = aec_tx:serialize_to_binary(Tx),
        Sig = crypto:sign(algo(), digest(), Bin, [PrivKey, curve()]),
        {ok, #signed_tx{data = Tx,
                        signatures = [Sig]}}
    catch
        error:Err ->
            erlang:error({Err, erlang:stacktrace()})
    end.

algo()   -> ecdsa.
digest() -> sha256.
curve()  -> crypto:ec_curve(secp256k1).

acct(me) ->
    {ok, Key} = aec_keys:pubkey(),
    Key;
acct(A) when is_binary(A) ->
    A.

mktempd() ->
    mktempd(os:type()).

mktempd({unix, _}) ->
    lib:nonl(?cmd("mktemp -d")).

new_pubkey() ->
    {Pub, Priv} = keypair(),
    ets:insert(?TAB, {Pub, Priv}),
    Pub.

keypair() ->
    crypto:generate_key(ecdh, crypto:ec_curve(secp256k1)).
