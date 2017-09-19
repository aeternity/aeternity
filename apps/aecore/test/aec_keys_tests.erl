%%%-------------------------------------------------------------------
%%% @author sennui
%%% @copyright (C) 2017, Aeternity
%%% @doc
%%%
%%% @end
%%% Created : 04. Sep 2017 23:31
%%%-------------------------------------------------------------------
-module(aec_keys_tests).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_PUB, <<4,130,41,165,13,201,185,26,2,151,146,68,56,108,22,242,94,157,95,191,140,86,
                    145,96,71,82,28,176,23,5,128,17,245,174,170,199,54,248,167,43,185,12,108,91,
                    107,188,126,242,98,36,211,79,105,50,16,124,227,93,228,142,83,163,126,167,206>>).

-define(TEST_PRIV, <<116,214,52,147,205,225,149,14,95,228,19,253,76,49,82,53,18,39,12,254,53,
                     220,210,199,152,25,208,77,217,186,141,212>>).

all_test_() ->
    {foreach,
     fun() ->
             TmpKeysDir = mktempd(),
             ok = application:ensure_started(crypto),
             {ok, _} = aec_keys:start_link(["mypassword", TmpKeysDir]),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = aec_keys:stop(),
             ok = application:stop(crypto),
             file:delete(TmpKeysDir)
     end,
     [fun(TmpKeysDir) ->
              [{"Sign coinbase transaction",
                fun() ->
                        {ok, PubKey} = aec_keys:pubkey(),
                        {ok, Tx} =
                            aec_coinbase_tx:new(#{account => PubKey},
                                                unused_trees_argument),
                        {ok, SignedTx} = aec_keys:sign(Tx),
                        ?assertEqual(Tx, aec_tx_sign:data(SignedTx))
                end}]
      end]}.

mktempd() ->
    mktempd(os:type()).

mktempd({unix, _}) ->
    lib:nonl(?cmd("mktemp -d")).

-endif.
