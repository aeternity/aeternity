%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aega_meta_tx_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../../aecore/include/blocks.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(TEST_MODULE, aetx).

-define(RECIPIENT_PUBKEY, <<"_________recipient_pubkey_______">>).
-define(FORTUNA_ON(Tests),
        begin case aecore_suite_utils:latest_protocol_version() >= ?FORTUNA_PROTOCOL_VSN of
                  true -> Tests;
                  false -> []
              end
        end).
      

%%%-------------------------------------------------------------------
%%% Test that the proper TTL is fetched
%%%-------------------------------------------------------------------
ttl_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpKeysDir) ->
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir)
     end,
     ?FORTUNA_ON(
        [
          {"Ensure that spend TTL is set", fun spend_ttl/0},
          {"Check single meta tx ttl", fun single_meta_ttl/0},
          {"Check nested meta tx ttl", fun nested_meta_ttl/0}
        ])}.

spend_ttl() ->
    TTL = 1234,
    {ok, SpendTx} = spend_tx(#{ttl => TTL}),
    ?assertEqual(TTL, aetx:ttl(SpendTx)),
    ok.

single_meta_ttl() ->
    case aecore_suite_utils:latest_protocol_version() of
        Protocol when Protocol < ?IRIS_PROTOCOL_VSN ->
            SpendTTL = 123,
            {ok, SpendTx} = spend_tx(#{ttl => SpendTTL,
                                       nonce => 0}),
            MetaTTL = 456,
            {ok, Meta} = meta(aetx_sign:new(SpendTx, []), #{ttl => MetaTTL}),
            ?assertEqual(MetaTTL, aetx:ttl(Meta));
        _ ->
            SpendTTL = 123,
            {ok, SpendTx} = spend_tx(#{ttl => SpendTTL,
                                       nonce => 0}),
            {ok, Meta} = meta(aetx_sign:new(SpendTx, []), #{}),
            ?assertEqual(SpendTTL, aetx:ttl(Meta))
    end.

nested_meta_ttl() ->
    case aecore_suite_utils:latest_protocol_version() of
        Protocol when Protocol < ?IRIS_PROTOCOL_VSN ->
            SpendTTL = 123,
            {ok, SpendTx} = spend_tx(#{ttl => SpendTTL,
                                       nonce => 0}),
            InnerMetaTTL = 456,
            {ok, InnerMeta} = meta(aetx_sign:new(SpendTx, []), #{ttl => InnerMetaTTL}),
            OuterMetaTTL = 789,
            {ok, Meta} = meta(aetx_sign:new(InnerMeta, []), #{ttl => OuterMetaTTL}),
            ?assertEqual(OuterMetaTTL, aetx:ttl(Meta));
        _ ->
            SpendTTL = 123,
            {ok, SpendTx} = spend_tx(#{ttl => SpendTTL,
                                       nonce => 0}),
            {ok, InnerMeta} = meta(aetx_sign:new(SpendTx, []), #{}),
            {ok, Meta} = meta(aetx_sign:new(InnerMeta, []), #{}),
            ?assertEqual(SpendTTL, aetx:ttl(Meta))
    end.

%%%-------------------------------------------------------------------
%%% Test versioning
%%%-------------------------------------------------------------------
version_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpKeysDir) ->
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir)
     end,
     ?FORTUNA_ON(
        [
          {"Ensure old meta tx is valid pre-Iris", fun allow_ttl_pre_iris/0},
          {"Ensure new meta tx is invalid pre-Iris", fun require_ttl_pre_iris/0},
          {"Ensure old meta tx is invalid post Iris", fun forbid_ttl_post_iris/0},
          {"Ensure new meta tx is valid post Iris", fun no_ttl_post_iris/0}
        ])}.

allow_ttl_pre_iris() ->
    ttl_iris(allowed, pre_iris, has_ttl).

require_ttl_pre_iris() ->
    ttl_iris(forbidden, pre_iris, no_ttl).

forbid_ttl_post_iris() ->
    ttl_iris(forbidden, post_iris, has_ttl).

no_ttl_post_iris() ->
    ttl_iris(allowed, post_iris, no_ttl).


ttl_iris(Allowed, Iris, HasTTL) ->
    PreIris =
        case Iris of
            pre_iris -> true;
            post_iris -> false
        end,
    IsAllowed =
        case Allowed of
            allowed -> true;
            forbidden -> false
        end,
    case aecore_suite_utils:latest_protocol_version() of
        Protocol when (Protocol < ?IRIS_PROTOCOL_VSN) =:= PreIris ->
            SpendTTL = 123,
            {ok, SpendTx} = spend_tx(#{ttl => SpendTTL,
                                        nonce => 0}),
            Opts =
                case HasTTL of
                    has_ttl ->
                        #{ttl => 456};
                    no_ttl -> #{}
                end,
            {ok, Meta} = meta(aetx_sign:new(SpendTx, []), Opts),
            IsAllowed = aetx:valid_at_protocol(Protocol, Meta);
        _PostIris -> ok
    end.


%%%-------------------------------------------------------------------
%%% Helpers
%%%-------------------------------------------------------------------

spend_tx(Opts) ->
    {ok, MinerPubkey} = aec_keys:pubkey(),
    DefaultOpts =
        #{sender_id => aeser_id:create(account, MinerPubkey),
          recipient_id => aeser_id:create(account, ?RECIPIENT_PUBKEY),
          amount => 40,
          fee => 20000,
          ttl => 100,
          nonce => 11,
          payload => <<"">>},
    {ok, _SpendTx} = aec_spend_tx:new(maps:merge(DefaultOpts, Opts)).

meta(InnerTx, Opts) ->
    {ok, MinerPubkey} = aec_keys:pubkey(),
    Protocol = aecore_suite_utils:latest_protocol_version(),
    DefaultOpts =
        #{ga_id       => aeser_id:create(account, MinerPubkey),
          auth_data   => <<"fake_auth_data">>,
          abi_version => 1,
          gas         => 10000,
          gas_price   => aec_governance:minimum_gas_price(Protocol),
          fee         => 20000,
          tx          => InnerTx},
    {ok, _MetaTx} = aega_meta_tx:new(maps:merge(DefaultOpts, Opts)).

