%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    aetx:used_gas/4 for a generalized-account meta transaction. #aetx.size
%%%    is the whole envelope, so the recursion into the inner transaction
%%%    charged its bytes twice. Gated at Arcus: producer and validator both
%%%    size a micro block with this, so a lower fix would move old blocks.
%%% @end
%%%-------------------------------------------------------------------
-module(aega_meta_tx_used_gas_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(GA_PUBKEY,        <<16#6A:256>>).
-define(AUTH_CONTRACT_PK, <<16#C0:256>>).
-define(RECIPIENT_PUBKEY, <<16#4E:256>>).
-define(AUTH_FUN,         <<16#F0:256>>).
-define(AUTH_DATA,        <<"authenticate-me">>).
-define(AUTH_GAS,         3000).
-define(HEIGHT,           1000).
-define(ABI,              ?ABI_FATE_SOPHIA_1).

%% Payload lengths chosen so the inner spend transaction lands near the sizes
%% the audit quoted (200 / 500 / 1000 bytes). The assertions below are written
%% against the measured size, not against these.
-define(PAYLOAD_SIZES, [0, 100, 400, 900]).

%%%===================================================================
%%% Pre-Arcus: unchanged, double count and all
%%%===================================================================

%% Every protocol below Arcus charges what it always charged: the whole
%% envelope, plus the inner transaction which pays for its own bytes again.
pre_arcus_is_unchanged_test_() ->
    [ {lists:flatten(io_lib:format("~w byte payload at protocol ~w", [PS, V])),
       fun() ->
           #{ meta := Meta, inner := Inner, trees := Trees, size := Size } =
               scenario(PS, ok),
           ?assertEqual(base_gas(V) + byte_gas() * Size + ?AUTH_GAS
                            + aetx:gas_limit(Inner, ?HEIGHT, V),
                        aetx:used_gas(Meta, ?HEIGHT, V, Trees))
       end}
      || PS <- ?PAYLOAD_SIZES,
         V  <- [?FORTUNA_PROTOCOL_VSN, ?LIMA_PROTOCOL_VSN,
                ?IRIS_PROTOCOL_VSN, ?CERES_PROTOCOL_VSN] ].

%%%===================================================================
%%% Arcus: the inner bytes are charged once
%%%===================================================================

arcus_charges_the_inner_bytes_once_test_() ->
    [ {lists:flatten(io_lib:format("~w byte payload", [PS])),
       fun() ->
           #{ meta := Meta, inner := Inner, trees := Trees,
              size := Size, inner_size := ISize } = scenario(PS, ok),
           ?assertEqual(base_gas(?ARCUS_PROTOCOL_VSN)
                            + byte_gas() * (Size - ISize) + ?AUTH_GAS
                            + aetx:gas_limit(Inner, ?HEIGHT, ?ARCUS_PROTOCOL_VSN),
                        aetx:used_gas(Meta, ?HEIGHT, ?ARCUS_PROTOCOL_VSN, Trees))
       end}
      || PS <- ?PAYLOAD_SIZES ].

%% The figure the audit put on it: byte_gas() per inner byte, and not a gas more.
arcus_saves_exactly_one_inner_size_test_() ->
    [ {lists:flatten(io_lib:format("~w byte payload", [PS])),
       fun() ->
           #{ meta := Meta, trees := Trees, inner_size := ISize } = scenario(PS, ok),
           Ceres = aetx:used_gas(Meta, ?HEIGHT, ?CERES_PROTOCOL_VSN, Trees),
           Arcus = aetx:used_gas(Meta, ?HEIGHT, ?ARCUS_PROTOCOL_VSN, Trees),
           ?assertEqual(byte_gas() * ISize, Ceres - Arcus)
       end}
      || PS <- ?PAYLOAD_SIZES ].

%% Salus and anything above it inherit the Arcus figure -- the gate is >=, not =.
above_arcus_inherits_the_correction_test() ->
    #{ meta := Meta, trees := Trees } = scenario(400, ok),
    ?assertEqual(aetx:used_gas(Meta, ?HEIGHT, ?ARCUS_PROTOCOL_VSN, Trees),
                 aetx:used_gas(Meta, ?HEIGHT, ?SALUS_PROTOCOL_VSN, Trees)).

%%%===================================================================
%%% Failed authentication: nothing is netted out
%%%===================================================================

%% With a failed auth call the inner transaction is not applied and never pays
%% for itself, so its bytes are the envelope's to carry. There is no double
%% count to remove, and removing one would let inner bytes ride free -- so this
%% case is identical at every protocol.
failed_auth_is_unchanged_at_arcus_test_() ->
    [ {lists:flatten(io_lib:format("~w byte payload", [PS])),
       fun() ->
           #{ meta := Meta, trees := Trees, size := Size } = scenario(PS, error),
           Expected = fun(V) -> base_gas(V) + byte_gas() * Size + ?AUTH_GAS end,
           ?assertEqual(Expected(?CERES_PROTOCOL_VSN),
                        aetx:used_gas(Meta, ?HEIGHT, ?CERES_PROTOCOL_VSN, Trees)),
           ?assertEqual(Expected(?ARCUS_PROTOCOL_VSN),
                        aetx:used_gas(Meta, ?HEIGHT, ?ARCUS_PROTOCOL_VSN, Trees))
       end}
      || PS <- ?PAYLOAD_SIZES ].

%%%===================================================================
%%% The shape the correction lines up with
%%%===================================================================

%% fee_gas/3 has netted the inner size out since Iris. used_gas/4 diverging from
%% it is what the fix closes; this pins the two together at Arcus so a later
%% edit to one of them cannot silently part from the other.
arcus_used_gas_matches_fee_gas_envelope_share_test() ->
    #{ meta := Meta, inner := Inner, trees := Trees } = scenario(400, ok),
    Arcus = aetx:used_gas(Meta, ?HEIGHT, ?ARCUS_PROTOCOL_VSN, Trees),
    FeeGas = aetx:fee_gas(Meta, ?HEIGHT, ?ARCUS_PROTOCOL_VSN),
    ?assertEqual(FeeGas + ?AUTH_GAS + aetx:gas_limit(Inner, ?HEIGHT, ?ARCUS_PROTOCOL_VSN),
                 Arcus).

%%%===================================================================
%%% Fixture
%%%===================================================================

base_gas(Version) ->
    aec_governance:tx_base_gas(ga_meta_tx, Version, ?ABI).

byte_gas() ->
    aec_governance:byte_gas().

%% A ga_meta_tx wrapping a spend transaction of a given payload size, over
%% trees holding the generalized account and its auth call with AuthResult as
%% the call's return type.
scenario(PayloadSize, AuthResult) ->
    {ok, SpendTx} =
        aec_spend_tx:new(#{ sender_id    => aeser_id:create(account, ?GA_PUBKEY)
                          , recipient_id => aeser_id:create(account, ?RECIPIENT_PUBKEY)
                          , amount       => 40
                          , fee          => 20000
                          , ttl          => 0
                          , nonce        => 0
                          , payload      => binary:copy(<<$p>>, PayloadSize)
                          }),
    SignedInner = aetx_sign:new(SpendTx, []),
    {ok, MetaTx} =
        aega_meta_tx:new(#{ ga_id       => aeser_id:create(account, ?GA_PUBKEY)
                          , auth_data   => ?AUTH_DATA
                          , abi_version => ?ABI
                          , gas         => 20000
                          , gas_price   => 1000000
                          , fee         => 1000000
                          , tx          => SignedInner
                          }),
    #{ meta       => MetaTx
     , inner      => SpendTx
     , trees      => trees(AuthResult)
     , size       => aetx:size(MetaTx)
     , inner_size => aetx:size(SpendTx)
     }.

trees(AuthResult) ->
    Trees0 = aec_trees:new_without_backend(),
    {ok, GAAccount} =
        aec_accounts:attach_ga_contract(
          aec_accounts:new(?GA_PUBKEY, 1000000000000),
          aeser_id:create(contract, ?AUTH_CONTRACT_PK), ?AUTH_FUN),
    Trees1 = aec_trees:set_accounts(
               Trees0, aec_accounts_trees:enter(GAAccount, aec_trees:accounts(Trees0))),
    aec_trees:set_calls(Trees1,
                        aect_call_state_tree:enter_auth_call(
                          auth_call(AuthResult), aec_trees:calls(Trees1))).

%% The auth call as aeprimop stores it: keyed under the generalized account,
%% with the id aega_meta_tx:call_id/2 recomputes from the auth data.
auth_call(AuthResult) ->
    Call0 = aect_call:new(aeser_id:create(account, ?GA_PUBKEY), _Nonce = 0,
                          aeser_id:create(contract, ?AUTH_CONTRACT_PK),
                          ?HEIGHT, _GasPrice = 1000000),
    AuthId = aega_meta_tx:auth_id(?GA_PUBKEY, ?AUTH_DATA),
    Call1 = aect_call:set_id(aect_call:ga_id(AuthId, ?AUTH_CONTRACT_PK), Call0),
    aect_call:set_gas_used(?AUTH_GAS, aect_call:set_return_type(AuthResult, Call1)).
