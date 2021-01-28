-module(aesc_utils_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

get_channel_test_() ->
    {foreach,
     fun() ->
         ok
     end,
     [ {"Get missing channel from empty trees", fun get_channel_from_empty_trees/0}
     , {"Get missing channel from non empty trees", fun get_missing_channel_from_non_empty_trees/0}
     , {"Get channel", fun get_channel/0}
     ]
    }.

checks_test_() ->
    {foreach,
     fun() ->
         ok
     end,
     [ {"Check is active: yes", fun check_is_active_yes/0}
     , {"Check is active: no", fun check_is_active_no/0}
     , {"Check payload round: last is non-FP", fun check_round_not_fp/0}
     , {"Check payload round: last is FP", fun check_round_fp/0}
     , {"Check is peer", fun check_is_peer/0}
     ]
    }.

deserialize_payload_test_() ->
    {foreach,
     fun() ->
         ok
     end,
     [ {"Deserialize empty", fun deserialize_empty_payload/0}
     , {"Deserialize correct payload", fun deserialize_correct_payload/0}
     , {"Deserialize payload from a different type", fun
        deserialize_payload_different_type/0}
     ]
    }.

check_solo_close_payload_test_() ->
    {foreach,
     fun() ->
         ok
     end,
     [ {"Deserialize broken close solo payload", fun deserialize_broken_close_solo_payload/0}
     , {"Payload from a missing channel",
        fun check_solo_payload_from_a_missing_channel/0}
     , {"Sender does not enough tokens", fun check_solo_not_enough_funds/0}
     , {"Sender is not a peer", fun check_solo_not_a_peer/0}
     , {"Channel is already closing", fun check_solo_already_closing/0}
     , {"Empty payload with root hash mismatch", fun check_solo_close_empty_payload_hash_mismatch/0}
     , {"Empty payload with missing accounts",
        fun check_solo_close_empty_payload_missing_accounts/0}
     , {"Empty payload with wrong amounts",
        fun check_solo_close_empty_payload_wrong_amounts/0}
     , {"Empty payload with less amounts",
        fun check_solo_close_empty_payload_less_amounts/0}
     , {"Empty payload with exact amounts",
        fun check_solo_close_empty_payload_exact_amounts/0}
     , {"Payload with wrong channel ID", fun check_solo_close_payload_wrong_channel_id/0}
     , {"Payload with wrong round", fun check_solo_close_payload_wrong_round/0}
     , {"Payload with wrong signatures", fun check_solo_close_payload_wrong_signatures/0}
     , {"Payload with wrong peers", fun check_solo_close_payload_wrong_peers/0}
     , {"Payload with wrong amounts", fun check_solo_close_payload_wrong_amounts/0}
     , {"Correct payload", fun check_solo_close_payload_correct/0}
     , {"Correct payload with more data in poi",
        fun check_solo_close_payload_correct_bigger_poi/0}
     ]
    }.

check_slash_payload_test_() ->
    {foreach,
     fun() ->
         ok
     end,
     [ {"Deserialize broken slash payload", fun deserialize_broken_slash_payload/0}
     , {"Payload from a missing channel",
        fun check_slash_from_a_missing_channel/0}
     , {"Fail on last_onchain", fun check_slash_payload_last_onchain/0}
     , {"Fail on not closing channel", fun check_slash_payload_not_closing/0}
     , {"Sender does not enough tokens", fun check_slash_not_enough_funds/0}
     , {"Sender is not a peer", fun check_slash_not_a_peer/0}
     , {"Payload with wrong channel ID", fun check_slash_payload_wrong_channel_id/0}
     , {"Payload with wrong round", fun check_slash_payload_wrong_round/0}
     , {"Payload with wrong signatures", fun check_slash_payload_wrong_signatures/0}
     , {"Payload with wrong peers", fun check_slash_payload_wrong_peers/0}
     , {"Payload with wrong amounts", fun check_slash_payload_wrong_amounts/0}
     , {"Correct payload", fun check_slash_payload_correct/0}
     , {"Correct payload can replace forced progress state",
        fun check_slash_payload_correct_after_wrong_fp/0}
     , {"Delegate can slash",
        fun check_slash_payload_correct_delegate/0}
     ]
    }.

check_solo_snapshot_payload_test_() ->
    {foreach,
     fun() ->
         ok
     end,
     [ {"Deserialize broken close solo snapshot payload", fun deserialize_broken_solo_snapshot_payload/0}
     , {"Payload from a missing channel",
        fun check_solo_snapshot_from_a_missing_channel/0}
     , {"Fail on last_onchain", fun check_solo_snapshot_payload_last_onchain/0}
     , {"Sender does not enough tokens", fun check_solo_snapshot_not_enough_funds/0}
     , {"Sender is not a peer", fun check_solo_snapshot_not_a_peer/0}
     , {"Channel is already closing", fun check_solo_snapshot_already_closing/0}
     , {"Payload with wrong channel ID", fun check_solo_snapshot_payload_wrong_channel_id/0}
     , {"Payload with wrong signatures", fun check_solo_snapshot_payload_wrong_signatures/0}
     , {"Correct payload", fun check_solo_snapshot_payload_correct/0}
     ]
    }.

check_fp_payload_test_() ->
    {foreach,
     fun() ->
         ok
     end,
     [ {"Deserialize broken close force progress payload", fun deserialize_broken_fp_payload/0}
     , {"Payload from a missing channel", fun check_fp_from_a_missing_channel/0}
     ]
    }.

get_channel_from_empty_trees() ->
    Trees = aec_trees:new_without_backend(),
    BogusChannelId = <<42:32/unit:8>>,
    {error, channel_does_not_exist} = aesc_utils:get_channel(BogusChannelId,
                                                             Trees),
    ok.

get_missing_channel_from_non_empty_trees() ->
    Trees0 = aec_trees:new_without_backend(),
    Channel = new_channel(),
    Trees = set_channel(Channel, Trees0),
    BogusChannelId = <<42:32/unit:8>>,
    {error, channel_does_not_exist} = aesc_utils:get_channel(BogusChannelId,
                                                             Trees),
    ok.

get_channel() ->
    Trees0 = aec_trees:new_without_backend(),
    Channel = new_channel(),
    Trees = set_channel(Channel, Trees0),
    Pubkey = aesc_channels:pubkey(Channel),
    {ok, Channel} = aesc_utils:get_channel(Pubkey, Trees),
    ok.


check_is_active_yes() ->
    Channel = new_channel(),
    ok = aesc_utils:check_is_active(Channel),
    ok.

check_is_active_no() ->
    Channel = closing_channel(),
    {error, channel_not_active} = aesc_utils:check_is_active(Channel),
    ok.

check_round_not_fp() ->
    Round = 10,
    Channel = new_channel(#{round => Round}),
    ok = aesc_utils:check_round_greater_than_last(Channel, Round + 1, deposit),
    ok = aesc_utils:check_round_greater_than_last(Channel, Round + 5, deposit),
    {error, same_round}  = aesc_utils:check_round_greater_than_last(Channel, Round, deposit),
    {error, old_round} = aesc_utils:check_round_greater_than_last(Channel, Round - 1, deposit),
    {error, old_round} = aesc_utils:check_round_greater_than_last(Channel, Round - 5, deposit),

    ok = aesc_utils:check_round_greater_than_last(Channel, Round + 1,
                                                  force_progress),
    ok = aesc_utils:check_round_greater_than_last(Channel, Round + 5,
                                                  force_progress),
    {error, same_round}  = aesc_utils:check_round_greater_than_last(Channel,
                                                                    Round,
                                                                    force_progress),
    {error, old_round} = aesc_utils:check_round_greater_than_last(Channel,
                                                                  Round - 1,
                                                                  force_progress),
    {error, old_round} = aesc_utils:check_round_greater_than_last(Channel,
                                                                  Round - 5,
                                                                  force_progress),
    ok.

check_round_fp() ->
    Round = 10,
    SoloRound = 8,
    Channel0 = new_channel(#{round => Round}),
    Channel = aesc_channels:set_solo_round(Channel0, SoloRound),
    ok = aesc_utils:check_round_greater_than_last(Channel, Round + 1, deposit),
    ok = aesc_utils:check_round_greater_than_last(Channel, Round + 5, deposit),
    %% next one is a bit odd: a deposit can overwrite a chain of forced
    %% progressed states
    ok = aesc_utils:check_round_greater_than_last(Channel, Round, deposit),
    ok = aesc_utils:check_round_greater_than_last(Channel, SoloRound, deposit),
    {error, same_round} = aesc_utils:check_round_greater_than_last(Channel, SoloRound - 1, deposit),
    {error, old_round} = aesc_utils:check_round_greater_than_last(Channel,
                                                                  SoloRound - 2, deposit),

    ok = aesc_utils:check_round_greater_than_last(Channel, Round + 1,
                                                  force_progress),
    ok = aesc_utils:check_round_greater_than_last(Channel, Round + 5,
                                                  force_progress),
    {error, same_round}  = aesc_utils:check_round_greater_than_last(Channel,
                                                                    Round,
                                                                    force_progress),
    {error, old_round} = aesc_utils:check_round_greater_than_last(Channel,
                                                                  Round - 1,
                                                                  force_progress),
    {error, old_round} = aesc_utils:check_round_greater_than_last(Channel,
                                                                  Round - 5,
                                                                  force_progress),
    ok.

check_is_peer() ->
    Alice = <<1:32/unit:8>>,
    Bob = <<2:32/unit:8>>,
    Carol = <<3:32/unit:8>>,
    Peers = [Alice, Bob],
    ok = aesc_utils:check_is_peer(Alice, Peers),
    ok = aesc_utils:check_is_peer(Bob, Peers),
    {error, account_not_peer} = aesc_utils:check_is_peer(Carol, Peers),
    ok.

deserialize_empty_payload() ->
    {ok, last_onchain} = aesc_utils:deserialize_payload(<<>>).

deserialize_correct_payload() ->
    {Bin, SignedTx, OffChainTx} = payload(),
    {ok, SignedTx, OffChainTx} = aesc_utils:deserialize_payload(Bin),
    ok.

deserialize_payload_different_type() ->
    {error, payload_deserialization_failed} =
        aesc_utils:deserialize_payload(<<"a">>),
    SignedTx = spend_tx(),
    Bin = aetx_sign:serialize_to_binary(SignedTx),
    {error, bad_offchain_state_type} =
        aesc_utils:deserialize_payload(Bin),
    ok.

deserialize_broken_close_solo_payload() ->
    FromPubKey = <<42:32/unit:8>>,
    Trees = aec_trees:new_without_backend(),
    PoI = aec_trees:new_poi(Trees),
    Channel = new_channel(),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    {error, payload_deserialization_failed} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee = 1,
                                            <<"a">>,
                                            PoI,
                                            OnChainTrees,
                                            tx_env()),
    SignedTx = spend_tx(),
    Bin = aetx_sign:serialize_to_binary(SignedTx),
    {error, bad_offchain_state_type} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee = 1,
                                            Bin,
                                            PoI,
                                            OnChainTrees,
                                            tx_env()),
    ok.

check_solo_payload_from_a_missing_channel() ->
    Trees = aec_trees:new_without_backend(),
    PoI = aec_trees:new_poi(Trees),
    Channel = new_channel(),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = aec_trees:new_without_backend(),
    FromPubKey = <<42:32/unit:8>>,
    {Bin, _SignedTx, _OffChainTx} = payload(#{channel_pubkey => ChannelPubKey}),
    {error, channel_does_not_exist} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee = 1,
                                            Bin,
                                            PoI,
                                            OnChainTrees,
                                            tx_env()),
    ok.

check_solo_not_enough_funds() ->
    Trees = aec_trees:new_without_backend(),
    PoI = aec_trees:new_poi(Trees),
    Channel = new_channel(),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    FromPubKey = <<42:32/unit:8>>,
    {Bin, _SignedTx, _OffChainTx} = payload(#{channel_pubkey => ChannelPubKey}),
    %% with non-empty payload
    {error, account_not_found} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee = 1,
                                            Bin,
                                            PoI,
                                            OnChainTrees,
                                            tx_env()),
    %% with empty payload
    {error, account_not_found} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee = 1,
                                            <<>>,
                                            PoI,
                                            OnChainTrees,
                                            tx_env()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    %% with non-empty payload with account present
    {error, insufficient_funds} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt + 1,
                                            Bin,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    %% with empty payload with account present
    {error, insufficient_funds} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt + 1,
                                            <<>>,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    ok.

check_solo_not_a_peer() ->
    Channel = new_channel(),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    FromPubKey = <<1:32/unit:8>>,
    %% From is not a participant
    Initiator = aesc_channels:initiator_pubkey(Channel),
    Responder = aesc_channels:responder_pubkey(Channel),
    true = Initiator =/= FromPubKey,
    true = Responder =/= FromPubKey,
    Trees = aec_trees:new_without_backend(),
    PoI = aec_trees:new_poi(Trees),
    {Bin, _SignedTx, _OffChainTx} = payload(#{channel_pubkey => ChannelPubKey}),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    %% with non-empty payload with account present
    {error, account_not_peer} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            Bin,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    %% with empty payload with account present
    {error, account_not_peer} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            <<>>,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    ok.

check_solo_already_closing() ->
    check_solo_already_closing(initiator_pubkey),
    check_solo_already_closing(responder_pubkey).

check_solo_already_closing(FromKey) ->
    FromPubKey = <<1:32/unit:8>>,
    Channel0 = new_channel(#{FromKey => FromPubKey}),
    Channel = aesc_channels:set_solo_closing(Channel0, 1234),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Trees = aec_trees:new_without_backend(),
    PoI = aec_trees:new_poi(Trees),
    {Bin, _SignedTx, _OffChainTx} = payload(#{channel_pubkey => ChannelPubKey}),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    %% with non-empty payload with account present
    {error, channel_not_active} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            Bin,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    %% with empty payload with account present
    {error, channel_not_active} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            <<>>,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    ok.

check_solo_close_empty_payload_hash_mismatch() ->
    check_solo_close_empty_payload_hash_mismatch(initiator_pubkey),
    check_solo_close_empty_payload_hash_mismatch(responder_pubkey).

check_solo_close_empty_payload_hash_mismatch(FromKey) ->
    FromPubKey = <<1:32/unit:8>>,
    Trees = set_account(aec_accounts:new(FromPubKey, 10),
                        aec_trees:new_without_backend()),
    PoI = aec_trees:new_poi(Trees),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel = new_channel(#{ FromKey => FromPubKey
                           , state_hash => PoIHash }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    {error, wrong_channel_peers} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            <<>>,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    ok.

check_solo_close_empty_payload_missing_accounts() ->
    check_solo_close_empty_payload_missing_accounts(initiator_pubkey),
    check_solo_close_empty_payload_missing_accounts(responder_pubkey).

check_solo_close_empty_payload_missing_accounts(FromKey) ->
    FromPubKey = <<1:32/unit:8>>,
    StateHash = <<12:32/unit:8>>,
    Channel = new_channel(#{ FromKey => FromPubKey
                                 , state_hash => StateHash}),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(FromPubKey, 10),
                        aec_trees:new_without_backend()),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI} = aec_trees:add_poi(accounts, FromPubKey, Trees, PoI0),
    PoIHash = aec_trees:poi_hash(PoI),
    true = PoIHash =/= StateHash, 
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    {error, invalid_poi_hash_in_channel} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            <<>>,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    ok.


check_solo_close_empty_payload_wrong_amounts() ->
    FromPubKey = <<1:32/unit:8>>,
    OtherPeer = <<2:32/unit:8>>,
    %% set a total balance of 11
    Trees0 = set_account(aec_accounts:new(FromPubKey, 10),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(OtherPeer, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, FromPubKey, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, OtherPeer, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel = new_channel(#{ initiator_pubkey => FromPubKey
                           , responder_pubkey => OtherPeer
                           %% make an onchain total balance of 10
                           , initiator_amount => 5
                           , responder_amount => 5
                           , state_hash => PoIHash }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    {error, poi_amounts_change_channel_funds} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            <<>>,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    ok.

check_solo_close_empty_payload_less_amounts() ->
    FromPubKey = <<1:32/unit:8>>,
    OtherPeer = <<2:32/unit:8>>,
    %% set a total balance of 8 
    Trees0 = set_account(aec_accounts:new(FromPubKey, 8),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(OtherPeer, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, FromPubKey, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, OtherPeer, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel = new_channel(#{ initiator_pubkey => FromPubKey
                           , responder_pubkey => OtherPeer
                           %% make an onchain total balance of 10
                           , initiator_amount => 5
                           , responder_amount => 5
                           , state_hash => PoIHash }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    ok =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            <<>>,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    ok.

check_solo_close_empty_payload_exact_amounts() ->
    FromPubKey = <<1:32/unit:8>>,
    OtherPeer = <<2:32/unit:8>>,
    TotalAmount = 10,
    Trees0 = set_account(aec_accounts:new(FromPubKey, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(OtherPeer, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, FromPubKey, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, OtherPeer, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel = new_channel(#{ initiator_pubkey => FromPubKey
                           , responder_pubkey => OtherPeer
                           %% make an onchain total balance of TotalAmount
                           , initiator_amount => TotalAmount - 2
                           , responder_amount => 2
                           , state_hash => PoIHash }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    ok =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            <<>>,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    ok.

check_solo_close_payload_wrong_channel_id() ->
    FromPubKey = <<1:32/unit:8>>,
    OtherPeer = <<2:32/unit:8>>,
    TotalAmount = 10,
    Trees0 = set_account(aec_accounts:new(FromPubKey, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(OtherPeer, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, FromPubKey, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, OtherPeer, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel = new_channel(#{ initiator_pubkey => FromPubKey
                           , responder_pubkey => OtherPeer
                           %% make an onchain total balance of TotalAmount
                           , initiator_amount => TotalAmount - 2
                           , responder_amount => 2
                           , state_hash => PoIHash }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    DifferentChannelPubkey = <<1:32/unit:8>>,
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    {Bin, _SignedTx, _OffChainTx} = payload(#{ channel_pubkey => DifferentChannelPubkey
                                             , state_hash => PoIHash }),
    {error, bad_state_channel_pubkey} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            Bin,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    ok.

check_solo_close_payload_wrong_round() ->
    FromPubKey = <<1:32/unit:8>>,
    OtherPeer = <<2:32/unit:8>>,
    TotalAmount = 10,
    Round = 33,
    Trees0 = set_account(aec_accounts:new(FromPubKey, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(OtherPeer, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, FromPubKey, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, OtherPeer, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel = new_channel(#{ initiator_pubkey => FromPubKey
                           , responder_pubkey => OtherPeer
                           %% make an onchain total balance of TotalAmount
                           , initiator_amount => TotalAmount - 2
                           , responder_amount => 2
                           , round => Round
                           , state_hash => PoIHash }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    %% round less than channel on-chain one
    {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round - 1
                            , state_hash => PoIHash }),
    {error, old_round} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            Bin1,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    %% round equal than channel on-chain one
    {Bin2, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round
                            , state_hash => PoIHash }),
    {error, same_round} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            Bin2,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    ok.

check_solo_close_payload_wrong_signatures() ->
    FromPubKey = <<1:32/unit:8>>,
    OtherPeer = <<2:32/unit:8>>,
    TotalAmount = 10,
    Round = 33,
    Trees0 = set_account(aec_accounts:new(FromPubKey, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(OtherPeer, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, FromPubKey, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, OtherPeer, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel = new_channel(#{ initiator_pubkey => FromPubKey
                           , responder_pubkey => OtherPeer
                           %% make an onchain total balance of TotalAmount
                           , initiator_amount => TotalAmount - 2
                           , responder_amount => 2
                           , round => Round
                           , state_hash => PoIHash }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round + 1
                            , state_hash => PoIHash }),
    {error, signature_check_failed} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            FromPubKey,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            Bin1,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    ok.

check_solo_close_payload_wrong_peers() ->
    {IPub, IPriv} = aecore_suite_utils:generate_key_pair(),
    {RPub, RPriv} = aecore_suite_utils:generate_key_pair(),
    TotalAmount = 10,
    Round = 33,
    Trees0 = set_account(aec_accounts:new(IPub, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(RPub, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    Test =
        fun(Pubkey) ->
            {ok, PoI} = aec_trees:add_poi(accounts, Pubkey, Trees, PoI0),
            PoIHash = aec_trees:poi_hash(PoI),
            Channel = new_channel(#{ initiator_pubkey => IPub
                                  , responder_pubkey => RPub
                                  %% make an onchain total balance of TotalAmount
                                  , initiator_amount => TotalAmount - 2
                                  , responder_amount => 2
                                  , round => Round
                                  , state_hash => PoIHash }),
            ChannelPubKey = aesc_channels:pubkey(Channel),
            OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
            Amt = 100,
            From = IPub,
            OnChainTrees1 = set_account(aec_accounts:new(IPub, Amt), OnChainTrees),
            {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                                    , round => Round + 1
                                    , state_hash => PoIHash
                                    , initiator_privkey => IPriv
                                    , responder_privkey => RPriv }),
            {error, wrong_channel_peers} =
                aesc_utils:check_solo_close_payload(ChannelPubKey,
                                                    From,
                                                    _Nonce = 1,
                                                    _Fee1 = Amt,
                                                    Bin1,
                                                    PoI,
                                                    OnChainTrees1,
                                                    tx_env())
        end,
    Test(IPub),
    Test(RPub),
    ok.

check_solo_close_payload_wrong_amounts() ->
    {IPub, IPriv} = aecore_suite_utils:generate_key_pair(),
    {RPub, RPriv} = aecore_suite_utils:generate_key_pair(),
    TotalAmount = 10,
    Round = 33,
    Trees0 = set_account(aec_accounts:new(IPub, TotalAmount),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(RPub, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, IPub, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, RPub, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel = new_channel(#{ initiator_pubkey => IPub
                          , responder_pubkey => RPub
                          %% make an onchain total balance of TotalAmount
                          , initiator_amount => TotalAmount - 2
                          , responder_amount => 2
                          , round => Round
                          , state_hash => PoIHash }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    From = IPub,
    OnChainTrees1 = set_account(aec_accounts:new(IPub, Amt), OnChainTrees),
    {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round + 1
                            , state_hash => PoIHash
                            , initiator_privkey => IPriv
                            , responder_privkey => RPriv }),
    {error, poi_amounts_change_channel_funds} =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            From,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            Bin1,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    ok.

check_solo_close_payload_correct() ->
    {IPub, IPriv} = aecore_suite_utils:generate_key_pair(),
    {RPub, RPriv} = aecore_suite_utils:generate_key_pair(),
    TotalAmount = 10,
    Round = 33,
    Trees0 = set_account(aec_accounts:new(IPub, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(RPub, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, IPub, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, RPub, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel = new_channel(#{ initiator_pubkey => IPub
                          , responder_pubkey => RPub
                          %% make an onchain total balance of TotalAmount
                          , initiator_amount => TotalAmount - 2
                          , responder_amount => 2
                          , round => Round
                          , state_hash => PoIHash }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    From = IPub,
    OnChainTrees1 = set_account(aec_accounts:new(IPub, Amt), OnChainTrees),
    {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round + 1
                            , state_hash => PoIHash
                            , initiator_privkey => IPriv
                            , responder_privkey => RPriv }),
    ok =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            From,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            Bin1,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    ok.

check_solo_close_payload_correct_bigger_poi() ->
    {IPub, IPriv} = aecore_suite_utils:generate_key_pair(),
    {RPub, RPriv} = aecore_suite_utils:generate_key_pair(),
    OtherAccount = <<1:32/unit:8>>,
    TotalAmount = 10,
    Round = 33,
    Trees0 = set_account(aec_accounts:new(IPub, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees1 = set_account(aec_accounts:new(RPub, 1), Trees0),
    %% note that this makes the total amount of the accounts bigger than the
    %% total amount of tokens dedicated to the channel. Since we ignore any
    %% account beyond the participants one, this is safe
    Trees = set_account(aec_accounts:new(OtherAccount, 1), Trees1),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, IPub, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, RPub, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel = new_channel(#{ initiator_pubkey => IPub
                          , responder_pubkey => RPub
                          %% make an onchain total balance of TotalAmount
                          , initiator_amount => TotalAmount - 2
                          , responder_amount => 2
                          , round => Round
                          , state_hash => PoIHash }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    From = IPub,
    OnChainTrees1 = set_account(aec_accounts:new(IPub, Amt), OnChainTrees),
    {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round + 1
                            , state_hash => PoIHash
                            , initiator_privkey => IPriv
                            , responder_privkey => RPriv }),
    ok =
        aesc_utils:check_solo_close_payload(ChannelPubKey,
                                            From,
                                            _Nonce = 1,
                                            _Fee1 = Amt,
                                            Bin1,
                                            PoI,
                                            OnChainTrees1,
                                            tx_env()),
    ok.

deserialize_broken_slash_payload() ->
    FromPubKey = <<42:32/unit:8>>,
    Trees = aec_trees:new_without_backend(),
    PoI = aec_trees:new_poi(Trees),
    Channel = new_channel(),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    {error, payload_deserialization_failed} =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       FromPubKey,
                                       _Nonce = 1,
                                       _Fee = 1,
                                       <<"a">>,
                                       PoI,
                                       OnChainTrees,
                                       tx_env()),
    SignedTx = spend_tx(),
    Bin = aetx_sign:serialize_to_binary(SignedTx),
    {error, bad_offchain_state_type} =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       FromPubKey,
                                       _Nonce = 1,
                                       _Fee = 1,
                                       Bin,
                                       PoI,
                                       OnChainTrees,
                                       tx_env()),
    ok.

check_slash_from_a_missing_channel() ->
    Trees = aec_trees:new_without_backend(),
    PoI = aec_trees:new_poi(Trees),
    Channel = new_channel(),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = aec_trees:new_without_backend(),
    FromPubKey = <<42:32/unit:8>>,
    {Bin, _SignedTx, _OffChainTx} = payload(#{channel_pubkey => ChannelPubKey}),
    {error, channel_does_not_exist} =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       FromPubKey,
                                       _Nonce = 1,
                                       _Fee = 1,
                                       Bin,
                                       PoI,
                                       OnChainTrees,
                                       tx_env()),
    ok.

check_slash_payload_last_onchain() ->
    Trees = aec_trees:new_without_backend(),
    PoI = aec_trees:new_poi(Trees),
    Channel = new_channel(),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    FromPubKey = <<42:32/unit:8>>,
    {error, slash_must_have_payload} =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       FromPubKey,
                                       _Nonce = 1,
                                       _Fee = 1,
                                       <<>>,
                                       PoI,
                                       OnChainTrees,
                                       tx_env()),
    ok.

check_slash_not_enough_funds() ->
    SoloRound = 10,
    Trees = aec_trees:new_without_backend(),
    PoI = aec_trees:new_poi(Trees),
    Channel0 = new_channel(),
    Channel = aesc_channels:set_solo_round(Channel0, SoloRound),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    FromPubKey = <<42:32/unit:8>>,
    {Bin, _SignedTx, _OffChainTx} = payload(#{channel_pubkey => ChannelPubKey}),
    {error, account_not_found} =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       FromPubKey,
                                       _Nonce = 1,
                                       _Fee = 1,
                                       Bin,
                                       PoI,
                                       OnChainTrees,
                                       tx_env()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    %% with account present
    {error, insufficient_funds} =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       FromPubKey,
                                       _Nonce = 1,
                                       _Fee1 = Amt + 1,
                                       Bin,
                                       PoI,
                                       OnChainTrees1,
                                       tx_env()),
    ok.

check_slash_payload_not_closing() ->
    FromPubKey = <<1:32/unit:8>>,
    OtherPeer = <<2:32/unit:8>>,
    TotalAmount = 10,
    Trees0 = set_account(aec_accounts:new(FromPubKey, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(OtherPeer, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, FromPubKey, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, OtherPeer, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel = new_channel(#{ initiator_pubkey => FromPubKey
                           , responder_pubkey => OtherPeer
                           %% make an onchain total balance of TotalAmount
                           , initiator_amount => TotalAmount - 2
                           , responder_amount => 2
                           , state_hash => PoIHash }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    DifferentChannelPubkey = <<1:32/unit:8>>,
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    {Bin, _SignedTx, _OffChainTx} = payload(#{ channel_pubkey => DifferentChannelPubkey
                                             , state_hash => PoIHash }),
    {error, channel_not_closing} =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       FromPubKey,
                                       _Nonce = 1,
                                       _Fee1 = Amt,
                                        Bin,
                                        PoI,
                                        OnChainTrees1,
                                        tx_env()),
    ok.


check_slash_not_a_peer() ->
    SoloRound = 20,
    ClosedHeight = 100,
    Channel0 = new_channel(),
    Channel1 = aesc_channels:set_solo_round(Channel0, SoloRound),
    Channel = aesc_channels:set_solo_closing(Channel1, ClosedHeight),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    FromPubKey = <<1:32/unit:8>>,
    %% From is not a participant
    Initiator = aesc_channels:initiator_pubkey(Channel),
    Responder = aesc_channels:responder_pubkey(Channel),
    true = Initiator =/= FromPubKey,
    true = Responder =/= FromPubKey,
    Trees = aec_trees:new_without_backend(),
    PoI = aec_trees:new_poi(Trees),
    {Bin, _SignedTx, _OffChainTx} = payload(#{channel_pubkey => ChannelPubKey}),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    %% with non-empty payload with account present
    {error, account_not_peer_or_delegate} =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       FromPubKey,
                                       _Nonce = 1,
                                       _Fee1 = Amt,
                                       Bin,
                                       PoI,
                                       OnChainTrees1,
                                       tx_env()),
    ok.

check_slash_payload_wrong_channel_id() ->
    SoloRound = 20,
    ClosedHeight = 100,
    FromPubKey = <<1:32/unit:8>>,
    OtherPeer = <<2:32/unit:8>>,
    TotalAmount = 10,
    Trees0 = set_account(aec_accounts:new(FromPubKey, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(OtherPeer, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, FromPubKey, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, OtherPeer, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel0 = new_channel(#{ initiator_pubkey => FromPubKey
                            , responder_pubkey => OtherPeer
                            %% make an onchain total balance of TotalAmount
                            , initiator_amount => TotalAmount - 2
                            , responder_amount => 2
                            , state_hash => PoIHash }),
    Channel1 = aesc_channels:set_solo_round(Channel0, SoloRound),
    Channel = aesc_channels:set_solo_closing(Channel1, ClosedHeight),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    DifferentChannelPubkey = <<1:32/unit:8>>,
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    {Bin, _SignedTx, _OffChainTx} = payload(#{ channel_pubkey => DifferentChannelPubkey
                                             , state_hash => PoIHash }),
    {error, bad_state_channel_pubkey} =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       FromPubKey,
                                       _Nonce = 1,
                                       _Fee1 = Amt,
                                        Bin,
                                        PoI,
                                        OnChainTrees1,
                                        tx_env()),
    ok.

check_slash_payload_wrong_round() ->
    FromPubKey = <<1:32/unit:8>>,
    OtherPeer = <<2:32/unit:8>>,
    TotalAmount = 10,
    Round = 33,
    ClosedHeight = 100,
    Trees0 = set_account(aec_accounts:new(FromPubKey, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(OtherPeer, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, FromPubKey, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, OtherPeer, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel0 = new_channel(#{ initiator_pubkey => FromPubKey
                            , responder_pubkey => OtherPeer
                            %% make an onchain total balance of TotalAmount
                            , initiator_amount => TotalAmount - 2
                            , responder_amount => 2
                            , round => Round
                            , state_hash => PoIHash }),
    Channel = aesc_channels:set_solo_closing(Channel0, ClosedHeight),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    %% round less than channel on-chain one
    {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round - 1
                            , state_hash => PoIHash }),
    {error, old_round} =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       FromPubKey,
                                       _Nonce = 1,
                                       _Fee1 = Amt,
                                       Bin1,
                                       PoI,
                                       OnChainTrees1,
                                       tx_env()),
    %% round equal than channel on-chain one
    {Bin2, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round
                            , state_hash => PoIHash }),
    {error, same_round} =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       FromPubKey,
                                       _Nonce = 1,
                                       _Fee1 = Amt,
                                       Bin2,
                                       PoI,
                                       OnChainTrees1,
                                       tx_env()),
    ok.

check_slash_payload_wrong_signatures() ->
    FromPubKey = <<1:32/unit:8>>,
    OtherPeer = <<2:32/unit:8>>,
    TotalAmount = 10,
    Round = 33,
    ClosedHeight = 100,
    Trees0 = set_account(aec_accounts:new(FromPubKey, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(OtherPeer, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, FromPubKey, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, OtherPeer, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel0 = new_channel(#{ initiator_pubkey => FromPubKey
                            , responder_pubkey => OtherPeer
                            %% make an onchain total balance of TotalAmount
                            , initiator_amount => TotalAmount - 2
                            , responder_amount => 2
                            , round => Round
                            , state_hash => PoIHash }),
    Channel = aesc_channels:set_solo_closing(Channel0, ClosedHeight),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round + 1
                            , state_hash => PoIHash }),
    {error, signature_check_failed} =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       FromPubKey,
                                       _Nonce = 1,
                                       _Fee1 = Amt,
                                        Bin1,
                                        PoI,
                                        OnChainTrees1,
                                        tx_env()),
    ok.

check_slash_payload_wrong_peers() ->
    {IPub, IPriv} = aecore_suite_utils:generate_key_pair(),
    {RPub, RPriv} = aecore_suite_utils:generate_key_pair(),
    TotalAmount = 10,
    Round = 33,
    ClosedHeight = 100,
    Trees0 = set_account(aec_accounts:new(IPub, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(RPub, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    Test =
        fun(Pubkey) ->
            {ok, PoI} = aec_trees:add_poi(accounts, Pubkey, Trees, PoI0),
            PoIHash = aec_trees:poi_hash(PoI),
            Channel0 = new_channel(#{ initiator_pubkey => IPub
                                   , responder_pubkey => RPub
                                   %% make an onchain total balance of TotalAmount
                                   , initiator_amount => TotalAmount - 2
                                   , responder_amount => 2
                                   , round => Round
                                   , state_hash => PoIHash }),
            Channel = aesc_channels:set_solo_closing(Channel0, ClosedHeight),
            ChannelPubKey = aesc_channels:pubkey(Channel),
            OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
            Amt = 100,
            From = IPub,
            OnChainTrees1 = set_account(aec_accounts:new(IPub, Amt), OnChainTrees),
            {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                                    , round => Round + 1
                                    , state_hash => PoIHash
                                    , initiator_privkey => IPriv
                                    , responder_privkey => RPriv }),
            {error, wrong_channel_peers} =
                aesc_utils:check_slash_payload(ChannelPubKey,
                                               From,
                                               _Nonce = 1,
                                               _Fee1 = Amt,
                                               Bin1,
                                               PoI,
                                               OnChainTrees1,
                                               tx_env())
        end,
    Test(IPub),
    Test(RPub),
    ok.

check_slash_payload_wrong_amounts() ->
    {IPub, IPriv} = aecore_suite_utils:generate_key_pair(),
    {RPub, RPriv} = aecore_suite_utils:generate_key_pair(),
    TotalAmount = 10,
    Round = 33,
    ClosedHeight = 100,
    Trees0 = set_account(aec_accounts:new(IPub, TotalAmount),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(RPub, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, IPub, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, RPub, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel0 = new_channel(#{ initiator_pubkey => IPub
                           , responder_pubkey => RPub
                           %% make an onchain total balance of TotalAmount
                           , initiator_amount => TotalAmount - 2
                           , responder_amount => 2
                           , round => Round
                           , state_hash => PoIHash }),
    Channel = aesc_channels:set_solo_closing(Channel0, ClosedHeight),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    From = IPub,
    OnChainTrees1 = set_account(aec_accounts:new(IPub, Amt), OnChainTrees),
    {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round + 1
                            , state_hash => PoIHash
                            , initiator_privkey => IPriv
                            , responder_privkey => RPriv }),
    {error, poi_amounts_change_channel_funds} =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       From,
                                       _Nonce = 1,
                                       _Fee1 = Amt,
                                       Bin1,
                                       PoI,
                                       OnChainTrees1,
                                       tx_env()),
    ok.

check_slash_payload_correct() ->
    {IPub, IPriv} = aecore_suite_utils:generate_key_pair(),
    {RPub, RPriv} = aecore_suite_utils:generate_key_pair(),
    TotalAmount = 10,
    Round = 33,
    ClosedHeight = 100,
    Trees0 = set_account(aec_accounts:new(IPub, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(RPub, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, IPub, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, RPub, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel0 = new_channel(#{ initiator_pubkey => IPub
                            , responder_pubkey => RPub
                            %% make an onchain total balance of TotalAmount
                            , initiator_amount => TotalAmount - 2
                            , responder_amount => 2
                            , round => Round
                            , state_hash => PoIHash }),
    Channel = aesc_channels:set_solo_closing(Channel0, ClosedHeight),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    From = IPub,
    OnChainTrees1 = set_account(aec_accounts:new(IPub, Amt), OnChainTrees),
    {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round + 1
                            , state_hash => PoIHash
                            , initiator_privkey => IPriv
                            , responder_privkey => RPriv }),
    ok =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       From,
                                       _Nonce = 1,
                                       _Fee1 = Amt,
                                       Bin1,
                                       PoI,
                                       OnChainTrees1,
                                       tx_env()),
    ok.

check_slash_payload_correct_delegate() ->
    {IPub, IPriv} = aecore_suite_utils:generate_key_pair(),
    {RPub, RPriv} = aecore_suite_utils:generate_key_pair(),
    {DelegatePub, _DPriv} = aecore_suite_utils:generate_key_pair(),
    TotalAmount = 10,
    Round = 33,
    ClosedHeight = 100,
    Trees0 = set_account(aec_accounts:new(IPub, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(RPub, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, IPub, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, RPub, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    
    Test =
        fun(Delegates) ->
            Channel0 = new_channel(#{ initiator_pubkey => IPub
                                    , responder_pubkey => RPub
                                    %% make an onchain total balance of TotalAmount
                                    , initiator_amount => TotalAmount - 2
                                    , responder_amount => 2
                                    , round => Round
                                    , state_hash => PoIHash
                                    , delegate_pubkeys => Delegates }),
            Channel = aesc_channels:set_solo_closing(Channel0, ClosedHeight),
            ChannelPubKey = aesc_channels:pubkey(Channel),
            OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
            Amt = 100,
            From = DelegatePub,
            OnChainTrees1 = set_account(aec_accounts:new(From, Amt), OnChainTrees),
            {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                                    , round => Round + 1
                                    , state_hash => PoIHash
                                    , initiator_privkey => IPriv
                                    , responder_privkey => RPriv }),
            ok =
                aesc_utils:check_slash_payload(ChannelPubKey,
                                              From,
                                              _Nonce = 1,
                                              _Fee1 = Amt,
                                              Bin1,
                                              PoI,
                                              OnChainTrees1,
                                              tx_env())
        end,
    Test(delegates([DelegatePub], [])), %% test with initiator's delegate
    Test(delegates([], [DelegatePub])), %% test with responder's delegate
    ok.


check_slash_payload_correct_after_wrong_fp() ->
    {IPub, IPriv} = aecore_suite_utils:generate_key_pair(),
    {RPub, RPriv} = aecore_suite_utils:generate_key_pair(),
    TotalAmount = 10,
    Round = 33,
    ForcedProgressChainLength = 3,
    SoloRound = Round - ForcedProgressChainLength, % the first FP
    ClosedHeight = 100,
    Trees0 = set_account(aec_accounts:new(IPub, TotalAmount - 1),
                         aec_trees:new_without_backend()),
    Trees = set_account(aec_accounts:new(RPub, 1), Trees0),
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, IPub, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, RPub, Trees, PoI1),
    PoIHash = aec_trees:poi_hash(PoI),
    Channel0 = new_channel(#{ initiator_pubkey => IPub
                           , responder_pubkey => RPub
                           %% make an onchain total balance of TotalAmount
                           , initiator_amount => TotalAmount - 2
                           , responder_amount => 2
                           , round => Round
                           , state_hash => PoIHash }),
    Channel1 = aesc_channels:set_solo_round(Channel0, SoloRound),
    Channel = aesc_channels:set_solo_closing(Channel1, ClosedHeight),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    From = IPub,
    OnChainTrees1 = set_account(aec_accounts:new(IPub, Amt), OnChainTrees),
    {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            %% we can replace as old off-chain state, as the
                            %% first forced progress
                            , round => Round - ForcedProgressChainLength
                            , state_hash => PoIHash
                            , initiator_privkey => IPriv
                            , responder_privkey => RPriv }),
    ok =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       From,
                                       _Nonce = 1,
                                       _Fee1 = Amt,
                                       Bin1,
                                       PoI,
                                       OnChainTrees1,
                                       tx_env()),
    {Bin2, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            %% we can replace older off-chain states
                            , round => Round - ForcedProgressChainLength - 1
                            , state_hash => PoIHash
                            , initiator_privkey => IPriv
                            , responder_privkey => RPriv }),
    {error, same_round} =
        aesc_utils:check_slash_payload(ChannelPubKey,
                                       From,
                                       _Nonce = 1,
                                       _Fee1 = Amt,
                                       Bin2,
                                       PoI,
                                       OnChainTrees1,
                                       tx_env()),
    ok.

deserialize_broken_solo_snapshot_payload() ->
    FromPubKey = <<42:32/unit:8>>,
    Channel = new_channel(),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    {error, payload_deserialization_failed} =
        aesc_utils:check_solo_snapshot_payload(ChannelPubKey,
                                               FromPubKey,
                                               _Nonce = 1,
                                               _Fee = 1,
                                               <<"a">>,
                                               OnChainTrees,
                                               tx_env()),
    SignedTx = spend_tx(),
    Bin = aetx_sign:serialize_to_binary(SignedTx),
    {error, bad_offchain_state_type} =
        aesc_utils:check_solo_snapshot_payload(ChannelPubKey,
                                               FromPubKey,
                                               _Nonce = 1,
                                               _Fee = 1,
                                               Bin,
                                               OnChainTrees,
                                               tx_env()),
    ok.

check_solo_snapshot_from_a_missing_channel() ->
    Channel = new_channel(),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = aec_trees:new_without_backend(),
    FromPubKey = <<42:32/unit:8>>,
    {Bin, _SignedTx, _OffChainTx} = payload(#{channel_pubkey => ChannelPubKey}),
    {error, channel_does_not_exist} =
        aesc_utils:check_solo_snapshot_payload(ChannelPubKey,
                                               FromPubKey,
                                               _Nonce = 1,
                                               _Fee = 1,
                                               Bin,
                                               OnChainTrees,
                                               tx_env()),
    ok.

check_solo_snapshot_payload_last_onchain() ->
    Channel = new_channel(),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    FromPubKey = <<42:32/unit:8>>,
    {error, snapshot_must_have_payload} =
        aesc_utils:check_solo_snapshot_payload(ChannelPubKey,
                                               FromPubKey,
                                               _Nonce = 1,
                                               _Fee = 1,
                                               <<>>,
                                               OnChainTrees,
                                               tx_env()),
    ok.

check_solo_snapshot_not_enough_funds() ->
    SoloRound = 10,
    Channel0 = new_channel(),
    Channel = aesc_channels:set_solo_round(Channel0, SoloRound),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    FromPubKey = <<42:32/unit:8>>,
    {Bin, _SignedTx, _OffChainTx} = payload(#{channel_pubkey => ChannelPubKey}),
    {error, account_not_found} =
        aesc_utils:check_solo_snapshot_payload(ChannelPubKey,
                                               FromPubKey,
                                               _Nonce = 1,
                                               _Fee = 1,
                                               Bin,
                                               OnChainTrees,
                                               tx_env()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    %% with account present
    {error, insufficient_funds} =
        aesc_utils:check_solo_snapshot_payload(ChannelPubKey,
                                               FromPubKey,
                                               _Nonce = 1,
                                               _Fee1 = Amt + 1,
                                               Bin,
                                               OnChainTrees1,
                                               tx_env()),
    ok.

check_solo_snapshot_not_a_peer() ->
    Channel = new_channel(),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    FromPubKey = <<1:32/unit:8>>,
    %% From is not a participant
    Initiator = aesc_channels:initiator_pubkey(Channel),
    Responder = aesc_channels:responder_pubkey(Channel),
    true = Initiator =/= FromPubKey,
    true = Responder =/= FromPubKey,
    {Bin, _SignedTx, _OffChainTx} = payload(#{channel_pubkey => ChannelPubKey}),
    Amt = 100,
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    %% with non-empty payload with account present
    {error, Error} =
        aesc_utils:check_solo_snapshot_payload(ChannelPubKey,
                                               FromPubKey,
                                               _Nonce = 1,
                                               _Fee1 = Amt,
                                               Bin,
                                               OnChainTrees1,
                                               tx_env()),
    case active_protocol() < ?IRIS_PROTOCOL_VSN of
        true -> {Error, Error} = {Error, account_not_peer};
        false -> {Error, Error} = {Error, account_not_peer_or_delegate}
    end,
    ok.

check_solo_snapshot_already_closing() ->
    check_solo_snapshot_already_closing(initiator_pubkey),
    check_solo_snapshot_already_closing(responder_pubkey).

check_solo_snapshot_already_closing(FromKey) ->
    FromPubKey = <<1:32/unit:8>>,
    Channel0 = new_channel(#{FromKey => FromPubKey}),
    Channel = aesc_channels:set_solo_closing(Channel0, 1234),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    {Bin, _SignedTx, _OffChainTx} = payload(#{channel_pubkey => ChannelPubKey}),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    %% with non-empty payload with account present
    {error, channel_not_active} =
        aesc_utils:check_solo_snapshot_payload(ChannelPubKey,
                                               FromPubKey,
                                               _Nonce = 1,
                                               _Fee1 = Amt,
                                               Bin,
                                               OnChainTrees1,
                                               tx_env()),
    ok.

check_solo_snapshot_payload_wrong_channel_id() ->
    FromPubKey = <<1:32/unit:8>>,
    OtherPeer = <<2:32/unit:8>>,
    TotalAmount = 10,
    Channel = new_channel(#{ initiator_pubkey => FromPubKey
                           , responder_pubkey => OtherPeer
                           %% make an onchain total balance of TotalAmount
                           , initiator_amount => TotalAmount - 2
                           , responder_amount => 2 }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    DifferentChannelPubkey = <<1:32/unit:8>>,
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    {Bin, _SignedTx, _OffChainTx} = payload(#{ channel_pubkey => DifferentChannelPubkey }),
    {error, bad_state_channel_pubkey} =
        aesc_utils:check_solo_snapshot_payload(ChannelPubKey,
                                               FromPubKey,
                                               _Nonce = 1,
                                               _Fee1 = Amt,
                                               Bin,
                                               OnChainTrees1,
                                               tx_env()),
    ok.

check_solo_snapshot_payload_wrong_round() ->
    FromPubKey = <<1:32/unit:8>>,
    OtherPeer = <<2:32/unit:8>>,
    Round = 33,
    Channel = new_channel(#{ initiator_pubkey => FromPubKey
                           , responder_pubkey => OtherPeer
                           %% make an onchain total balance of TotalAmount
                           , round => Round }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    %% round less than channel on-chain one
    {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round - 1 }),
    {error, old_round} =
        aesc_utils:check_solo_snapshot_payload(ChannelPubKey,
                                               FromPubKey,
                                               _Nonce = 1,
                                               _Fee1 = Amt,
                                               Bin1,
                                               OnChainTrees1,
                                               tx_env()),
    %% round equal than channel on-chain one
    {Bin2, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round }),
    {error, same_round} =
        aesc_utils:check_solo_snapshot_payload(ChannelPubKey,
                                               FromPubKey,
                                               _Nonce = 1,
                                               _Fee1 = Amt,
                                               Bin2,
                                               OnChainTrees1,
                                               tx_env()),
    ok.

check_solo_snapshot_payload_wrong_signatures() ->
    FromPubKey = <<1:32/unit:8>>,
    OtherPeer = <<2:32/unit:8>>,
    Round = 33,
    Channel = new_channel(#{ initiator_pubkey => FromPubKey
                           , responder_pubkey => OtherPeer
                           %% make an onchain total balance of TotalAmount
                           , round => Round }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    OnChainTrees1 = set_account(aec_accounts:new(FromPubKey, Amt), OnChainTrees),
    {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round + 1 }),
    {error, signature_check_failed} =
        aesc_utils:check_solo_snapshot_payload(ChannelPubKey,
                                               FromPubKey,
                                               _Nonce = 1,
                                               _Fee1 = Amt,
                                               Bin1,
                                               OnChainTrees1,
                                               tx_env()),
    ok.

check_solo_snapshot_payload_correct() ->
    {IPub, IPriv} = aecore_suite_utils:generate_key_pair(),
    {RPub, RPriv} = aecore_suite_utils:generate_key_pair(),
    Round = 33,
    Channel = new_channel(#{ initiator_pubkey => IPub
                          , responder_pubkey => RPub
                          %% make an onchain total balance of TotalAmount
                          , round => Round }),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    Amt = 100,
    From = IPub,
    OnChainTrees1 = set_account(aec_accounts:new(IPub, Amt), OnChainTrees),
    {Bin1, _, _} = payload(#{ channel_pubkey => ChannelPubKey
                            , round => Round + 1
                            , initiator_privkey => IPriv
                            , responder_privkey => RPriv }),
    ok =
        aesc_utils:check_solo_snapshot_payload(ChannelPubKey,
                                               From,
                                               _Nonce = 1,
                                               _Fee1 = Amt,
                                               Bin1,
                                               OnChainTrees1,
                                               tx_env()),
    ok.

deserialize_broken_fp_payload() ->
    Channel = new_channel(),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = set_channel(Channel, aec_trees:new_without_backend()),
    %% some update that is serializable
    Update =
        aesc_offchain_update:op_transfer(aeser_id:create(account, <<123:32/unit:8>>),
                                         aeser_id:create(account, <<321:32/unit:8>>),
                                         10),
    OffChainTrees = aec_trees:new_without_backend(),
    AetxFPTx1 =
        create_force_progess_tx(#{ channel_pubkey => ChannelPubKey
                                 , payload => <<"a">>
                                 , update => Update
                                 , off_chain_trees => OffChainTrees }),
    {error, payload_deserialization_failed} =
        aesc_utils:check_force_progress(strip_aetx(AetxFPTx1),
                                        OnChainTrees,
                                        tx_env()),
    SignedTx = spend_tx(),
    Bin = aetx_sign:serialize_to_binary(SignedTx),
    AetxFPTx2=
        create_force_progess_tx(#{ channel_pubkey => ChannelPubKey
                                 , payload => Bin 
                                 , update => Update
                                 , off_chain_trees => OffChainTrees }),
    {error, bad_offchain_state_type} =
        aesc_utils:check_force_progress(strip_aetx(AetxFPTx2),
                                        OnChainTrees,
                                        tx_env()),
    ok.

check_fp_from_a_missing_channel() ->
    Channel = new_channel(),
    ChannelPubKey = aesc_channels:pubkey(Channel),
    OnChainTrees = aec_trees:new_without_backend(),
    %% some update that is serializable
    Update =
        aesc_offchain_update:op_transfer(aeser_id:create(account, <<123:32/unit:8>>),
                                         aeser_id:create(account, <<321:32/unit:8>>),
                                         10),
    OffChainTrees = aec_trees:new_without_backend(),
    {Bin, _SignedTx, _OffChainTx} = payload(#{channel_pubkey => ChannelPubKey}),
    AetxFPTx1 =
        create_force_progess_tx(#{ channel_pubkey => ChannelPubKey
                                 , payload => Bin 
                                 , update => Update
                                 , off_chain_trees => OffChainTrees }),
    {error, channel_does_not_exist} =
        aesc_utils:check_force_progress(strip_aetx(AetxFPTx1),
                                        OnChainTrees,
                                        tx_env()),
    ok.


%% helpers
new_channel() ->
    new_channel(#{}).

new_channel(Opts) ->
    InitiatorAcc = aec_accounts:new(maps:get(initiator_pubkey, Opts,
                                          <<42:32/unit:8>>),
                                 1000),
    ResponderAcc = aec_accounts:new(maps:get(responder_pubkey, Opts,
                                          <<43:32/unit:8>>),
                                 1000),
    aesc_channels:new( InitiatorAcc
                     , maps:get(initiator_amount, Opts, 43)
                     , ResponderAcc
                     , maps:get(responder_amount, Opts, 43)
                     , maps:get(reserve_amount, Opts, 20)
                     , maps:get(delegate_pubkeys, Opts, no_delegates())
                     , maps:get(state_hash, Opts, <<123:32/unit:8>>)
                     , maps:get(lock_period, Opts, 3)
                     , maps:get(nonce, Opts, 1)
                     , active_protocol()
                     , maps:get(round, Opts, 1)).

set_channel(Channel, Trees) ->
    SCTrees0 = aec_trees:channels(Trees),
    SCTrees = aesc_state_tree:enter(Channel, SCTrees0),
    aec_trees:set_channels(Trees, SCTrees).

set_account(Account, Trees) ->
    AccsTrees0 = aec_trees:accounts(Trees),
    AccsTrees = aec_accounts_trees:enter(Account, AccsTrees0),
    aec_trees:set_accounts(Trees, AccsTrees).

active_protocol() ->
    aec_hard_forks:protocol_effective_at_height(1).

closing_channel() ->
    closing_channel(#{}).

closing_channel(Opts) ->
    Initiator = maps:get(initiator_pubkey, Opts, <<42:32/unit:8>>),
    IAmt = 10,
    Responder = maps:get(reponder_pubkey, Opts, <<43:32/unit:8>>),
    RAmt = 10,
    %% construct the off-chain state trees
    Accs =
        lists:foldl(
            fun({Pubkey, Amt}, AccumTrees) ->
                Acc = aec_accounts:new(Pubkey, Amt),
                aec_accounts_trees:enter(Acc, AccumTrees)
            end,
            aec_accounts_trees:empty(),
            [{Initiator, IAmt}, {Responder, RAmt}]),
    Trees0 = aec_trees:new_without_backend(),
    Trees = aec_trees:set_accounts(Trees0, Accs),
    StateHash = aec_trees:hash(Trees),
    Channel0 = new_channel(Opts#{ initiator_pubkey => Initiator
                                , initiator_amount => IAmt
                                , responder_pubkey => Responder
                                , responder_amount => RAmt
                                , state_hash       => StateHash}),
    %% produce a proof of inclusion with the same hash
    PoI0 = aec_trees:new_poi(Trees),
    {ok, PoI1} = aec_trees:add_poi(accounts, Initiator, Trees, PoI0),
    {ok, PoI} = aec_trees:add_poi(accounts, Responder, Trees, PoI1),
    %% close the channel with the correct poi
    _Channel = aesc_channels:close_solo_last_onchain(Channel0, PoI,
                                                     _Height = 10).

spend_tx() ->
    {ok, SpendTx} =
        aec_spend_tx:new(#{ sender_id     => aeser_id:create(account, <<1:32/unit:8>>)
                          , recipient_id => aeser_id:create(account, <<2:32/unit:8>>)
                          , amount       => 1
                          , fee          => 1
                          , nonce        => 1
                          , payload      => <<>>}),
    _SignedTx = aetx_sign:new(SpendTx, []).

tx_env() ->
    tx_env(#{}).

tx_env(Opts) ->
    Height = maps:get(height, Opts, 123),
    Time = maps:get(time, Opts, 123),
    Nonce = maps:get(nonce, Opts, 123),
    PrevHash = maps:get(prev_hash, Opts, <<1:32/unit:8>>),
    PrevKeyHash = maps:get(prev_key_hash, Opts, PrevHash),
    RootHash = maps:get(root_hash, Opts, <<1:32/unit:8>>),
    Miner  = maps:get(miner, Opts, <<1:32/unit:8>>),
    Beneficiary = maps:get(beneficiary, Opts, Miner),
    Target = maps:get(target, Opts, 507057548),
    Evd = lists:duplicate(32, 1), 
    KeyHeader =
        aec_headers:new_key_header(Height, PrevHash, PrevKeyHash, RootHash, Miner, Beneficiary,
               Target, Evd, Nonce, Time, _Info = default, _Version = 1),
    aetx_env:tx_env_from_key_header(KeyHeader, _KeyHash = <<1:32/unit:8>>, Time, <<1:32/unit:8>>).

payload() ->
    payload(#{}).

payload(Opts) ->
    GetOrCreatePrivkey =
        fun(Key) ->
            case maps:find(Key, Opts) of
                {ok, Privkey} -> Privkey;
                error ->
                    {_pubkey, Privkey} = aecore_suite_utils:generate_key_pair(),
                    Privkey
            end
        end,
    IPriv = GetOrCreatePrivkey(initiator_privkey),
    RPriv = GetOrCreatePrivkey(responder_privkey),
    {ok, AetxOffChainTx} =
        aesc_offchain_tx:new(
            #{ channel_id => aeser_id:create(channel, maps:get(channel_pubkey,
                                                               Opts,
                                                               <<1:32/unit:8>>))
             , state_hash => maps:get(state_hash, Opts, <<2:32/unit:8>>)
             , round      => maps:get(round, Opts, 42)}),
    {aesc_offchain_tx, OffChainTx} = aetx:specialize_callback(AetxOffChainTx),
    SignedTx0 = aetx_sign:new(AetxOffChainTx, []),
    SignedTx = lists:foldl(
                  fun(PK, STx1) ->
                          aec_test_utils:co_sign_tx(
                            STx1, PK)
                  end,
                  SignedTx0,
                  [IPriv, RPriv]),
    PayloadBin = aetx_sign:serialize_to_binary(SignedTx),
    {PayloadBin, SignedTx, OffChainTx}.

create_force_progess_tx(#{ payload := Payload
                         , update := Update
                         , off_chain_trees := OffChainTrees } = Opts) ->
    {ok, Aetx} =
        aesc_force_progress_tx:new(
            #{ channel_id => aeser_id:create(channel, maps:get(channel_pubkey,
                                                               Opts,
                                                               <<1:32/unit:8>>))
            , from_id => aeser_id:create(account, maps:get(from_pubkey,
                                                           Opts,
                                                           <<2:32/unit:8>>))
            , payload => Payload
            , update  => Update
            , state_hash => maps:get(state_hash, Opts,
                                     aec_trees:hash(OffChainTrees))
            , round      => maps:get(round, Opts, 42)
            , offchain_trees =>  OffChainTrees
            , fee   => maps:get(fee, Opts, 1)
            , nonce => maps:get(nonce, Opts, 1)}),
    Aetx.

strip_aetx(Aetx) ->
    {_, InnerTx} = aetx:specialize_callback(Aetx),
    InnerTx.

no_delegates() ->
    delegates([], []).

delegates(IIds, RIds) ->
   case active_protocol() < ?IRIS_PROTOCOL_VSN of
      true -> IIds ++ RIds;
      false -> {IIds, RIds}
    end.
