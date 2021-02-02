-module(aesc_channels_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(ALICE, <<987:32/unit:8>>).
-define(BOB,   <<986:32/unit:8>>).
-define(CAROL, <<985:32/unit:8>>).
-define(DAVE,  <<984:32/unit:8>>).
-define(EMMA,  <<983:32/unit:8>>).
-define(FREYA, <<982:32/unit:8>>).
-define(GEORGE,<<981:32/unit:8>>).

get_channel_test_() ->
    {foreach,
     fun() ->
         ok
     end,
     [ {"New channel", fun new_channel/0}
     , {"Test different initiators", fun different_initiators/0}
     , {"Test different responders", fun different_responders/0}
     , {"Delegate ids", fun different_delegates/0}
     ]
    }.

new_channel() ->
    InitiatorPubkey = <<42:32/unit:8>>,
    IAmt = 420,
    ResponderPubkey = <<43:32/unit:8>>,
    RAmt = 430,
    Reserve = 12,
    StateHash = <<123:32/unit:8>>,
    LockPeriod = 4,
    Nonce = 12,
    Round = 1,
    InitiatorAcc = aec_accounts:new(InitiatorPubkey, 1000),
    ResponderAcc = aec_accounts:new(ResponderPubkey, 1000),
    Channel =
        aesc_channels:new( InitiatorAcc
                        , IAmt 
                        , ResponderAcc
                        , RAmt 
                        , Reserve 
                        , no_delegates() 
                        , StateHash 
                        , LockPeriod 
                        , Nonce
                        , active_protocol()
                        , Round),
    _ = aesc_channels:pubkey(Channel),
    ok.

different_initiators() ->
    Test =
        fun(InitiatorPubkey) ->
            Channel = new_channel_(#{initiator_pubkey => InitiatorPubkey}),
            %% same pubkey
            InitiatorPubkey = aesc_channels:initiator_pubkey(Channel),
            InitiatorId = aeser_id:create(account, InitiatorPubkey),
            %% same id
            InitiatorId = aesc_channels:initiator_id(Channel),
            %% expected role
            initiator = aesc_channels:role_by_pubkey(Channel, InitiatorPubkey),
            initiator = aesc_channels:role_by_id(Channel, InitiatorId),
            ok
        end,
    Test(?ALICE),
    Test(?BOB),
    ok.

different_responders() ->
    Test =
        fun(ResponderPubkey) ->
            Channel = new_channel_(#{responder_pubkey => ResponderPubkey}),
            %% same pubkey
            ResponderPubkey = aesc_channels:responder_pubkey(Channel),
            ResponderId = aeser_id:create(account, ResponderPubkey),
            %% same id
            ResponderId = aesc_channels:responder_id(Channel),
            %% expected role
            responder = aesc_channels:role_by_pubkey(Channel, ResponderPubkey),
            responder = aesc_channels:role_by_id(Channel, ResponderId),
            ok
        end,
    Test(?ALICE),
    Test(?BOB),
    ok.

different_delegates() ->
    Test =
        fun(IDelegates, RDelegates) ->
            Delegates = delegates(IDelegates, RDelegates),
            Channel = new_channel_(#{delegate_pubkeys => Delegates}),
            EncL = fun(L) -> [aeser_id:create(account, Pubkey) || Pubkey <- L]end,
            case aesc_channels:version(Channel) < 3 of
                true ->
                    Delegates = aesc_channels:delegate_pubkeys(Channel, initiator),
                    Delegates = aesc_channels:delegate_pubkeys(Channel, responder),
                    Delegates = aesc_channels:delegate_pubkeys(Channel, any),
                    EncDelegates = EncL(Delegates),
                    EncDelegates = aesc_channels:delegate_ids(Channel, initiator),
                    EncDelegates = aesc_channels:delegate_ids(Channel, responder),
                    EncDelegates = aesc_channels:delegate_ids(Channel, any);
                false ->
                    IDelegates = aesc_channels:delegate_pubkeys(Channel, initiator),
                    RDelegates = aesc_channels:delegate_pubkeys(Channel, responder),
                    AllDelegates = IDelegates ++ RDelegates,
                    AllDelegates = aesc_channels:delegate_pubkeys(Channel, any),
                    EncIDelegates = EncL(IDelegates),
                    EncIDelegates = aesc_channels:delegate_ids(Channel, initiator),
                    EncRDelegates = EncL(RDelegates),
                    EncRDelegates = aesc_channels:delegate_ids(Channel, responder),
                    EncDelegates = EncL(AllDelegates),
                    EncDelegates = aesc_channels:delegate_ids(Channel, any)
            end,
            ok
        end,
    Test([], []),
    Test([?ALICE], []),
    Test([], [?ALICE]),
    Test([?ALICE], [?ALICE]),
    Test([?ALICE], [?BOB]),
    Test([?ALICE, ?BOB, ?CAROL], []),
    Test([], [?ALICE, ?BOB, ?CAROL]),
    Test([?ALICE, ?BOB, ?CAROL], [?DAVE, ?EMMA, ?FREYA]),
    ok.

%% Private functions
new_channel_(Opts) ->
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

active_protocol() ->
    aec_hard_forks:protocol_effective_at_height(1).

no_delegates() ->
    delegates([], []).

delegates(IIds, RIds) ->
   case active_protocol() < ?IRIS_PROTOCOL_VSN of
      true -> IIds ++ RIds;
      false -> {IIds, RIds}
    end.
