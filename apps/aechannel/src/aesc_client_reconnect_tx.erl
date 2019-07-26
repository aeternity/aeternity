-module(aesc_client_reconnect_tx).

-behavior(aetx).
-behavior(aesc_signable_transaction).

%% Behavior API
-export([
          new/1
        , type/0
        , fee/1
        , gas/1
        , ttl/1
        , nonce/1
        , origin/1
        , check/3
        , process/3
        , signers/2
        , version/1
        , serialization_template/1
        , serialize/1
        , deserialize/2
        , for_client/1
        , valid_at_protocol/2
        ]).

-export([
          channel_id/1
        , channel_pubkey/1
        , role/1
        , round/1
        , state_hash/1
        ]).

-define(CHANNEL_CLIENT_RECONNECT_TX_TYPE, channel_client_reconnect_tx).
-define(CHANNEL_CLIENT_RECONNECT_TX_FEE, 0).  % off-chain

-define(INITIAL_VSN, 1).

-type vsn() :: non_neg_integer().

-record(channel_client_reconnect_tx,
        {
          channel_id   :: aeser_id:id()
        , role         :: initiator | responder
        , pub_key      :: aeser_id:id()
        }).

-opaque tx() :: #channel_client_reconnect_tx{}.

-export_type([tx/0]).


-spec new(map()) -> {ok, aetx:tx()}.
new(#{ channel_id := ChannelId
     , role       := Role
     , pub_key    := Pubkey }) ->
    channel = aeser_id:specialize_type(ChannelId),
    account = aeser_id:specialize_type(Pubkey),
    true = (Role == initiator) orelse (Role == responder),
    Tx = #channel_client_reconnect_tx{
            channel_id = ChannelId
          , role       = Role
          , pub_key    = Pubkey },
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_CLIENT_RECONNECT_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_client_reconnect_tx{}) ->
    %% This tx should never hit the mempool or any block
    ?CHANNEL_CLIENT_RECONNECT_TX_FEE.

-spec gas(tx()) -> non_neg_integer().
gas(#channel_client_reconnect_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_client_reconnect_tx{}) ->
    0.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_client_reconnect_tx{}) ->
    0.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_client_reconnect_tx{pub_key = Pubkey}) ->
    aeser_id:specialize(Pubkey, account).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_client_reconnect_tx{
        channel_id = _ChannelId
      , role       = _Role
      , pub_key    = _Pubkey }, Trees, _Env) ->
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#channel_client_reconnect_tx{}, _Trees, #{}) ->
    error(off_chain_tx).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}.
signers(#channel_client_reconnect_tx{role = Role} = Tx, Trees) ->
    case aec_chain:get_channel(channel_pubkey(Tx), Trees) of
        {ok, Channel} ->
            case Role of
                initiator ->
                    {ok, [aesc_channels:initiator_pubkey(Channel)]};
                responder ->
                    {ok, [aesc_channels:responder_pubkey(Channel)]}
            end;
        {error, not_found} -> {error, channel_does_not_exist}
    end.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_client_reconnect_tx{
             channel_id = ChannelId
           , role       = Role
           , pub_key    = Pubkey }) ->
    {?INITIAL_VSN,
     [ {channel_id, ChannelId}
     , {role      , atom_to_binary(Role, utf8)}
     , {pub_key   , Pubkey}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?INITIAL_VSN, [ {channel_id, ChannelId}
                          , {role      , Role0}
                          , {pub_key   , Pubkey} ]) ->
    channel = aeser_id:specialize_type(ChannelId),
    account = aeser_id:specialize_type(Pubkey),
    Role = case binary_to_existing_atom(Role0, utf8) of
               initiator = R -> R;
               responder = R -> R
           end,
    #channel_client_reconnect_tx{
       channel_id = ChannelId
     , role       = Role
     , pub_key    = Pubkey }.

-spec for_client(tx()) -> map().
for_client(#channel_client_reconnect_tx{
              channel_id = ChannelId
            , role       = Role
            , pub_key    = Pubkey }) ->
    #{ <<"channel_id">> => aeser_api_encoder:encode(id_hash, ChannelId)
     , <<"role">>       => atom_to_binary(Role, utf8)
     , <<"pub_key">>    => aeser_api_encoder:encode(account, Pubkey) }.

serialization_template(?INITIAL_VSN) ->
    [ {channel_id, id}
    , {role      , binary}
    , {pub_key   , id} ].

%%%===================================================================
%%% Getters
%%%===================================================================

-spec channel_pubkey(tx()) -> aesc_channels:pubkey().
channel_pubkey(#channel_client_reconnect_tx{channel_id = ChannelId}) ->
    aeser_id:specialize(ChannelId, channel).

-spec channel_id(tx()) -> aesc_channels:id().
channel_id(#channel_client_reconnect_tx{channel_id = ChannelId}) ->
    ChannelId.

-spec role(tx()) -> initiator | responder.
role(#channel_client_reconnect_tx{ role = Role }) ->
    Role.

-spec round(tx()) -> aesc_channels:round().
round(#channel_client_reconnect_tx{}) ->
    error(invalid).

-spec state_hash(tx()) -> binary().
state_hash(#channel_client_reconnect_tx{}) ->
    error(invalid).

-spec version(tx()) -> non_neg_integer().
version(_) ->
    ?INITIAL_VSN.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(_Protocol, _Tx) ->
    true.

