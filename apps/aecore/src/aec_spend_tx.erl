%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_spend_tx).

%% API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         ttl/1,
         nonce/1,
         origin/1,
         sender_id/1,
         sender_pubkey/1,
         recipient_id/1,
         amount/1,
         check/3,
         process/3,
         signers/2,
         version/0,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

-export([payload/1]).

-behavior(aetx).

-include("blocks.hrl").

-define(SPEND_TX_VSN, 1).
-define(SPEND_TX_TYPE, spend_tx).

-record(spend_tx, {
          sender_id        :: aec_id:id(),
          recipient_id     :: aec_id:id(),
          amount    = 0    :: non_neg_integer(),
          fee       = 0    :: non_neg_integer(),
          ttl       = 0    :: aetx:tx_ttl(),
          nonce     = 0    :: non_neg_integer(),
          payload   = <<>> :: binary()}).

-opaque tx() :: #spend_tx{}.

-export_type([tx/0]).

-spec new(map()) -> {ok, aetx:tx()}.
new(#{sender_id    := SenderId,
      recipient_id := RecipientId,
      amount       := Amount,
      fee          := Fee,
      nonce        := Nonce,
      payload      := Payload} = Args)
  when is_integer(Amount), Amount >= 0,
       is_integer(Nonce), Nonce >= 0,
       is_integer(Fee), Fee >= 0,
       is_binary(Payload) ->
    assert_sender(SenderId),
    assert_recipient(RecipientId),
    Tx = #spend_tx{sender_id    = SenderId,
                   recipient_id = RecipientId,
                   amount       = Amount,
                   fee          = Fee,
                   ttl          = maps:get(ttl, Args, 0),
                   nonce        = Nonce,
                   payload      = Payload},
    {ok, aetx:new(?MODULE, Tx)}.

assert_sender(Id) ->
    case aec_id:specialize_type(Id) of
        account -> ok;
        Other   -> error({illegal_id_type, Other})
    end.

assert_recipient(Id) ->
    case aec_id:specialize_type(Id) of
        account  -> ok;
        name     -> ok;
        oracle   -> ok;
        contract -> ok;
        Other   -> error({illegal_id_type, Other})
    end.

-spec type() -> atom().
type() ->
    ?SPEND_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#spend_tx{fee = F}) ->
    F.

-spec gas(tx()) -> non_neg_integer().
gas(#spend_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#spend_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#spend_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#spend_tx{} = Tx) ->
    sender_pubkey(Tx).

-spec sender_id(tx()) -> aec_id:id().
sender_id(#spend_tx{sender_id = SenderId}) ->
    SenderId.

-spec sender_pubkey(tx()) -> aec_keys:pubkey().
sender_pubkey(#spend_tx{sender_id = SenderId}) ->
    aec_id:specialize(SenderId, account).

-spec recipient_id(tx()) -> aec_id:id().
recipient_id(#spend_tx{recipient_id = RecipientId}) ->
    RecipientId.

-spec amount(tx()) -> non_neg_integer().
amount(#spend_tx{amount = Amount}) ->
    Amount.

-spec payload(tx()) -> binary().
payload(#spend_tx{payload = Payload}) ->
    Payload.

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#spend_tx{}, Trees,_Env) ->
    %% Note: All checks are done in process/3
    {ok, Trees}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#spend_tx{} = Tx, _) -> {ok, [sender_pubkey(Tx)]}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
process(#spend_tx{} = SpendTx, Trees, Env) ->
    Instructions =
        aec_tx_processor:spend_tx_instructions(sender_pubkey(SpendTx),
                                               recipient_id(SpendTx),
                                               amount(SpendTx),
                                               fee(SpendTx),
                                               nonce(SpendTx)),
    aec_tx_processor:eval(Instructions, Trees, aetx_env:height(Env)).

serialize(#spend_tx{sender_id    = SenderId,
                    recipient_id = RecipientId,
                    amount       = Amount,
                    fee          = Fee,
                    ttl          = TTL,
                    nonce        = Nonce,
                    payload      = Payload}) ->
    {version(),
     [ {sender_id, SenderId}
     , {recipient_id, RecipientId}
     , {amount, Amount}
     , {fee, Fee}
     , {ttl, TTL}
     , {nonce, Nonce}
     , {payload, Payload}
     ]}.

deserialize(?SPEND_TX_VSN,
            [ {sender_id, SenderId}
            , {recipient_id, RecipientId}
            , {amount, Amount}
            , {fee, Fee}
            , {ttl, TTL}
            , {nonce, Nonce}
            , {payload, Payload}]) ->
    %% Asserts
    account = aec_id:specialize_type(SenderId),
    case aec_id:specialize_type(RecipientId) of
        account  -> ok;
        name     -> ok;
        oracle   -> ok;
        contract -> ok
    end,
    #spend_tx{sender_id    = SenderId,
              recipient_id = RecipientId,
              amount       = Amount,
              fee          = Fee,
              ttl          = TTL,
              nonce        = Nonce,
              payload      = Payload}.

serialization_template(?SPEND_TX_VSN) ->
    [ {sender_id, id}
    , {recipient_id, id}
    , {amount, int}
    , {fee, int}
    , {ttl, int}
    , {nonce, int}
    , {payload, binary}
    ].

for_client(#spend_tx{sender_id    = SenderId,
                     recipient_id = RecipientId,
                     amount       = Amount,
                     fee          = Fee,
                     ttl          = TTL,
                     nonce        = Nonce,
                     payload      = Payload}) ->
    #{<<"sender_id">>    => aehttp_api_encoder:encode(id_hash, SenderId),
      <<"recipient_id">> => aehttp_api_encoder:encode(id_hash, RecipientId),
      <<"amount">>       => Amount,
      <<"fee">>          => Fee,
      <<"ttl">>          => TTL,
      <<"nonce">>        => Nonce,
      <<"payload">>      => Payload}.

%% Internals

version() ->
    ?SPEND_TX_VSN.
