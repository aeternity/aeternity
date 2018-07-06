%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_spend_tx).

%% API
-export([new/1,
         type/0,
         fee/1,
         ttl/1,
         nonce/1,
         origin/1,
         recipient/1,
         check/5,
         process/5,
         signers/2,
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
          sender                    :: aec_id:id(),
          recipient                 :: aec_id:id(),
          amount    = 0             :: non_neg_integer(),
          fee       = 0             :: non_neg_integer(),
          ttl       = 0             :: aetx:tx_ttl(),
          nonce     = 0             :: non_neg_integer(),
          payload   = <<>>          :: binary()}).

-opaque tx() :: #spend_tx{}.

-export_type([tx/0]).

-spec new(map()) -> {ok, aetx:tx()}.
new(#{sender := Sender,
      recipient := Recipient,
      amount := Amount,
      fee := Fee,
      nonce := Nonce,
      payload := Payload} = Args) when is_integer(Amount), Amount >= 0,
                                       is_integer(Nonce), Nonce >= 0,
                                       is_integer(Fee), Fee >= 0,
                                       is_binary(Payload) ->
    assert_id(Sender),
    assert_id(Recipient),
    Tx = #spend_tx{sender = Sender,
                   recipient = Recipient,
                   amount = Amount,
                   fee = Fee,
                   ttl = maps:get(ttl, Args, 0),
                   nonce = Nonce,
                   payload = Payload},
    {ok, aetx:new(?MODULE, Tx)}.

assert_id(Id) ->
    case aec_id:specialize_type(Id) of
        account -> ok;
        name    -> ok;
        Other   -> error({illegal_id_type, Other})
    end.

-spec type() -> atom().
type() ->
    ?SPEND_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#spend_tx{fee = F}) ->
    F.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#spend_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#spend_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#spend_tx{} = Tx) ->
    sender_pubkey(Tx).

-spec sender(tx()) -> aec_id:id().
sender(#spend_tx{sender = Sender}) ->
    Sender.

-spec sender_pubkey(tx()) -> aec_keys:pubkey().
sender_pubkey(#spend_tx{sender = Sender}) ->
    aec_id:specialize(Sender, account).

-spec recipient(tx()) -> aec_id:id().
recipient(#spend_tx{recipient = Recipient}) ->
    Recipient.

resolve_recipient(#spend_tx{recipient = Recipient}, Trees) ->
    case aec_id:specialize(Recipient) of
        {account, RecipientPubkey} -> {ok, RecipientPubkey};
        {name, NameHash} -> aens:resolve_from_hash(account_pubkey, NameHash, aec_trees:ns(Trees))
    end.

-spec payload(tx()) -> binary().
payload(#spend_tx{payload = Payload}) ->
    Payload.

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#spend_tx{} = SpendTx, _Context, Trees, Height, _ConsensusVersion) ->
    Checks = [fun check_sender_account/3],
    case aeu_validation:run(Checks, [SpendTx, Trees, Height]) of
        ok ->
            case resolve_recipient(SpendTx, Trees) of
                {ok, RecipientPubkey} ->
                    {ok, aec_trees:ensure_account(RecipientPubkey, Trees)};
                {error, _} = E ->
                    E
            end;
        {error, _Reason} = Error ->
            Error
    end.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#spend_tx{} = Tx, _) -> {ok, [sender_pubkey(Tx)]}.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) -> {ok, aec_trees:trees()}.
process(#spend_tx{amount = Amount,
                  fee = Fee,
                  nonce = Nonce} = Tx, _Context, Trees0, _Height, _ConsensusVersion) ->
    SenderPubkey = sender_pubkey(Tx),
    {ok, RecipientPubkey} = resolve_recipient(Tx, Trees0),
    AccountsTrees0 = aec_trees:accounts(Trees0),

    {value, SenderAccount0} = aec_accounts_trees:lookup(SenderPubkey, AccountsTrees0),
    {ok, SenderAccount} = aec_accounts:spend(SenderAccount0, Amount + Fee, Nonce),
    AccountsTrees1 = aec_accounts_trees:enter(SenderAccount, AccountsTrees0),

    {value, RecipientAccount0} = aec_accounts_trees:lookup(RecipientPubkey, AccountsTrees1),
    {ok, RecipientAccount} = aec_accounts:earn(RecipientAccount0, Amount),
    AccountsTrees2 = aec_accounts_trees:enter(RecipientAccount, AccountsTrees1),

    Trees = aec_trees:set_accounts(Trees0, AccountsTrees2),
    {ok, Trees}.

serialize(#spend_tx{sender = Sender,
                    recipient = Recipient,
                    amount = Amount,
                    fee = Fee,
                    ttl = TTL,
                    nonce = Nonce,
                    payload = Payload}) ->
    {version(),
     [ {sender, Sender}
     , {recipient, Recipient}
     , {amount, Amount}
     , {fee, Fee}
     , {ttl, TTL}
     , {nonce, Nonce}
     , {payload, Payload}
     ]}.

deserialize(?SPEND_TX_VSN,
            [ {sender, Sender}
            , {recipient, Recipient}
            , {amount, Amount}
            , {fee, Fee}
            , {ttl, TTL}
            , {nonce, Nonce}
            , {payload, Payload}]) ->
    %% Asserts
    account = aec_id:specialize_type(Sender),
    case aec_id:specialize_type(Recipient) of
        account -> ok;
        name    -> ok
    end,
    #spend_tx{sender = Sender,
              recipient = Recipient,
              amount = Amount,
              fee = Fee,
              ttl = TTL,
              nonce = Nonce,
              payload = Payload}.

serialization_template(?SPEND_TX_VSN) ->
    [ {sender, id}
    , {recipient, id}
    , {amount, int}
    , {fee, int}
    , {ttl, int}
    , {nonce, int}
    , {payload, binary}
    ].

for_client(#spend_tx{amount = Amount,
                     fee = Fee,
                     ttl = TTL,
                     nonce = Nonce,
                     payload = Payload} = Tx) ->
    #{<<"sender">> => aec_base58c:encode(id_hash, sender(Tx)),
      <<"data_schema">> => <<"SpendTxJSON">>, % swagger schema name
      <<"recipient">> => aec_base58c:encode(id_hash, recipient(Tx)),
      <<"amount">> => Amount,
      <<"fee">> => Fee,
      <<"ttl">> => TTL,
      <<"nonce">> => Nonce,
      <<"payload">> => Payload,
      <<"vsn">> => version()}.

%% Internals

-spec check_sender_account(tx(), aec_trees:trees(), aec_blocks:height()) ->
                                  ok | {error, term()}.
check_sender_account(#spend_tx{amount = Amount,
                               fee = Fee, nonce = TxNonce } = Tx, Trees, _Height) ->
    SenderPubkey = sender_pubkey(Tx),
    aetx_utils:check_account(SenderPubkey, Trees, TxNonce, Fee + Amount).

version() ->
    ?SPEND_TX_VSN.
