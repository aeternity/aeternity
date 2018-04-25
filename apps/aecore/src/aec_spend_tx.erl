%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_spend_tx).

%% API
-export([new/1,
         type/0,
         fee/1,
         nonce/1,
         origin/1,
         recipient/1,
         check/5,
         process/5,
         accounts/1,
         signers/1,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

-export([payload/1]).

-behavior(aetx).

-include("common.hrl").
-include("blocks.hrl").

-define(SPEND_TX_VSN, 2).
-define(SPEND_TX_NO_PAYLOAD_VSN, 1).
-define(SPEND_TX_TYPE, spend_tx).

-define(is_tx_vsn_applicable_at_consensus_vsn(TxV, CV),
        ( ((TxV =:= ?SPEND_TX_NO_PAYLOAD_VSN) and (CV =< ?CONSENSUS_V_0_11_0_VERSION))
          or ((TxV =:= ?SPEND_TX_VSN) and (CV > ?CONSENSUS_V_0_11_0_VERSION))
        )).

-ifdef(TEST).
-export([version/1]).
-endif.

-record(spend_tx, {
          sender    = <<>>          :: pubkey(),
          recipient = <<>>          :: pubkey(),
          amount    = 0             :: non_neg_integer(),
          fee       = 0             :: non_neg_integer(),
          nonce     = 0             :: non_neg_integer(),
          payload   = <<>>          :: binary(),
          vsn       = ?SPEND_TX_VSN :: non_neg_integer()}).

-opaque tx() :: #spend_tx{}.

-export_type([tx/0]).

-spec new(map()) -> {ok, aetx:tx()}.
new(#{sender := SenderPubkey,
      recipient := RecipientPubkey,
      amount := Amount,
      fee := Fee,
      nonce := Nonce}=M) when is_integer(Amount), Amount >= 0,
                              is_integer(Nonce), Nonce >= 0,
                              is_integer(Fee), Fee >= 0,
                              is_binary(SenderPubkey),
                              is_binary(RecipientPubkey)
                              ->
    Vsn = maps:get(vsn, M, ?SPEND_TX_VSN),
    Payload =
        case Vsn of
            ?SPEND_TX_NO_PAYLOAD_VSN -> <<>>;
            ?SPEND_TX_VSN -> maps:get(payload, M, <<>>)
        end,
    Tx = #spend_tx{sender = SenderPubkey,
                   recipient = RecipientPubkey,
                   amount = Amount,
                   fee = Fee,
                   nonce = Nonce,
                   payload = Payload,
                   vsn = Vsn},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?SPEND_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#spend_tx{fee = F}) ->
    F.

-spec nonce(tx()) -> non_neg_integer().
nonce(#spend_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#spend_tx{sender = Sender}) ->
    Sender.

-spec recipient(tx()) -> pubkey().
recipient(#spend_tx{recipient = Recipient}) ->
    Recipient.

-spec payload(tx()) -> binary().
payload(#spend_tx{payload = Payload}) ->
    Payload.

-ifdef(TEST).
-spec version(tx()) -> non_neg_integer().
version(#spend_tx{vsn = Vsn}) ->
    Vsn.
-endif.

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), height(), non_neg_integer()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#spend_tx{vsn = Vsn} = SpendTx, _Context, Trees0, Height, ConsensusVersion)
  when ?is_tx_vsn_applicable_at_consensus_vsn(Vsn, ConsensusVersion) ->
    RecipientPubkey = recipient(SpendTx),
    Checks = [fun check_tx_fee/3,
              fun check_sender_account/3],
    case aeu_validation:run(Checks, [SpendTx, Trees0, Height]) of
        ok ->
            case aec_trees:ensure_account_at_height(RecipientPubkey, Trees0, Height) of
                {ok, Trees} ->
                    {ok, Trees};
                {error, account_height_too_big} ->
                    {error, recipient_account_height_too_big}
            end;
        {error, _Reason} = Error ->
            Error
    end;
check(#spend_tx{vsn = Vsn} = SpendTx, _Context, Trees0, Height, ConsensusVersion)
  when not ?is_tx_vsn_applicable_at_consensus_vsn(Vsn, ConsensusVersion) ->
    {error, tx_version_not_applicable_at_consensus_version}.

-spec accounts(tx()) -> [pubkey()].
accounts(#spend_tx{sender = SenderPubKey, recipient = RecipientPubKey}) ->
    [SenderPubKey, RecipientPubKey].

-spec signers(tx()) -> [pubkey()].
signers(#spend_tx{sender = SenderPubKey}) -> [SenderPubKey].

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), height(), non_neg_integer()) -> {ok, aec_trees:trees()}.
process(#spend_tx{sender = SenderPubkey,
                  recipient = RecipientPubkey,
                  amount = Amount,
                  fee = Fee,
                  nonce = Nonce,
                  vsn = Vsn}, _Context, Trees0, Height, ConsensusVersion)
  when ?is_tx_vsn_applicable_at_consensus_vsn(Vsn, ConsensusVersion) ->
    AccountsTrees0 = aec_trees:accounts(Trees0),

    {value, SenderAccount0} = aec_accounts_trees:lookup(SenderPubkey, AccountsTrees0),
    {ok, SenderAccount} = aec_accounts:spend(SenderAccount0, Amount + Fee, Nonce, Height),
    AccountsTrees1 = aec_accounts_trees:enter(SenderAccount, AccountsTrees0),

    {value, RecipientAccount0} = aec_accounts_trees:lookup(RecipientPubkey, AccountsTrees1),
    {ok, RecipientAccount} = aec_accounts:earn(RecipientAccount0, Amount, Height),
    AccountsTrees2 = aec_accounts_trees:enter(RecipientAccount, AccountsTrees1),

    Trees = aec_trees:set_accounts(Trees0, AccountsTrees2),
    {ok, Trees}.

serialize(#spend_tx{sender = Sender,
                    recipient = Recipient,
                    amount = Amount,
                    fee = Fee,
                    nonce = Nonce,
                    vsn = Vsn = ?SPEND_TX_NO_PAYLOAD_VSN}) ->
    {Vsn,
     [ {sender, Sender}
     , {recipient, Recipient}
     , {amount, Amount}
     , {fee, Fee}
     , {nonce, Nonce}
     ]};
serialize(#spend_tx{sender = Sender,
                    recipient = Recipient,
                    amount = Amount,
                    fee = Fee,
                    nonce = Nonce,
                    payload = Payload,
                    vsn = Vsn = ?SPEND_TX_VSN}) ->
    {Vsn,
     [ {sender, Sender}
     , {recipient, Recipient}
     , {amount, Amount}
     , {fee, Fee}
     , {nonce, Nonce}
     , {payload, Payload}
     ]}.

deserialize(Vsn = ?SPEND_TX_NO_PAYLOAD_VSN,
            [ {sender, Sender}
            , {recipient, Recipient}
            , {amount, Amount}
            , {fee, Fee}
            , {nonce, Nonce}]) ->
    #spend_tx{sender = Sender,
              recipient = Recipient,
              amount = Amount,
              fee = Fee,
              nonce = Nonce,
              vsn = Vsn};
deserialize(Vsn = ?SPEND_TX_VSN,
            [ {sender, Sender}
            , {recipient, Recipient}
            , {amount, Amount}
            , {fee, Fee}
            , {nonce, Nonce}
            , {payload, Payload}]) ->
    #spend_tx{sender = Sender,
              recipient = Recipient,
              amount = Amount,
              fee = Fee,
              nonce = Nonce,
              payload = Payload,
              vsn = Vsn}.

serialization_template(?SPEND_TX_NO_PAYLOAD_VSN) ->
    [ {sender, binary}
    , {recipient, binary}
    , {amount, int}
    , {fee, int}
    , {nonce, int}
    ];
serialization_template(?SPEND_TX_VSN) ->
    [ {sender, binary}
    , {recipient, binary}
    , {amount, int}
    , {fee, int}
    , {nonce, int}
    , {payload, binary}
    ].

for_client(#spend_tx{sender = Sender,
                     recipient = Recipient,
                     amount = Amount,
                     fee = Fee,
                     nonce = Nonce,
                     payload = Payload,
                     vsn = Vsn}) ->
    #{<<"sender">> => aec_base58c:encode(account_pubkey, Sender),
      <<"data_schema">> => <<"SpendTxJSON">>, % swagger schema name
      <<"recipient">> => aec_base58c:encode(account_pubkey, Recipient),
      <<"amount">> => Amount,
      <<"fee">> => Fee,
      <<"nonce">> => Nonce,
      <<"payload">> => Payload,
      <<"vsn">> => Vsn}.

%% Internals

-spec check_tx_fee(tx(), aec_trees:trees(), height()) ->
                          ok | {error, too_low_fee}.
check_tx_fee(#spend_tx{fee = Fee}, _Trees, _Height) ->
    case Fee >= aec_governance:minimum_tx_fee() of
        true ->
            ok;
        false ->
            {error, too_low_fee}
    end.

-spec check_sender_account(tx(), aec_trees:trees(), height()) ->
                                  ok | {error, term()}.
check_sender_account(#spend_tx{sender = SenderPubkey, amount = Amount,
                               fee = Fee, nonce = TxNonce }, Trees, Height) ->
    aetx_utils:check_account(SenderPubkey, Trees, Height, TxNonce, Fee + Amount).
