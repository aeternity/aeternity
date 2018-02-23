%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_spend_tx).

%% API
-export([new/1,
         fee/1,
         nonce/1,
         origin/1,
         recipient/1,
         check/3,
         process/3,
         accounts/1,
         signers/1,
         serialize/1,
         deserialize/1,
         for_client/1]).

-behavior(aetx).

-include("common.hrl").

-record(spend_tx, {
          sender    = <<>> :: pubkey(),
          recipient = <<>> :: pubkey(),
          amount    = 0    :: non_neg_integer(),
          fee       = 0    :: non_neg_integer(),
          nonce     = 0    :: non_neg_integer()}).
-opaque tx() :: #spend_tx{}.

-export_type([tx/0]).

-spec new(map()) -> {ok, aetx:tx()}.
new(#{sender := SenderPubkey,
      recipient := RecipientPubkey,
      amount := Amount,
      fee := Fee,
      nonce := Nonce}) ->
    Tx = #spend_tx{sender = SenderPubkey,
                   recipient = RecipientPubkey,
                   amount = Amount,
                   fee = Fee,
                   nonce = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

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

-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#spend_tx{recipient = RecipientPubkey} = SpendTx, Trees0, Height) ->
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
    end.

-spec accounts(tx()) -> [pubkey()].
accounts(#spend_tx{sender = SenderPubKey, recipient = RecipientPubKey}) ->
    [SenderPubKey, RecipientPubKey].

-spec signers(tx()) -> [pubkey()].
signers(#spend_tx{sender = SenderPubKey}) -> [SenderPubKey].

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#spend_tx{sender = SenderPubkey,
                  recipient = RecipientPubkey,
                  amount = Amount,
                  fee = Fee,
                  nonce = Nonce}, Trees0, Height) ->
    AccountsTrees0 = aec_trees:accounts(Trees0),

    {value, SenderAccount0} = aec_accounts_trees:lookup(SenderPubkey, AccountsTrees0),
    {ok, SenderAccount} = aec_accounts:spend(SenderAccount0, Amount + Fee, Nonce, Height),
    AccountsTrees1 = aec_accounts_trees:enter(SenderAccount, AccountsTrees0),

    {value, RecipientAccount0} = aec_accounts_trees:lookup(RecipientPubkey, AccountsTrees1),
    {ok, RecipientAccount} = aec_accounts:earn(RecipientAccount0, Amount, Height),
    AccountsTrees2 = aec_accounts_trees:enter(RecipientAccount, AccountsTrees1),

    Trees = aec_trees:set_accounts(Trees0, AccountsTrees2),
    {ok, Trees}.

-define(SPEND_TX_VSN, 1).

serialize(#spend_tx{sender = Sender,
                    recipient = Recipient,
                    amount = Amount,
                    fee = Fee,
                    nonce = Nonce}) ->
    [#{<<"vsn">> => version()},
     #{<<"sender">> => Sender},
     #{<<"recipient">> => Recipient},
     #{<<"amount">> => Amount},
     #{<<"fee">> => Fee},
     #{<<"nonce">> => Nonce}].

deserialize([#{<<"vsn">>  := ?SPEND_TX_VSN},
             #{<<"sender">> := Sender},
             #{<<"recipient">> := Recipient},
             #{<<"amount">> := Amount},
             #{<<"fee">> := Fee},
             #{<<"nonce">> := Nonce}]) ->
    #spend_tx{sender = Sender,
              recipient = Recipient,
              amount = Amount,
              fee = Fee,
              nonce = Nonce}.

for_client(#spend_tx{sender = Sender,
                     recipient = Recipient,
                     amount = Amount,
                     fee = Fee,
                     nonce = Nonce}) ->
    #{<<"sender">> => aec_base58c:encode(account_pubkey, Sender),
      <<"data_schema">> => <<"SpendTxJSON">>, % swagger schema name
      <<"recipient">> => aec_base58c:encode(account_pubkey, Recipient),
      <<"amount">> => Amount,
      <<"fee">> => Fee,
      <<"nonce">> => Nonce,
      <<"vsn">> => ?SPEND_TX_VSN}.

version() ->
    ?SPEND_TX_VSN.

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
