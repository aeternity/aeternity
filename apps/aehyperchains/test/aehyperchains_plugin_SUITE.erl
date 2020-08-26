-module(aehyperchains_plugin_SUITE).


%add_spend_tx(Node, Amount, Fee, Nonce, TTL, Sender, Recipient) ->
 %   SenderId = aeser_id:create(account, maps:get(pubkey, Sender)),
  %  RecipientId = aeser_id:create(account, Recipient),
   % Params = #{ sender_id    => SenderId,
    %            recipient_id => RecipientId,
     %           amount       => Amount,
      %          nonce        => Nonce,
       %         ttl          => TTL,
        %        payload      => <<>>,
         %       fee          => Fee },
    %{ok, Tx} = aec_spend_tx:new(Params),
   % STx = aec_test_utils:sign_tx(Tx, maps:get(privkey, Sender)),
   % Res = rpc:call(Node, aec_tx_pool, push, [STx]),
   % {Res, aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx))}.
