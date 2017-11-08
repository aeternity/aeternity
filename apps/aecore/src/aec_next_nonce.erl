-module(aec_next_nonce).

-export([for_account/0]).

for_account() ->
    %% TODO: Nonce shall not be determined on accounts state tree only
    %% To be implemented in PT-152543561
    case aec_keys:pubkey() of
        {ok, SenderPubkey} ->
            {ok, LastBlock} = aec_chain:top(),
            Trees = aec_blocks:trees(LastBlock),
            AccountsTrees = aec_trees:accounts(Trees),
            {ok, Account} = aec_accounts:get(SenderPubkey, AccountsTrees),
            {ok, aec_accounts:nonce(Account) + 1};
        {error, key_not_found} = Error ->
            Error
    end.
