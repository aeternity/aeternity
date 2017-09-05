-module(aec_accounts).

%% API
-export([get/2,
         get_with_proofs/2,
         put/2,
         earn/3]).

-include("trees.hrl").

get(Pubkey, _AccountsTree) ->
    %% To be implemented when we have state Merkle trees
    %% to hold accounts with balances
    Account = #account{pubkey = Pubkey,
                       balance = 0},
    {ok, Account}.

get_with_proofs(Pubkey, _AccountsTree) ->
    %% To be implemented when we have state Merkle trees
    %% to hold accounts with balances
    Account = #account{pubkey = Pubkey,
                       balance = 0},
    {ok, {Account, [<<"fakeproof">>]}}.

put(_Account, AccountsTrees) ->
    %% To be implemented when we have state Merkle trees
    %% to hold accounts with balances
    {ok, AccountsTrees}.

earn(#account{balance = Balance0}, Amount, Height) ->
    {ok, #account{balance = Balance0 + Amount,
                  height = Height}}.
