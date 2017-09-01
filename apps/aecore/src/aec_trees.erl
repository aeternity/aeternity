-module(aec_trees).

-include("trees.hrl").

%% API
-export([accounts/1,
         with_accounts/2]).

accounts(Trees) ->
    Trees#trees.accounts.

with_accounts(Trees, Accounts) ->
    Trees#trees{accounts = Accounts}.
