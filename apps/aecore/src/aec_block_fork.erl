-module(aec_block_fork).

-export([apply_minerva/1,
         apply_fortuna/1,
         apply_lima/1
        ]).

-spec apply_minerva(aec_trees:trees()) -> aec_trees:trees().
apply_minerva(Trees) ->
    apply_accounts_file(Trees, aec_fork_block_settings:minerva_accounts()).

-spec apply_fortuna(aec_trees:trees()) -> aec_trees:trees().
apply_fortuna(Trees) ->
    apply_accounts_file(Trees, aec_fork_block_settings:fortuna_accounts()).

-spec apply_lima(aec_trees:trees()) -> aec_trees:trees().
apply_lima(Trees) ->
    apply_accounts_file(Trees, aec_fork_block_settings:lima_accounts()).

apply_accounts_file(Trees, Accounts) ->
    AccTrees =
        lists:foldl(
            fun({Pubkey, Balance}, AccumAccTrees) when
                  is_integer(Balance) andalso Balance >= 0
                  andalso byte_size(Pubkey) =:= 32 ->
                Account =
                    case aec_accounts_trees:lookup(Pubkey, AccumAccTrees) of
                        none -> aec_accounts:new(Pubkey, Balance);
                        {value, OldAcc} ->
                            {ok, NewAcc} = aec_accounts:earn(OldAcc, Balance),
                            NewAcc
                    end,
                aec_accounts_trees:enter(Account, AccumAccTrees)
            end,
            aec_trees:accounts(Trees),
            Accounts),
    aec_trees:set_accounts(Trees, AccTrees).

