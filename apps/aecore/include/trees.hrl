-record(trees, {
          accounts :: aec_accounts_trees:tree(),
          oracles  :: aeo_state_tree:tree(),
          ns       :: aens_state_tree:tree()}).
-type(trees() :: #trees{}).

%% Placeholder to define state Merkle trees

-record(account, {
          pubkey = <<>>  :: pubkey(),
          balance = 0    :: non_neg_integer(),
          nonce = 0      :: non_neg_integer(),
          height = 0     :: height()}).
-type(account() :: #account{}).
