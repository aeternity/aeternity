-include("common.hrl").

-record(trees, {
          accounts}).
-type(trees() :: #trees{}).

%% Placeholder to define state Merkle trees

-record(account, {
          pubkey = <<>>  :: pubkey(),
          balance = 0    :: non_neg_integer(),
          nonce = 0      :: non_neg_integer(),
          height = 0     :: non_neg_integer()}).
