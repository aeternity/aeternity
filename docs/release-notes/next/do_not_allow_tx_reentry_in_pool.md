* If a transaction had been discarded from the tx pool, it can reenter. This
  PR provides the functionality for the node operator to define their own
  strategy if the node shall allow reentry of already discarded transactions.
  This is configurable in the config as `mempool/allow_reentry_of_txs` and the
  default is `false`.
