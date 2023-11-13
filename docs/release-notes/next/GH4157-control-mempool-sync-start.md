* A config option, `mempool:sync_start` (integer) has been introduced to control when syncing of the mempool
  starts during chain sync. A positive number denotes the height at which to begin syncing; a negative number
  denotes how far from the network top height (best guess by the sync logic) to start. Default is `-500`, i.e.
  start syncing the mempool 500 blocks from the top. A mempool sync is always triggered when chain sync is done,
  and if a negative value greater than the top height is given, sync starts from `0`, i.e. from the beginning.