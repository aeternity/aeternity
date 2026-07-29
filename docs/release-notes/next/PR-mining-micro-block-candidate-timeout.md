* Added `mining.micro_block_candidate_timeout` (milliseconds, default `1000`), bounding the
  time a leader may spend selecting transactions for one micro-block candidate. On expiry the
  candidate is published with the transactions packed so far instead of the whole build being
  lost, so a pool shaped such that selection cannot make progress no longer costs the leader a
  whole micro block cycle. Selection in progress is never interrupted, so a build may overrun
  the limit by one batch of transactions plus the time to apply it; keep the value comfortably
  below `mining.micro_block_cycle`. Set to `0` to restore unbounded selection.
  Builds that hit the limit log a warning and are counted by the new
  `tx_pool.candidate.expired` and `tx_pool.candidate.expired_txs` metrics.
* Sped up micro-block candidate selection: the account state of a sender is now resolved once
  per selection pass rather than once per transaction examined, and the set of already-packed
  transactions is no longer a list that every pass rescans. Both matter most on a node whose
  mempool has grown large, where selection walks many transactions per candidate.
