* Added `mining.micro_block_candidate_timeout` (milliseconds, default `1000`), bounding the time a
  leader spends selecting transactions for one micro-block candidate. On expiry the candidate is
  published with the transactions packed so far instead of the whole build being lost. Set to `0`
  to restore unbounded selection. Builds that hit the limit log a warning and are counted by the
  new `ae.epoch.aecore.mining.micro_candidate_expired` metric. Keep it below the window your node
  builds in: `mining.micro_block_cycle` when mining, or `child_block_production_time` (default
  `500`) as a Hyperchains leader — a larger value is capped to that window. Only a value at or
  above `mining.micro_block_cycle` is warned about at startup.
* Candidate selection is faster on a node whose mempool has grown large: a sender's account state
  is resolved once per selection pass rather than once per transaction examined, and the set of
  already-packed transactions is no longer a list that every pass rescans.
