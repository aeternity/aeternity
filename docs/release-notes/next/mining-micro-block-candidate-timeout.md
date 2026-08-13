* Added `mining.micro_block_candidate_timeout` (milliseconds, default `1000`), bounding the time a
  leader spends selecting transactions for one micro-block candidate. On expiry the candidate is
  published with the transactions packed so far instead of the whole build being lost. Set to `0`
  to restore unbounded selection. Builds that hit the limit log a warning and are counted by the
  new `ae.epoch.aecore.mining.micro_candidate_expired` metric. Keep it below the window your node
  builds in: `mining.micro_block_cycle` when mining, or `child_block_production_time` (default
  `500`) as a Hyperchains leader — a larger value is capped to that window. Only a value at or
  above `mining.micro_block_cycle` is warned about at startup. `docs/configuration.md` covers
  what the limit deliberately does not do.
* Micro-block candidate selection now follows each sender's nonce sequence instead of taking
  transactions in fee order alone. Senders are still served in fee order; only the order *within*
  one sender has changed. A transaction whose nonce the chain is not ready for cannot be applied on
  top of the block being built, however much it pays, so selecting it spent the candidate's gas on
  an apply that was going to fail and sent the build round again for more. A sender with a long
  queue of varying fees — as one client mixing spends with contract calls produces — could keep a
  leader busy for a whole micro block cycle and have it produce nothing.

  A transaction held up behind a gap in its sender's nonces is therefore no longer selected, so it
  no longer counts as an apply failure it never had, and is no longer retired by
  `mempool.tx_failures.common.tx_nonce_too_high_for_account` (default `30` failures); it stays
  until the gap before it fills or its mempool stay runs out.
* Candidate selection is faster on a node whose mempool has grown large: a sender's account state
  is resolved once per selection pass rather than once per transaction examined, the set of
  already-packed transactions is no longer a list that every pass rescans, and a sender that can
  give the candidate nothing more is skipped for the rest of the walk.
* The transactions of one account from `GET /accounts/{pubkey}/transactions/pending` are still in
  nonce order, but where several compete for the same nonce the dearest now comes first rather than
  an arbitrary one. Only one of them can ever be mined.
