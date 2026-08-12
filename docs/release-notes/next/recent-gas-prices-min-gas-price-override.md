* A config option, `http:dry_run:min_gas_price_override` (integer, in aettos) has been introduced.
  It is **disabled by default**: absent or `null` means off, which is the behaviour of every
  existing node. When it is set, `GET /v2/recent-gas-prices` and `GET /v3/recent-gas-prices`
  report `max(observed, override)` as `min_gas_price` — a floor, never a substitution, so the
  advertised figure can only be raised and can never fall below the price the chain actually
  shows. `utilization` is reported unchanged, computed from the chain as before.

  This is a **reporting-only** setting. It is enforced nowhere: no transaction is admitted,
  rejected, gossiped or included differently because of it, and it does not affect
  `mining:min_miner_gas_price`, which is the floor the mempool really applies (at admission in
  `aec_tx_pool:check_minimum_miner_gas_price/6`, and at candidate selection in
  `aec_tx_pool:check_candidate/10`). Operators wanting a floor that is actually enforced — and
  that then raises `recent-gas-prices` truthfully, because that endpoint folds the prices of
  transactions which really were mined — should set `mining:min_miner_gas_price` instead.

  Operators should understand what enabling it does. Published SDKs read the 1-minute bucket of
  this endpoint and build transactions against it once utilization reaches 70%, so a node with
  this set causes clients to pay the reported price even though no miner requires it. There is
  no version gate and nothing in the response marks the figure as configured rather than
  observed.

  The ask this implements was phrased as two settings — an enable/disable flag plus a nullable
  value. It ships as **one** knob: a boolean beside a nullable integer gives four states of which
  two are meaningless (enabled with no value, disabled with one), and the neighbouring
  `http:dry_run` settings are all single values. Absent/null is the off state, so nothing is lost.
