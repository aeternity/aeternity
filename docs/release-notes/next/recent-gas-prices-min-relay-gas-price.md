* A config option, `http:gas_price:min_relay_gas_price` (integer, in aettos; equally settable as
  `AE__HTTP__GAS_PRICE__MIN_RELAY_GAS_PRICE`): a reporting-only gas price floor, applied to the
  prices this node reports on `GET /v[23]/recent-gas-prices` and in public dry-run results.

* A config option, `http:gas_price:reporting_utilization_override` (integer, `0`–`100`; equally
  settable as `AE__HTTP__GAS_PRICE__REPORTING_UTILIZATION_OVERRIDE`): the utilization percentage
  reported alongside a raised price floor. `0` reports utilization as observed.

* **A window with no observed price is exempt from both.** `recent-gas-prices` reports `0` for a
  window no micro block fell inside — that is "no data", not a cheap chain — and such a window
  passes through untouched in both fields.

* Both are **off by default** — `min_relay_gas_price` is disabled when absent or `0`, and
  `reporting_utilization_override` defaults to `0` — so an existing node behaves exactly as before.
  Both are advanced settings; leave them unset unless you have a specific reason.
