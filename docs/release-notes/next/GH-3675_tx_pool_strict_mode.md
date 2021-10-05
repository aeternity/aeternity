* Introduces a new `strict` mode in the tx-pool. It allows the node operator
  to specify different maximum attempts per error type of a transaction, thus
  allowing a higher grace period for some transactions to become valid while
  limiting other errors. All error types can be specified in the config but
  some come with sensible defaults. This feature can also be turned off in the
  config.
