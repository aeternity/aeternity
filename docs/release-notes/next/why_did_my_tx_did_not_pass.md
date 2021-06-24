* Introduces a new endpoint for checking a transaction currently residing in
  the mempool: if it can be included by miners or it is blocked by something:
  not enough tokens, skipped nonce or something else. The endpoint is
  `/debug/check-tx/pool/{transaction-hash}`. It is a debug endpoint and should
  not be used in production.
