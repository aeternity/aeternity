* Provides a new HTTP endpoint `/accounts/{pubkey}/next-nonce` for fetching
  the next account nonce. This is the maximum of current account nonce and the
  biggest nonce in the mempool. This might yeld unexepected results: contents
  of the transaction mempool depend on receiving those transactions in the
  first place. That's why the new endpoint does not check if there are missing
  nonces in the transactions in the mempool.
