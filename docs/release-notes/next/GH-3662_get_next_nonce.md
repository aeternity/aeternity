* Provides a new HTTP endpoint `/accounts/{pubkey}/next-nonce` for fetching
  the next account nonce. There are two strategies to be taken: provide a
  missing nonce if there is a gap in the transactions in the mempool or fetch
  an incremented highest nonce in the transaction pool.
