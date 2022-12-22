* Include fields `fee` and `gas_price` in GAMetaTx when computing the TX-hash
  of the inner transaction. This way a malicious miner can't change them before
  inserting the transaction in a micro block. Note: The authentication logic
  still needs to _actually_ use the Auth.TxHash during authentication for this
  to take effect!
