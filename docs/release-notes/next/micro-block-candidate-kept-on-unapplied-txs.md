* A leader no longer discards a ready micro-block candidate when a newly arrived
  transaction cannot be added to it. Failing to extend a candidate is routine - a
  future nonce, no gas left, a sender that has already spent its balance in the
  block being built - but it was handled as a lost candidate: `get_candidate`
  answered `no_candidate` and the whole mempool was walked again to build a
  replacement, which any such transaction could trigger at will and without ever
  paying a fee. Genuine build failures and worker crashes still rebuild.
  One side effect: the dropped rebuild was also what reported such a transaction to
  the mempool as failed, so it is now counted once per rebuild rather than once per
  arrival and takes longer to reach the limit at which the pool drops it.
