* Obsoletes the old State Channel off-chain transaction type which contained
  the list of updates that produced the latest `state_hash`. The new
  transaction type is available since Fortuna hard fork and the FSM is
  producing such transactions ever since. This detaches the off-chain protocol
  from the on-chain one and allows development of unique off-chain protocols
  that don't need their updates to be serializable on-chain.
