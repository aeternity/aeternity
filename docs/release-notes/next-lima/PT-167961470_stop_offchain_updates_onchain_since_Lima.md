* Obsoletes the old State Channel off-chain transaction type which contained
  the list of updates that produced the latest `state_hash`. The new
  transaction type is available since Fortuna hard fork and the FSM is
  producing such transactions ever since. This detaches the off-chain protocol
  from the on-chain one and allows development of unique off-chain protocols
  that don't need their updates to be serializable on-chain. From Lima hard
  fork on, old version off-chain transactions will not be allowed as a payload
  in on-chain dispute transactions. For currently existing state channels which
  latest co-authenticated state is an old version off-chain transaction is
  suggested to make a new co-authenticated transaction that is using the new
  serialization. If the other party refuses to authenticate a new round or is
  simply missing, one can use the solo closing sequence instead.
