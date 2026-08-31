* A config option, `monitoring:publisher:fee` (integer, in aettos; equally settable as
  `AE__MONITORING__PUBLISHER__FEE`, default `0`): a fee floor for the periodic monitoring spend
  transaction. The fee actually used is the largest of this, the protocol minimum, and the current
  miner gas price times the transaction's gas limit — the transaction was previously always priced
  at whichever of the latter two was greater, with no way to raise it further.
