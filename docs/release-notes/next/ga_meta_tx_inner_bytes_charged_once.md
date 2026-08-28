* A generalized-account meta transaction whose inner transaction is applied no longer pays
  for the inner transaction's bytes twice. The meta envelope's size includes the inner
  transaction, and the recursive charge for the inner transaction added those same bytes
  again; the envelope is now charged for its own bytes only. This is a consensus change
  gated on the Arcus (v7) protocol, and no Arcus height is scheduled yet: until one is set
  every network keeps the current amount, so blocks already on chain replay unchanged.
  From Arcus onward such a transaction's reported `used_gas` falls by 20 gas per byte of the
  inner transaction - 1,580 gas for an empty spend and 19,640 for one carrying a 900-byte
  payload.
  A meta transaction whose inner transaction failed is unaffected at every protocol, since
  the inner transaction is never charged for itself in that case.
