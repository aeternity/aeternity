* FATE contract-storage reads are charged in proportion to the size of the value
  read (a fixed floor plus a per-byte rate) instead of a flat amount, so gas
  reflects the work a read actually does. This is a consensus change gated on the
  Arcus (v7) protocol: it takes effect for state-changing transactions only from
  the height at which Arcus activates, and no such height is scheduled yet. Until
  then every network keeps the current flat charge, and blocks already on chain
  keep replaying under the frozen pre-Arcus store logic.
  Note that dry-run gas estimates are metered at the new Arcus cost right away
  (see the dry-run release note), so an estimate can legitimately be higher than
  what the same call currently costs on chain - but never lower, since each
  repriced store register read is floored at the activated protocol's charge, and
  every other store read is only ever charged more. Contracts that read large
  values from their store - in particular via large store maps - should be
  re-estimated before an Arcus height is set.
