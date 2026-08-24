* Dry-run gas estimates (`internal`/`public` dry-run endpoints) now meter FATE
  contract-storage reads at the size-proportional Arcus (v7) gas cost by default,
  even before Arcus activates on the chain. This only changes the returned gas
  amounts - never tx validity, ABI/VM version, or the response schema - and bounds
  large-store reads by the dry-run gas ceiling. Each repriced store *register* read
  is floored at what the activated protocol would charge for it, and every other
  store read is only ever charged more, so the step-up can only ever report the
  same or more gas than the call costs on chain today - at most one flat pre-Arcus
  charge (2000 gas) per register read more. Historical replay (Rosetta/indexer
  reconstruction) and mempool includability checks are unaffected and keep
  reporting the currently activated protocol's cost. Disable via
  `http.dry_run.store_read_gas_metering: false` to report the currently activated
  protocol's flat charge instead. The setting only governs this forward step-up:
  once Arcus activates the size-proportional charge is the activated cost, and
  dry-run reports it either way.
