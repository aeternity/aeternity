* Dry-run gas estimates (`internal`/`public` dry-run endpoints) now meter FATE
  contract-storage reads at the size-proportional Salus (v8) gas cost by default,
  even before Salus activates on the chain. This only changes the returned gas
  amounts - never tx validity, ABI/VM version, or the response schema - and bounds
  large-store reads by the dry-run gas ceiling. Historical replay (Rosetta/indexer
  reconstruction) and mempool includability checks are unaffected and keep
  reporting the currently activated protocol's cost. Disable via
  `http.dry_run.salus_gas_metering: false` to fall back to the activated
  protocol's flat charge.
