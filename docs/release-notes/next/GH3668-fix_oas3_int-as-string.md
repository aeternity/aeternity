* Fixes a bug when fetching a transaction via the HTTP API and the flag
  `int-as-string` is rised. This used to fail the specification and the
  request was crashing.
