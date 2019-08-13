* Extends transaction signing - also allow signatures of the transaction hash.
  The transaction serialization is potentially long, so signing the
  transaction hash means less cryptographic work.
