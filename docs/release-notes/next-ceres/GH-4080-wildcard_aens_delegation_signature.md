* Add generic/wildcard AENS delegation signatures. I.e. instead of delegating
  authority for a contract to operate with a specific name (name hash), by
  signing just the string `AENS` (+ network id, public key and contract address
  as usual) you can delegate the authority for a contract to handle all your
  names with one signature. See [Issue
  #4080](https://github.com/aeternity/aeternity/issues/4080) for details.

  BEWARE: This gives the contrac authority to handle all current _and future_
  names on your behalf, so it should be used with extreme care and only for
  wellknown (and well understood!!) contracts.
