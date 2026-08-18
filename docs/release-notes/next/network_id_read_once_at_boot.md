* `fork_management: network_id` is now resolved once, while the node boots, and reused from
  then on instead of being resolved from the configuration on every use. The configuration
  file is only read at startup, so a running node already behaved this way; it now stops
  paying for the lookup per signature verified and per FATE `NETWORK_ID` instruction.
