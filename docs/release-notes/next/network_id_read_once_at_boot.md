* `fork_management: network_id` is now read once, while the node boots, and is fixed for
  the lifetime of the node, instead of being resolved from the configuration on every
  use. The configuration file is only read at startup, so this is what a running node
  already did in practice; it now stops paying for the lookup per signature verified and
  per FATE `NETWORK_ID` instruction.
