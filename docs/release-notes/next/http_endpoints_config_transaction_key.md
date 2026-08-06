* **Warning**: the config schema for `http > endpoints` now strictly validates its keys
  (`additionalProperties: false`). The transaction endpoints toggle has always been read by
  the node as `transaction` (singular) - a schema typo previously described it as `transactions`
  (plural), which was accepted by validation but silently had no effect. If your config still
  uses the old `transactions` key under `http > endpoints`, the node will now fail schema
  validation and refuse to start. Rename it to `transaction` before upgrading.
