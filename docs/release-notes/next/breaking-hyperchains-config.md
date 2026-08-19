* **Breaking, Hyperchains configurations only.** `chain.consensus` was reworked; a
  7.2.2 configuration needs editing. Proof-of-Work configurations are unaffected.
  Stops the node with `unknown_consensus`: type `"hyper_chain"` (now
  `"hyperchain"`) and type `"smart_contract"` (removed with its module).
  Accepted in silence and then ignored, so delete them or they will appear to be
  set: `config.parent_chain.confirmations`, `config.parent_chain.producing_commitments`,
  `config.parent_chain.consensus.{amount,fee,spend_address}`,
  `config.stakers[].parent_chain_account`, `config.expected_key_block_rate` and
  `config.lazy_leader_trigger_time`. Do not expect configuration validation to find
  them: `chain.consensus` validates only entries at height 1 or above, and a
  Hyperchain's entry is the genesis one, keyed `"0"`. Even a missing or misspelled
  `type` there passes validation, and fails later as a crash rather than as
  `unknown_consensus`. See `docs/hyperchains.md` for the current shape.
