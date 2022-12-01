* Introduces posting of commitments on the parent chain. Each commitment
  uses a key block hash to represent the child chain on the parent chain.
  Since there is a delay caused of the number of confirmations on the parent
  chain block, the commitments are offset with as many blocks as the number of
  commitments is. This also means that the first commitments are based on the
  genesis block itself.
* Revisits heavily the config, notable changes are:
    * there is a section representing the parent chain consensus: with a
      `type` (now AE2AE), parent chain `network_id` and parent chain
      `spend_address` where all commitments are sent to. The latter one is
      likely to be removed.
    * there is a specific section for parent chain nodes polling
    * the keys now support parent chain keys as well
