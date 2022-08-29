* Introduces a new protocol for PoS consensuses. It is ruled by a smart
  contract.
* Introduces two smart contracts to govern a PoS consensus
* Keyblocks are signed by the miner. This is validated once nodes are
  syncing. Since backwards compatibility is required, the keyblock's seal is
  reused for this (where the PoW solution is stored), while the excess is
  padded with zeroes
* Block rewards pass through the smart contract.
* A parent chain connector is introduced. It consumes the parent chain node's API
  and will be used to feed the child chain's consensus. An AE2AE connector is
  fully operational while a AE2BTC one is stubbed. Since this is mostly in RnD
  state, once the details are worked out using the AE2AE, the AE2BTC will be
  filled in.
* The consensus smart contract is extended to have the following
  functionalities:
  * Anyone can become a validator if they have enough child chain coins.
    Validators can set themselves as ONLINE or OFFLINE - only ONLINE ones are
    considered while electing a new leader
  * Users who don't have enough to stake can delegate their staking power to a
    validator of their choice, thus becoming a delegate
  * Validators have withdraw limits depending on total staking power or
    percentage of their own stake in their pool
  * Validators have branding fields in the contract - name, description and
    avatar url
  * There is a preconfigurable delay for staking and another one for unstaking
  * There is a preconfigurable delay before a validator counts as online

