# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v6.6.0) is a maintenance Iris and HyperChains release.

It:

* A new command bin/aeternity cli has been added which provides access to an online
  interactive CLI for exploring the status of the system and changing operational state

* The default provided bin/aeternity script has been replaced by one customised to the aeternity project

* The `bin/aeternity status` command now prints a subset of the output from `curl http://localhost:3013/v2/status` in a human friendly form.

* WARNING - backwards incompatibility. This change removes the following currently unsupported commands:
  `upgrade|downgrade|install|uninstall|unpack|versions|export|reboot`

* Upgrade to a new and much improved rocksdb mnesia backend plugin. This also enables a direct access API (optional) for greater speed and stability. See `configuration.md` for details.

* Drop support of OTP22

* Drop support of Ubuntu 16.04

* Fixes a bug in the OAS3 definition for endpoint "/accounts/{pubkey}/next-nonce" in a combination with `int-as-string` flag

With regards of HyperChains it:

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

* Introduces a new consensus model for HyperChains. This is where we will keep
  HyperChains specifics. Some parts of the consensus are stubbed as they are
  still in work in progress state. It can be activated by setting the config
  value for the appropriate height to have a `name` being `hyper_chain`. This
  is added to the handful of existing different consensuses to chose from,
  most interesting of which are:

    * `pow_cuckoo` for PoW as it is currently on mainnet

    * `smart_contract` for a pure PoS consensus, driven by a smart contract

* AE parent connector is being refactored, now its settings are exposed to the
  config - parent chain nodes and a fetch interval in milliseconds. The type
  of the parent chain is also required, currently only AE2AE is available so
  the parent type is 'AE'

* Parent chain cache - we cache parent chain headers in order to speed up the
   child's syncing. It has the following configs:

   * a number of confirmations before a parent chain block is considered to be
     finalized. This impacts consensus

   * parent chain start height - the height of the parent chain that
     corresponds to the child's genesis block. This is also impacting
     consensus

* The consensus contract is being reworked - now the responsibilites for DPoS
  and leader election are split into two contracts. This makes it easier to
  reuse DPoS while changing the election and vice versa

* Parent chain block hash is being propagated to the election smart contract.
  It is being used during the election as a source of entropy.

* Genesis seed files are generated and not being included in releases.

* Seed files are loaded according to the network id. This makes it easier to
  setup different HyperChains.




Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).

