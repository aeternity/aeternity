# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v6.8.0) is a maintenance Iris and HyperChains release.

It:
* While it should be possible to override config values using OS environment variables,
  this didn't work for e.g. `http:endpoints:dry-run`, since the name contains a hyphen.
  Generally, it didn't work for any children of `http:endpoints`, since that schema subtree
  was improperly structured. This has been fixed, and for any config variable whose name contains
  a hyphen, the corresponding OS environment variable should replace any hyphen with an underscore:
  `AE__HTTP__ENDPOINTS__DRY_RUN=true`.
* The config variable `http:endpoints:node_operator` has been changed to `node-operator`, since
  this is what was expected by the application code. Due to the structural error above, it was
  possible to specify `node-operator` even before, and this is the only thing that would have worked.
  With the corrected structure and name change, such a setting will also be properly validated.
* Changes the key that defines the consensus in the config: it used to be
  called `name` and now it is renamed to `type`. **:warning: This is a backwards
  incompatible change :warning:**
* Error reporting due to invalid configuration data during startup has been cleaned up significantly
* Validation errors due to invalid data in OS environment config variables were ignored. Now, startup is terminated.
* Fixes a bug in the node settings: `cors` configs were not processed
  correctly.
* Changes the default directory to store Stratum keys. It used to be in the
  lib's priv directory which is rather unconvinient to use. Now the keys are
  configured according to the root of the project (suggestion is to keep them
  in `data/stratum/keys` but this is up to the user to decide). **:warning: This is a backwards
  incompatible change :warning:**

With regards of HyperChains it:
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
* When staking, return shares bought, stake, and execution height.
* When unstaking, return shares bought, stake, and execution height.
* Allow validator to become online at contract creation height
* Staker should not be able to be left with less than the minimum stake
* Removes the default consensus for networks with `network_id` starting with a
  `hc_` prefix
* Fixes a bug in commitments production - there was a race condition that
  could lead to deadlocks
* Allows running a HC not only on CERES protocol but also on IRIS


Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).

