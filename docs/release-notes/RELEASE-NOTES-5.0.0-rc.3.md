# About this release

Third **Lima** release candidate.

Changelog:

* Added infrastructure for deploying contracts (with fresh tokens) during hard forks from Lima release.
  This will be used to finalize the token migration.
* Added the migrated tokens in the Phase 3 of the token migration.
* Adds one (1) percent of the total initial (ERC20) token supply to an account
  according to Pool D in https://hackmd.aepps.com/s/H1qF1w1j7#
* Set Lima testnet hardfork height to 154300 (16th Oct 2019, 09:00 UTC)
* Set Lima mainnet to 161150 (30th Oct 2019, 13:00 UTC)
* Aligns AEVM store gas pricing with FATE.
* ContractCallTX where the called contract uses FATE has a lower base cost (60% cheaper than a call to an AEVM contract).
* State Channels: The support for state cache encryption (introduced in v5.0.0-rc.2)
  has been disabled. An improved approach is being developed which greatly improves API
  usability. This feature is expected to be released with v5.1.0.

The node is able to start from a database produced by v4.* releases. For the rest, this release is not backward compatible with v4.* releases.

Please join the mainnet by following the instructions in the documentation below, and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
