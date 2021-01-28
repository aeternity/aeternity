# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.7.0) is a maintenance Lima release.

It:
* Introduces an external HTTP endpoint(/v2/status/chain-ends) which reports keyhashes for which no successor is present in the database. Useful for retrieving orphan blocks and tracking chain reorganizations.

* Introduces configurable fork resistance (`sync:sync_allowed_height_from_top`), which lets
  a node reject long forks injected via the sync protocol. Documented in docs/fork-resistance.md

* HTTP endpoint `names/<name>` now also returns a field `owner` containing the account owning the name.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).

