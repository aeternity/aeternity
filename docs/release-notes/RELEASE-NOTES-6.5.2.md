# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v6.5.2) is a maintenance Iris release.

It:

* Fixed a bug in the transaction pool: an invalid name transaction could cause
  it to crash
* Added support for building and running the system with Erlang/OTP 25
* This change removes the experimental leveled database backend. Anyone currently using this experimental feature
  will need to delete their local database and re-sync using the default and supported RocksDB database.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).

