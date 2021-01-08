# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.6.3) is an emergency Lima release.

There had been a 51% attack on the network. This release is an expression of
the ecosystem's desire to move from the attacker's fork to the community one.
If you prefer staying on the former, please use release 5.6.0

This release requires the node to be running without the GC being enabled. If
your node has it enabled - the DB is not suitable and your node will likely
crash on start. In this case please either sync from the genesis block or
download a [DB backup](https://downloads.aeternity.io/#backups).
Once on the community fork - you can enable the GC.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).

