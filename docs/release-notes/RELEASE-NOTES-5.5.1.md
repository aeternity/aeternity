# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.5.1) is a maintenance Lima release.

* Adds support in the State Channel FSM to detect and properly act upon
  unexpected channel_force_progress_tx on-chain
* Fix check of contracts which prevented new nodes from synching from scratch
* When a State Channel responder acceptor times out (accept_timeout), it checks to see whether it should give up or keep trying. This logic was faulty and has now been fixed.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
