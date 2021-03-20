# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.10.0) is a maintenance Lima release.

It:

* An API method has been added for state channels that allows the client to tell the fsm
  to assume that minimum depth has been achieved for a certain tx hash. If used by both
  clients, it can make a state channel immediately available after creation (and after
  deposits/withdrawals). The minimum_depth confirmation message is reported as it arrives
  later.

* Improves the gossiping of orphaned transactions. Now, as soon a transaction
  ends up on a micro fork, the node broadcasts it to one's peers, even if some
  of them are still syncing.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
 

